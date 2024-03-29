/*
 * stub.c -- build stubs for an IDL interface
 * Copyright 2010  Kalle A. Sandström <ksandstr@iki.fi>
 *
 * This file is part of µiX.
 *
 * µiX is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * µiX is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with µiX.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <stdlib.h>
#include <glib.h>
#include <errno.h>
#include <libIDL/IDL.h>
#include <llvm-c/Core.h>

#include "defs.h"
#include "llvmutil.h"
#include "l4x2.h"


static LLVMTypeRef stub_fn_type(
	struct llvm_ctx *ctx,
	struct method_info *inf,
	int timeout_kind)
{
	struct message_info *reply, dummy;
	if(inf->num_reply_msgs > 0) reply = inf->replies[0];
	else {
		/* to shut up the Clang static analyser */
		dummy = (struct message_info){ };
		reply = &dummy;
	}

	const bool has_context = has_complex_exn(inf->node);
	int num_params = IDL_list_length(IDL_OP_DCL(inf->node).parameter_dcls),
		max_args = num_params * 2 + 7;
	T arg_types[max_args];
	for(int i=0; i<max_args; i++) arg_types[i] = NULL;
	int max_arg = -1, arg_offset = 0;

	/* memory map acceptor */
	if(has_mapped_param(inf->node)) {
		arg_types[++max_arg] = ctx->wordt;	/* L4_Fpage_t, range */
		arg_offset++;
	}

	/* IPC destination parameter */
	if(!has_pager_target(inf->node)) {
		arg_types[++max_arg] = ctx->wordt;	/* L4_ThreadId_t, peer */
		arg_offset++;
	}

	/* return value pointer */
	if(reply != NULL && reply->ret_type != NULL) {
		assert(IDL_NODE_TYPE(reply->ret_type) != IDLN_TYPE_SEQUENCE);
		arg_types[++max_arg] = LLVMPointerType(llvm_rigid_type(ctx,
			reply->ret_type), 0);
		arg_offset++;
	}

	/* parameters */
	T *at_base = &arg_types[arg_offset];
	IDL_LIST_FOREACH(cur, IDL_OP_DCL(inf->node).parameter_dcls) {
		IDL_tree pdecl = IDL_LIST(cur).data,
			type = get_type_spec(IDL_PARAM_DCL(pdecl).param_type_spec);
		enum IDL_param_attr attr = IDL_PARAM_DCL(pdecl).attr;
		int nargs = 1;
		struct msg_param *p;
		bool out;
		if(attr == IDL_PARAM_IN) {
			p = find_pdecl(inf->request->params, pdecl);
			out = false;
		} else {
			assert(!inf->oneway);	/* ensured by analysis. */
			assert(inf->num_reply_msgs > 0);
			p = find_pdecl(reply->params, pdecl);
			out = true;
		}
		assert(p != NULL);

		if(is_value_type(type)) {
			T typ = llvm_value_type(ctx, type);
			if(out) typ = LLVMPointerType(typ, 0);
			at_base[p->arg_ix] = typ;
		} else if(IDL_NODE_TYPE(type) == IDLN_TYPE_SEQUENCE) {
			at_base[p->arg_ix + 0] = LLVMPointerType(
				llvm_rigid_type(ctx, SEQ_SUBTYPE(type)), 0);
			T cttyp = ctx->i32t;
			if(out) cttyp = LLVMPointerType(cttyp, 0);
			at_base[p->arg_ix + 1] = cttyp;
			nargs = 2;
		} else if(IDL_NODE_TYPE(type) == IDLN_TYPE_STRING) {
			at_base[p->arg_ix] = LLVMPointerType(
				LLVMInt8TypeInContext(ctx->ctx), 0);
		} else if(IDL_NODE_TYPE(type) == IDLN_TYPE_WIDE_STRING) {
			/* TODO: use a wchar_t from the ABI */
			at_base[p->arg_ix] = LLVMPointerType(ctx->i32t, 0);
		} else if(IDL_NODE_TYPE(type) == IDLN_TYPE_ARRAY) {
			at_base[p->arg_ix] = LLVMPointerType(
				llvm_rigid_type(ctx, get_array_type(type)), 0);
		} else {
			at_base[p->arg_ix] = LLVMPointerType(
				llvm_rigid_type(ctx, type), 0);
		}

		max_arg = MAX(max_arg, p->arg_ix + nargs - 1 + arg_offset);
	}

	if(has_context) {
		IDL_tree iface = IDL_get_parent_node(inf->node, IDLN_INTERFACE, NULL);
		assert(iface != NULL);
		/* (union types aren't explicitly supported by LLVM any more.) */
		arg_types[++max_arg] = LLVMPointerType(
			LLVMVoidTypeInContext(ctx->ctx), 0);
	}

	T l4_time_t = LLVMInt16TypeInContext(ctx->ctx);
	if(timeout_kind & TIMEOUT_SEND) arg_types[++max_arg] = l4_time_t;
	if(timeout_kind & TIMEOUT_RECV) arg_types[++max_arg] = l4_time_t;

	if(max_arg < 0) {
		return LLVMFunctionType(ctx->i32t, NULL, 0, 0);
	} else {
		return LLVMFunctionType(ctx->i32t, arg_types, max_arg + 1, 0);
	}
}


/* NOTE: "ix" is given as "exception number, plus one"; the tag-only field is
 * index zero.
 */
static LLVMValueRef get_ex_from_ctx(
	struct llvm_ctx *ctx,
	IDL_tree iface,
	LLVMValueRef ctxptr,
	int ix)
{
	assert(iface != NULL);
	assert(ix > 0);

	T ext = context_type_of_iface(ctx, iface, ix - 1);
	assert(ext != NULL);
	return LLVMBuildPointerCast(ctx->builder, ctxptr,
		LLVMPointerType(ext, 0), "ex.ptr");
}


/* returns the acceptor. */
static LLVMValueRef build_stub_receive_strings(
	struct llvm_ctx *ctx,
	struct method_info *inf,
	struct stritem_info *stritems,
	LLVMValueRef *args,
	LLVMValueRef ctxptr)
{
	assert(stritems != NULL);
	assert(stritems[0].length >= 0);

	/* (historical name. should be str_param_lists or something.) */
	GList *long_lists[inf->num_reply_msgs];
	for(int i=0; i < inf->num_reply_msgs; i++) {
		long_lists[i] = inf->replies[i]->string;
	}

	IDL_tree iface = IDL_get_parent_node(inf->node, IDLN_INTERFACE, NULL);
	int brpos = 1;
	for(int i=0; stritems[i].length >= 0; i++) {
		struct stritem_info *si = &stritems[i];

		/* find the first parameter that can fit the whole string item.
		 *
		 * NOTE: we should rather receive the first N bytes in the reply's
		 * buffer, and the other M-N bytes in the longest item where M > N.
		 * this would require a bit more thought and testing, and can be
		 * written as an optimization later on, so...
		 */
		si->memptr = NULL;
		for(int j=0; j < inf->num_reply_msgs && si->memptr == NULL; j++) {
			if(long_lists[j] == NULL) continue;
			struct msg_param *p = long_lists[j]->data;

			int size = max_size(p->type);
			if(size >= si->length) {
				si->reply_pos = j;
				si->param = p;
				if(IS_EXN_MSG(inf->replies[j])) {
					V indexes[2] = { CONST_INT(0), CONST_INT(0) },
						exptr = get_ex_from_ctx(ctx, iface,
							ctxptr, inf->replies[j]->ctx_index);
					si->memptr = LLVMBuildGEP(ctx->builder,
						LLVMBuildStructGEP(ctx->builder, exptr,
							p->arg_ix + 1, "ex.lp.mem.ptr"),
						indexes, 2, "ex.lp.1st.ptr");
					si->lenptr = NULL;
				} else {
					/* it's in an ordinary reply */
					si->memptr = args[p->arg_ix];
					/* FIXME: specify in msg_param whether these have a lenptr
					 * or not. this hack is ad-hoc and terrible.
					 */
					if(IDL_NODE_TYPE(p->type) == IDLN_TYPE_STRING) {
						si->lenptr = NULL;
					} else {
						si->lenptr = args[p->arg_ix + 1];
					}
				}
			}
		}
		assert(si->memptr != NULL);
		for(int j=0; j < inf->num_reply_msgs; j++) {
			/* (pretty sure that g_list_next(NULL) == NULL, but let's
			 * be safe)
			 */
			if(long_lists[j] != NULL) {
				long_lists[j] = g_list_next(long_lists[j]);
			}
		}

		V si_words[2];
		V lenword = si->lenptr == NULL ? CONST_WORD(si->length)
			: LLVMBuildLoad(ctx->builder, si->lenptr, "lenword.val");
		if(IDL_NODE_TYPE(si->param->type) == IDLN_TYPE_SEQUENCE) {
			int bs = size_in_bits(SEQ_SUBTYPE(si->param->type));
			lenword = LLVMBuildMul(ctx->builder, lenword, CONST_UINT(bs / 8), "lenword.mult");
		}
		build_simple_string_item(ctx, si_words, si->memptr, lenword, NULL);
		if(si[1].length >= 0) {
			/* we're not the last; set the C bit. */
			si_words[0] = LLVMBuildOr(ctx->builder, CONST_WORD(1),
				si_words[0], "si.desc.with.C");
		}
		LLVMBuildStore(ctx->builder, si_words[0],
			UTCB_ADDR_VAL(ctx, CONST_INT(BR_OFFSET(brpos++)), "si.br0.addr"));
		LLVMBuildStore(ctx->builder, si_words[1],
			UTCB_ADDR_VAL(ctx, CONST_INT(BR_OFFSET(brpos++)), "si.br1.addr"));
	}

	return brpos > 1 ? CONST_WORD(1) : CONST_WORD(0);
}


static void build_stub_reply_decode(
	struct llvm_ctx *ctx,
	LLVMValueRef *ret_args,
	LLVMValueRef *args,
	const struct method_info *inf,
	LLVMValueRef ctxptr,
	const struct stritem_info *stritems)
{
	V label = build_label_from_tag(ctx, ctx->tag);
	/* TODO: get this from the target's <errno.h>; it's the value that is
	 * returned by the stub when the return message has a label that this
	 * cannot recognize.
	 */
	V ex_tag_ptr = NULL;
	if(ctxptr != NULL) {
		ex_tag_ptr = LLVMBuildPointerCast(ctx->builder, ctxptr,
			LLVMPointerType(ctx->wordt, 0), "ex.tag.ptr");
	}
	branch_set_phi(ctx, ctx->retval_phi, CONST_INT(-EINVAL));
	V label_sw = LLVMBuildSwitch(ctx->builder, label, ctx->exit_bb, 3);

	if(find_exn(inf->node, &is_negs_exn) != NULL) {
		/* the dreaded MSG_ERROR (the simple exception) */
		assert(!inf->oneway);
		BB msgerr_bb = add_sibling_block(ctx, "msgerr.match");
		LLVMAddCase(label_sw, CONST_WORD(1), msgerr_bb);
		LLVMPositionBuilderAtEnd(ctx->builder, msgerr_bb);
		branch_set_phi(ctx, ctx->retval_phi,
			LLVMBuildNeg(ctx->builder, ctx->mr1, "msgerr.val.neg"));
		LLVMBuildBr(ctx->builder, ctx->exit_bb);
	}

	/* complex exceptions */
	if(ctxptr != NULL) {
		BB exn_bb = add_sibling_block(ctx, "catch.exn");
		LLVMAddCase(label_sw, CONST_WORD(2), exn_bb);
		LLVMPositionBuilderAtEnd(ctx->builder, exn_bb);
		LLVMBuildStore(ctx->builder, ctx->mr1, ex_tag_ptr);
		/* FIXME: come up with the correct return value for "unrecognized
		 * exception status from server"!
		 */
		branch_set_phi(ctx, ctx->retval_phi, CONST_WORD(666));
		V sw = LLVMBuildSwitch(ctx->builder, ctx->mr1, ctx->exit_bb,
			inf->num_reply_msgs - 1);
		IDL_tree iface = IDL_get_parent_node(inf->node,
			IDLN_INTERFACE, NULL);
		for(int i=1; i < inf->num_reply_msgs; i++) {
			const struct message_info *msg = inf->replies[i];
			if(is_negs_exn(msg->node) || is_noreply_exn(msg->node)) {
				continue;
			}
			BB decode = add_sibling_block(ctx, "catch.x%x", msg->sublabel);
			LLVMAddCase(sw, CONST_WORD(msg->sublabel), decode);
			LLVMPositionBuilderAtEnd(ctx->builder, decode);
			build_decode_exception(ctx,
				get_ex_from_ctx(ctx, iface, ctxptr, msg->ctx_index),
				msg, i, stritems);
			/* on exception, a stub returns 0 because it's an IPC success
			 * regardless. the caller gets to check ctxptr->tag.
			 */
			branch_set_phi(ctx, ctx->retval_phi, CONST_WORD(0));
			LLVMBuildBr(ctx->builder, ctx->exit_bb);
		}
	}

	/* regular things */
	BB noexn_success_bb = add_sibling_block(ctx, "noexn.success");
	LLVMAddCase(label_sw, CONST_WORD(0), noexn_success_bb);
	LLVMPositionBuilderAtEnd(ctx->builder, noexn_success_bb);
	const struct message_info *reply =
		inf->num_reply_msgs > 0 ? inf->replies[0] : NULL;
	if(reply != NULL) {
		if(ctxptr != NULL) {
			/* in the no-exception case, tag will be 0. */
			LLVMBuildStore(ctx->builder, CONST_WORD(0), ex_tag_ptr);
		}
		build_msg_decoder(ctx, ret_args, args, reply, 0, stritems, true);
	}
	branch_set_phi(ctx, ctx->retval_phi, CONST_WORD(0));
	LLVMBuildBr(ctx->builder, ctx->exit_bb);
}


static void build_stub_msgerr(struct llvm_ctx *ctx)
{
	/* this mimics a L4.X2 error status, as a code 0 in receive phase (where
	 * inline sequence decode happens), and returns the negative error value
	 * shifted up by 4 bits.
	 */
	V err = LLVMBuildShl(ctx->builder, ctx->errval_phi, CONST_WORD(4),
			"err.shifted"),
		code = LLVMBuildOr(ctx->builder, err, CONST_WORD(1), "err.code");
	branch_set_phi(ctx, ctx->retval_phi, code);
	LLVMBuildBr(ctx->builder, ctx->exit_bb);
}


static void build_ipc_stub(
	struct llvm_ctx *ctx,
	struct method_info *inf,
	int timeout_kind)
{
	char *stubpfx = get_stub_prefix(inf->node),
		*opname = decapsify(METHOD_NAME(inf->node)),
		*stubname = g_strconcat(
			stubpfx == NULL ? "" : stubpfx,
			stubpfx == NULL ? "" : "_",
			opname,
			timeout_kind != 0 ? "_timeout" : "",
			NULL);
	g_free(stubpfx);
	g_free(opname);

	struct message_info *reply = NULL;
	if(inf->num_reply_msgs > 0) reply = inf->replies[0];
	T fn_type = stub_fn_type(ctx, inf, timeout_kind);
	V fn = LLVMAddFunction(ctx->module, stubname, fn_type);
	g_free(stubname);

	int num_args = LLVMCountParams(fn), arg_offset = 0;
	assert(num_args >= 1);
	V *args = g_new(V, num_args);
	LLVMGetParams(fn, args);

	V accept_range = NULL;
	if(has_mapped_param(inf->node)) accept_range = args[arg_offset++];

	V ipc_dest = NULL;
	if(!has_pager_target(inf->node)) ipc_dest = args[arg_offset++];

	V *ret_args = NULL;
	if(reply != NULL && reply->ret_type != NULL) {
		ret_args = &args[arg_offset++];
	}

	/* prelude. */
	begin_function(ctx, fn);
	ctx->build_msgerr_bb = &build_stub_msgerr;
	if(ipc_dest == NULL) {
		assert(has_pager_target(inf->node));
		/* load the pager TCR. */
		ipc_dest = LLVMBuildLoad(ctx->builder,
			UTCB_ADDR_VAL(ctx, CONST_INT(TCR_PAGER), "pager.addr"),
			"pager.tid");
	}

	/* function exit. */
	ctx->exit_bb = add_sibling_block(ctx, "exit");
	LLVMPositionBuilderAtEnd(ctx->builder, ctx->exit_bb);
	ctx->retval_phi = LLVMBuildPhi(ctx->builder, ctx->i32t, "retval.phi");

	/* send-half. */
	BB start_bb = add_sibling_block(ctx, "stub.start");
	LLVMPositionBuilderAtEnd(ctx->builder, start_bb);
	struct stritem_info *stritems = stub_stritems(inf);
	V tag = build_msg_encoder(ctx, inf->request, NULL, &args[arg_offset],
		false);

	/* 0 is L4_Time_t for Never, and goes in both timeout fields */
	V timeouts = CONST_WORD(0);
	int back_ix = num_args - 1;
	if(timeout_kind != 0) {
		if(timeout_kind & TIMEOUT_RECV) {
			timeouts = LLVMBuildZExtOrBitCast(ctx->builder,
				args[back_ix--], ctx->wordt, "to.w.recv");
		}
		if(timeout_kind & TIMEOUT_SEND) {
			timeouts = LLVMBuildOr(ctx->builder, timeouts,
				LLVMBuildShl(ctx->builder,
					LLVMBuildZExtOrBitCast(ctx->builder, args[back_ix--],
						ctx->wordt, "to.send"),
					CONST_INT(16), "to.send.shifted"),
				"to.w.send");
		}
	}

	V ctxptr = NULL;
	if(has_complex_exn(inf->node)) ctxptr = args[back_ix--];

	V acceptor = CONST_WORD(0);
	if(stritems != NULL && stritems[0].length >= 0) {
		acceptor = build_stub_receive_strings(ctx, inf, stritems,
			&args[arg_offset], ctxptr);
	}
	if(accept_range != NULL) {
		acceptor = LLVMBuildOr(ctx->builder, acceptor,
			LLVMBuildAnd(ctx->builder, accept_range,
				CONST_WORD(~0xfULL), "accept.range.masked"),
			"acc.with.maps");
	}

	if(inf->oneway) {
		/* L4_nilthread == word(0) */
		ctx->tag = build_l4_ipc_call(ctx, ipc_dest, timeouts,
			CONST_WORD(0), tag, NULL, NULL, NULL);
	} else {
		/* load the acceptor. */
		LLVMBuildStore(ctx->builder, acceptor,
			UTCB_ADDR_VAL(ctx, CONST_INT(BR_OFFSET(0)), "acceptor.addr"));
		ctx->tag = build_l4_ipc_call(ctx, ipc_dest, timeouts,
			ipc_dest, tag, NULL, &ctx->mr1, &ctx->mr2);

		int min_u = 66;
		for(int i=0; i < inf->num_reply_msgs; i++) {
			const struct message_info *msg = inf->replies[i];
			if(IDL_NODE_TYPE(msg->node) == IDLN_EXCEPT_DCL
				&& (is_negs_exn(msg->node) || is_noreply_exn(msg->node)))
			{
				continue;
			}

			int mu = msg_min_u(inf->replies[i]);
			min_u = MIN(min_u, mu);
		}
		min_u--;

		build_store_received_regs(ctx, min_u, ctx->mr1, ctx->mr2);
	}

	/* check for IPC errors. */
	V errcode = LLVMBuildLoad(ctx->builder,
		UTCB_ADDR_VAL(ctx, CONST_INT(TCR_ERROR_CODE), "ec.addr"),
		"ec.value");
	V err_bit = LLVMBuildAnd(ctx->builder, ctx->tag,
			CONST_WORD(1 << 15), "ipc.tag.e"),
		err_cond = LLVMBuildICmp(ctx->builder, LLVMIntEQ, err_bit,
			CONST_WORD(0), "ipc.tag.e.cond");
	if(inf->oneway) {
		V rv = LLVMBuildSelect(ctx->builder, err_cond, CONST_WORD(0),
			errcode, "ec.or.success");
		branch_set_phi(ctx, ctx->retval_phi, rv);
		LLVMBuildBr(ctx->builder, ctx->exit_bb);
	} else {
		BB err_bb = add_sibling_block(ctx, "ipcerror"),
			noerr_bb = add_sibling_block(ctx, "ipcok");
		LLVMBuildCondBr(ctx->builder, err_cond, noerr_bb, err_bb);

		/* on IPC error just pass the msgerr. */
		LLVMPositionBuilderAtEnd(ctx->builder, err_bb);
		branch_set_phi(ctx, ctx->retval_phi, errcode);
		LLVMBuildBr(ctx->builder, ctx->exit_bb);

		/* on success, decode replies. */
		LLVMPositionBuilderAtEnd(ctx->builder, noerr_bb);
		build_stub_reply_decode(ctx, ret_args, &args[arg_offset],
			inf, ctxptr, stritems);
	}

	/* cleanup and exit. */
	end_function(ctx, start_bb);
	LLVMPositionBuilderAtEnd(ctx->builder, ctx->exit_bb);
	LLVMBuildRet(ctx->builder, ctx->retval_phi);

	LLVMDisposeBuilder(ctx->builder);
	ctx->builder = NULL;

	g_free(args);
	g_free(stritems);
}


LLVMValueRef build_stubs_for_iface(
	struct llvm_ctx *ctx,
	const struct iface_info *iface)
{
	GLIST_FOREACH(cur, iface->ops) {
		struct method_info *inf = cur->data;

		IDL_tree parent = IDL_get_parent_node(inf->node,
			IDLN_INTERFACE, NULL);
		assert(parent != NULL);
		if(parent != iface->node) {
			/* don't generate stubs for inherited interfaces. the common
			 * example is L4X2::FaultHandler.
			 */
			continue;
		}

		/* (TODO: don't build the timeoutless variant if given an
		 * attribute not to do so. see header.c for details.)
		 */
		build_ipc_stub(ctx, inf, 0);
		int tok = op_timeout_kind(inf->node);
		if(tok != 0) build_ipc_stub(ctx, inf, tok);
	}
	return NULL;
}
