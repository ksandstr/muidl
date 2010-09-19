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
#include <libIDL/IDL.h>
#include <llvm-c/Core.h>

#include "muidl.h"
#include "llvmutil.h"
#include "l4x2.h"


static LLVMTypeRef stub_fn_type(
	struct llvm_ctx *ctx,
	struct method_info *inf,
	int timeout_kind)
{
	struct message_info *reply = NULL;
	if(inf->num_reply_msgs > 0) reply = inf->replies[0];

	const bool has_context = IDL_OP_DCL(inf->node).raises_expr != NULL;
	int num_params = IDL_list_length(IDL_OP_DCL(inf->node).parameter_dcls),
		num_args = num_params * 2 + 2 + 1 + 2 + (has_context ? 1 : 0);
	T arg_types[num_args];
	for(int i=0; i<num_args; i++) arg_types[i] = NULL;
	int max_arg = -1, arg_offset = 0;

	/* IPC destination parameter */
	if(!has_pager_target(ctx->ns, inf->node)) {
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
	for(IDL_tree cur = IDL_OP_DCL(inf->node).parameter_dcls;
		cur != NULL;
		cur = IDL_LIST(cur).next)
	{
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
			p = find_pdecl(reply->params, pdecl);
			out = true;
		}
		assert(p != NULL);

		if(is_value_type(type)) {
			T typ = llvm_value_type(ctx, type);
			if(out) typ = LLVMPointerType(typ, 0);
			at_base[p->arg_ix] = typ;
		} else if(IDL_NODE_TYPE(type) == IDLN_TYPE_SEQUENCE) {
			IDL_tree subtype = get_type_spec(
				IDL_TYPE_SEQUENCE(type).simple_type_spec);
			at_base[p->arg_ix + 0] = LLVMPointerType(
				llvm_rigid_type(ctx, subtype), 0);
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
		} else if(is_rigid_type(ctx->ns, type)) {
			at_base[p->arg_ix] = LLVMPointerType(
				llvm_rigid_type(ctx, type), 0);
		}

		max_arg = MAX(max_arg, p->arg_ix + nargs - 1 + arg_offset);
	}

	if(has_context) {
		IDL_tree iface = IDL_get_parent_node(inf->node, IDLN_INTERFACE, NULL);
		assert(iface != NULL);
		arg_types[++max_arg] = LLVMPointerType(
			context_type_of_iface(ctx, iface), 0);
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


/* returns the acceptor. */
static LLVMValueRef build_stub_receive_strings(
	struct llvm_ctx *ctx,
	struct method_info *inf,
	struct stritem_info *stritems,
	LLVMValueRef *args)
{
	assert(stritems != NULL);
	assert(stritems[0].length >= 0);

	GList *long_lists[inf->num_reply_msgs];
	for(int i=0; i < inf->num_reply_msgs; i++) {
		long_lists[i] = g_list_first(inf->replies[i]->_long);
	}

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
		for(int j=0; j < inf->num_reply_msgs; j++) {
			if(long_lists[j] == NULL) continue;
			struct msg_param *p = long_lists[j]->data;

			int size = max_size(p->X._long.type);
			printf("looking at `%s' (size %d) to fill stritem[%d] of len %d\n",
				p->name, size, i, si->length);
			if(size >= si->length) {
				si->reply_pos = j;
				si->param = p;
				if(IS_EXN_MSG(inf->replies[j])) {
					si->memptr = NULL;		/* TODO */
					assert(false);
				} else {
					/* it's in an ordinary reply */
					si->memptr = args[p->arg_ix];
				}
				break;
			}
		}
		assert(si->memptr != NULL);
		for(int j=0; j < inf->num_reply_msgs; j++) {
			long_lists[j] = g_list_next(long_lists[j]);
		}

		V si_words[2];
		build_simple_string_item(ctx, si_words, si->memptr,
			CONST_WORD(si->length), NULL);
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

	V ipc_dest = NULL;
	if(!has_pager_target(ctx->ns, inf->node)) {
		ipc_dest = args[arg_offset++];
	}

	V *ret_args = NULL;
	if(reply != NULL && reply->ret_type != NULL) {
		ret_args = &args[arg_offset++];
	}

	/* prelude. */
	const bool oneway = IDL_OP_DCL(inf->node).f_oneway != 0;
	begin_function(ctx, fn);
	ctx->build_msgerr_bb = &build_stub_msgerr;
	if(ipc_dest == NULL) {
		assert(has_pager_target(ctx->ns, inf->node));
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
	V acceptor = CONST_WORD(0);
	if(stritems != NULL && stritems[0].length >= 0) {
		acceptor = build_stub_receive_strings(ctx, inf, stritems,
			&args[arg_offset]);
	}
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
	if(IDL_OP_DCL(inf->node).raises_expr != NULL) ctxptr = args[back_ix--];

	if(oneway) {
		/* L4_nilthread == word(0) */
		ctx->tag = build_l4_ipc_call(ctx, ipc_dest, timeouts,
			CONST_WORD(0), tag, NULL, NULL, NULL);
	} else {
		/* load the acceptor. */
		LLVMBuildStore(ctx->builder, acceptor,
			UTCB_ADDR_VAL(ctx, CONST_INT(BR_OFFSET(0)), "acceptor.addr"));
		/* FIXME: generate "stritems" from params, load buffer registers */
		ctx->tag = build_l4_ipc_call(ctx, ipc_dest, timeouts,
			ipc_dest, tag, NULL, &ctx->mr1, &ctx->mr2);
	}
	/* check for IPC errors. */
	BB err_bb = add_sibling_block(ctx, "ipcerror"),
		noerr_bb = add_sibling_block(ctx, "ipcok");
	V err_bit = LLVMBuildAnd(ctx->builder, ctx->tag,
		CONST_WORD(1 << 15), "ipc.tag.e");
	LLVMBuildCondBr(ctx->builder, LLVMBuildICmp(ctx->builder, LLVMIntEQ,
		CONST_WORD(0), err_bit, "ipc.error.p"), noerr_bb, err_bb);

	/* IPC error handler. */
	LLVMPositionBuilderAtEnd(ctx->builder, err_bb);
	V errcode = LLVMBuildLoad(ctx->builder,
		UTCB_ADDR_VAL(ctx, CONST_INT(TCR_ERROR_CODE), "ec.addr"),
		"ec.value");
	branch_set_phi(ctx, ctx->retval_phi, errcode);
	LLVMBuildBr(ctx->builder, ctx->exit_bb);

	/* IPC success path. */
	LLVMPositionBuilderAtEnd(ctx->builder, noerr_bb);
	V label = build_label_from_tag(ctx, ctx->tag);
	if(find_exn(inf->node, &is_negs_exn) != NULL) {
		/* the dreaded MSG_ERROR (the simple exception) */
		assert(!oneway);
		V matches = LLVMBuildICmp(ctx->builder, LLVMIntEQ,
				label, CONST_WORD(1), "negexn.matches");
		BB msgerr_bb = add_sibling_block(ctx, "msgerr.match"),
			no_msgerr_bb = add_sibling_block(ctx, "no.msgerr");
		LLVMBuildCondBr(ctx->builder, matches, msgerr_bb, no_msgerr_bb);

		LLVMPositionBuilderAtEnd(ctx->builder, msgerr_bb);
		branch_set_phi(ctx, ctx->retval_phi,
			LLVMBuildNeg(ctx->builder, ctx->mr1, "msgerr.val.neg"));
		LLVMBuildBr(ctx->builder, ctx->exit_bb);

		LLVMPositionBuilderAtEnd(ctx->builder, no_msgerr_bb);
	}

	/* complex exceptions */
	if(ctxptr != NULL) {
		BB no_exn_bb = add_sibling_block(ctx, "no.exn"),
			exn_bb = add_sibling_block(ctx, "catch.exn");
		V catch_cond = LLVMBuildICmp(ctx->builder, LLVMIntEQ, label,
				CONST_WORD(2), "label.exn.p"),
			ex_tag_ptr = LLVMBuildStructGEP(ctx->builder, ctxptr,
				0, "ex.tag.ptr");
		LLVMBuildCondBr(ctx->builder, catch_cond, exn_bb, no_exn_bb);

		LLVMPositionBuilderAtEnd(ctx->builder, exn_bb);
		LLVMBuildStore(ctx->builder, ctx->mr1, ex_tag_ptr);
		/* FIXME: come up with the correct return value for "unrecognized
		 * exception status from server"!
		 */
		branch_set_phi(ctx, ctx->retval_phi, CONST_WORD(666));
		V sw = LLVMBuildSwitch(ctx->builder, ctx->mr1, ctx->exit_bb,
			inf->num_reply_msgs - 1);
		for(int i=1; i < inf->num_reply_msgs; i++) {
			const struct message_info *msg = inf->replies[i];
			if(is_negs_exn(msg->node) || is_noreply_exn(msg->node)) {
				continue;
			}
			BB decode = add_sibling_block(ctx, "catch.x%x", msg->sublabel);
			LLVMAddCase(sw, CONST_WORD(msg->sublabel), decode);
			LLVMPositionBuilderAtEnd(ctx->builder, decode);
#if 1
			/* LLVM has some kind of brain damage wrt GEP and union pointers */
			T cp_type = LLVMGetElementType(LLVMTypeOf(ctxptr)),
				u_types[LLVMCountUnionElementTypes(cp_type)];
			LLVMGetUnionElementTypes(cp_type, u_types);
			V exptr = LLVMBuildPointerCast(ctx->builder, ctxptr,
				LLVMPointerType(u_types[msg->ctx_index], 0), "ex.ptr");
#else
			V exptr = LLVMBuildStructGEP(ctx->builder, ctxptr,
				msg->ctx_index, "ex.ptr");
#endif
			build_decode_exception(ctx, exptr, msg);
			/* on exception, a stub returns 0 because it's an IPC success
			 * regardless. the caller gets to check ctxptr->tag.
			 */
			branch_set_phi(ctx, ctx->retval_phi, CONST_WORD(0));
			LLVMBuildBr(ctx->builder, ctx->exit_bb);
		}

		LLVMPositionBuilderAtEnd(ctx->builder, no_exn_bb);
		/* in the no-exception case, tag will be 0. */
		LLVMBuildStore(ctx->builder, CONST_WORD(0), ex_tag_ptr);
	}

	/* regular things */
	if(reply != NULL) {
		build_msg_decoder(ctx, ret_args, &args[arg_offset], reply,
			stritems, true);
	}
	branch_set_phi(ctx, ctx->retval_phi, CONST_WORD(0));
	LLVMBuildBr(ctx->builder, ctx->exit_bb);

	/* cleanup and exit. */
	end_function(ctx, start_bb);
	LLVMPositionBuilderAtEnd(ctx->builder, ctx->exit_bb);
	LLVMBuildRet(ctx->builder, ctx->retval_phi);

	LLVMDisposeBuilder(ctx->builder);
	ctx->builder = NULL;

	g_free(args);
}


gboolean iter_build_stubs(IDL_tree_func_data *tf, void *ud)
{
	struct llvm_ctx *ctx = ud;
	switch(IDL_NODE_TYPE(tf->tree)) {
		case IDLN_LIST:
		case IDLN_MODULE:
		case IDLN_SRCFILE:
			return TRUE;

		default: return FALSE;

		case IDLN_INTERFACE: {
			GList *tagmask_list = NULL,
				*methods = analyse_methods_of_iface(ctx->pr,
					&tagmask_list, tf->tree);
			GLIST_FOREACH(cur, methods) {
				struct method_info *inf = cur->data;
				/* (TODO: don't build the timeoutless variant if given an
				 * attribute not to do so. see header.c for details.)
				 */
				build_ipc_stub(ctx, inf, 0);
				int tok = op_timeout_kind(inf->node);
				if(tok != 0) build_ipc_stub(ctx, inf, tok);
			}
			g_list_foreach(methods, (GFunc)&free_method_info, NULL);
			g_list_free(methods);
			g_list_free(tagmask_list);
			return FALSE;
		}
	}
}
