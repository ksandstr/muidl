/*
 * dispatch.c -- build a dispatcher for an IDL interface
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

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <errno.h>

#include <llvm-c/Core.h>
#include <libIDL/IDL.h>

#include "defs.h"
#include "llvmutil.h"
#include "l4x2.h"

#define IN_MUIDL_IMPL
#include "muidl.h"


/* FIXME: move these three into support.c or some such */
static LLVMTypeRef llvm_supp_ctx_type(struct llvm_ctx *ctx)
{
	/* last_sender, last_tag, exn_tag */
	T types[3] = { ctx->wordt, ctx->wordt, ctx->wordt };
	return LLVMStructTypeInContext(ctx->ctx, types, 3, 0);
}
#define SUPP_LAST_SENDER_IX 0
#define SUPP_LAST_TAG_IX 1
#define SUPP_EXN_TAG_IX 2


static LLVMValueRef get_alloc_supp_ctx_fn(struct llvm_ctx *ctx)
{
	V fn = LLVMGetNamedFunction(ctx->module, "muidl_supp_alloc_context");
	if(fn == NULL) {
		T fnt = LLVMFunctionType(LLVMVoidTypeInContext(ctx->ctx),
			&ctx->i32t, 1, 0);
		fn = LLVMAddFunction(ctx->module, "muidl_supp_alloc_context", fnt);
	}
	assert(fn != NULL);
	return fn;
}


/* also called from except.c . */
LLVMValueRef build_fetch_supp_ctx(struct llvm_ctx *ctx)
{
	V fn = LLVMGetNamedFunction(ctx->module, "muidl_supp_get_context");
	if(fn == NULL) {
		T fnt = LLVMFunctionType(
			LLVMPointerType(LLVMInt8TypeInContext(ctx->ctx), 0),
			NULL, 0, 0);
		fn = LLVMAddFunction(ctx->module, "muidl_supp_get_context", fnt);
	}
	assert(fn != NULL);
	V rawptr = LLVMBuildCall(ctx->builder, fn, NULL, 0, "suppctx.ptr.raw");
	return LLVMBuildPointerCast(ctx->builder, rawptr,
		LLVMPointerType(llvm_supp_ctx_type(ctx), 0), "suppctx.ptr");
}


static LLVMValueRef build_ipcfailed_cond(
	struct llvm_ctx *ctx,
	LLVMValueRef mr0)
{
	return LLVMBuildICmp(ctx->builder, LLVMIntNE,
		ctx->zero,
		LLVMBuildAnd(ctx->builder, mr0,
			LLVMConstInt(ctx->wordt, 1 << 15, 0), "mr0.e"),
		"ipcfailed.cond");
}


static int vtable_byref_param_args(
	struct llvm_ctx *ctx,
	LLVMTypeRef *dst,
	IDL_tree type)
{
	int count = 1;
	if(IDL_NODE_TYPE(type) == IDLN_TYPE_ARRAY) {
		dst[0] = LLVMPointerType(
			llvm_rigid_type(ctx, get_array_type(type)), 0);
	} else if(is_rigid_type(type)) {
		dst[0] = LLVMPointerType(llvm_rigid_type(ctx, type), 0);
	} else if(IDL_NODE_TYPE(type) == IDLN_TYPE_STRING) {
		dst[0] = LLVMPointerType(LLVMInt8TypeInContext(ctx->ctx), 0);
	} else if(IDL_NODE_TYPE(type) == IDLN_TYPE_WIDE_STRING) {
		/* TODO: get wchar_t size from ABI! */
		dst[0] = LLVMPointerType(ctx->i32t, 0);
	} else {
		assert(IDL_NODE_TYPE(type) == IDLN_TYPE_SEQUENCE);
		dst[0] = LLVMPointerType(
			llvm_rigid_type(ctx, SEQ_SUBTYPE(type)), 0);
		dst[1] = LLVMPointerType(ctx->i32t, 0);
		count = 2;
	}
	return count;
}


/* dst should have two LLVMTypeRefs' worth of space to allow for sequences
 * (pointer + length). returns number of entries added, 2 for sequences and 1
 * for everything else.
 */
static int vtable_param_args(
	struct llvm_ctx *ctx,
	LLVMTypeRef *dst,
	IDL_tree pdecl)
{
	IDL_tree type = get_type_spec(IDL_PARAM_DCL(pdecl).param_type_spec);
	if(is_byval_param(pdecl)) {
		dst[0] = llvm_value_type(ctx, type);
		return 1;
	} else if(IDL_NODE_TYPE(type) == IDLN_TYPE_SEQUENCE
		&& IDL_PARAM_DCL(pdecl).attr == IDL_PARAM_IN)
	{
		dst[0] = LLVMPointerType(
			llvm_rigid_type(ctx, SEQ_SUBTYPE(type)), 0);
		dst[1] = ctx->i32t;
		return 2;
	} else {
		return vtable_byref_param_args(ctx, dst, type);
	}
}


/* FIXME: this is a terrible hack and should be remedied by separating the "has
 * return value y/n" decision from the type choice (i.e. also type
 * representation, of which we have 2: llvm and C)
 */
static LLVMTypeRef vtable_return_type(
	struct llvm_ctx *ctx,
	IDL_tree op,
	bool *actual_p)
{
	IDL_tree ret_type = get_type_spec(IDL_OP_DCL(op).op_type_spec);
	char *ctyp = return_type(ctx->ns, op, actual_p, true);
	LLVMTypeRef t;
	if(strcmp(ctyp, "int") == 0) t = ctx->i32t;
	else if(strcmp(ctyp, "void") == 0) t = LLVMVoidTypeInContext(ctx->ctx);
	else if(is_rigid_type(ret_type)) {
		/* TODO: enforce this in verify.c */
		assert(IDL_NODE_TYPE(ret_type) != IDLN_TYPE_ARRAY);
		t = llvm_rigid_type(ctx, ret_type);
	} else {
		NOTDEFINED(ret_type);
	}
	g_free(ctyp);
	return t;
}


/* TODO: wrap this to cache things in @ctx, and move it into a llvmutil.c or
 * some such
 */
static LLVMTypeRef get_vtable_type(
	struct llvm_ctx *ctx,
	const struct iface_info *iface)
{
	const int num_fields = g_list_length(iface->ops);
	T field_types[num_fields];
	for(int i=0; i<num_fields; i++) field_types[i] = NULL;

	GLIST_FOREACH(cur, iface->ops) {
		struct method_info *op = cur->data;
		IDL_tree idl_rettyp = get_type_spec(IDL_OP_DCL(op->node).op_type_spec),
			param_list = IDL_OP_DCL(op->node).parameter_dcls;
		/* each parameter can be a sequence */
		const int n_args_max = IDL_list_length(param_list) * 2
			+ (idl_rettyp != NULL ? 1 : 0);
		LLVMTypeRef arg_types[n_args_max];
		int arg_pos = 0;
		bool ret_actual = false;
		LLVMTypeRef rettyp = vtable_return_type(ctx, op->node, &ret_actual);
		if(idl_rettyp != NULL && !ret_actual) {
			arg_pos += vtable_byref_param_args(ctx, &arg_types[arg_pos],
				idl_rettyp);
			if(find_neg_exn(op->node) != NULL) rettyp = ctx->i32t;
			else rettyp = LLVMVoidTypeInContext(ctx->ctx);
		}
		IDL_LIST_FOREACH(p_cur, param_list) {
			assert(arg_pos < n_args_max);
			IDL_tree pdecl = IDL_LIST(p_cur).data;
			arg_pos += vtable_param_args(ctx, &arg_types[arg_pos], pdecl);
		}

		T funptr = LLVMPointerType(
			LLVMFunctionType(rettyp, arg_types, arg_pos, 0), 0);
		field_types[op->vtab_offset] = funptr;
	}

	return LLVMStructTypeInContext(ctx->ctx, field_types, num_fields, 0);
}


static void emit_out_param(
	struct llvm_ctx *ctx,
	LLVMValueRef *dst,
	IDL_tree ptyp)
{
	if(IS_MAPGRANT_TYPE(ptyp)) {
		/* this is just a pointer to a struct of 2 words. */
		dst[0] = build_local_storage(ctx, ctx->mapgrant, NULL,
			"out.mapgrant.mem");
	} else if(IDL_NODE_TYPE(ptyp) == IDLN_TYPE_ARRAY) {
		IDL_tree size_list = IDL_TYPE_ARRAY(ptyp).size_list;
		assert(IDL_list_length(size_list) == 1);
		int size = IDL_INTEGER(IDL_LIST(size_list).data).value;
		dst[0] = build_local_storage(ctx,
			llvm_rigid_type(ctx, get_array_type(ptyp)),
			CONST_INT(size), "out.array.mem");
	} else if(is_rigid_type(ptyp)) {
		dst[0] = build_local_storage(ctx, llvm_rigid_type(ctx, ptyp),
			NULL, "out.val.mem");
	} else if(IDL_NODE_TYPE(ptyp) == IDLN_TYPE_SEQUENCE) {
		dst[0] = build_seq_param_storage(ctx, ptyp, "out.seq.mem");
		dst[1] = build_local_storage(ctx, ctx->i32t, NULL, "out.seq.len.mem");
	} else if(IDL_NODE_TYPE(ptyp) == IDLN_TYPE_STRING) {
		int max_size = STR_BOUND_VAL(ptyp);
		dst[0] = build_local_storage(ctx, LLVMInt8TypeInContext(ctx->ctx),
			CONST_INT(max_size + 1), "out.str.mem");
	} else {
		g_assert_not_reached();
	}
}


static LLVMBasicBlockRef build_op_decode(
	struct llvm_ctx *ctx,
	LLVMValueRef function,
	const struct method_info *inf,
	const struct stritem_info *stritems)
{
	struct print_ctx *pr = ctx->pr;
	char *name = !IS_EXN_MSG(inf)
		? decapsify(IDL_IDENT(IDL_OP_DCL(inf->node).ident).str)
		: decapsify(IDL_IDENT(IDL_EXCEPT_DCL(inf->node).ident).str);
	char *opname = tmp_f(pr, "%s.decode", name);
	BB bb = LLVMAppendBasicBlockInContext(ctx->ctx, function, opname);
	opname = tmp_f(pr, "%s", name);
	g_free(name);

	LLVMTypeRef rv_type = vtable_return_type(ctx, inf->node, NULL);
	const bool oneway = IDL_OP_DCL(inf->node).f_oneway;
	const struct message_info *req = inf->request, *reply = NULL;
	if(!oneway) {
		assert(inf->num_reply_msgs > 0);
		assert(!IS_EXN_MSG(inf->replies[0]));
		reply = inf->replies[0];
	}

	LLVMPositionBuilderAtEnd(ctx->builder, bb);

	/* find the position of the first typed word. */
	LLVMValueRef tag_u_val = build_u_from_tag(ctx, ctx->tag);
	ctx->tmax = LLVMBuildAdd(ctx->builder,
		tag_u_val, build_t_from_tag(ctx, ctx->tag), "tmax");
	ctx->tpos = LLVMBuildAdd(ctx->builder, LLVMConstInt(ctx->i32t, 1, 0),
		tag_u_val, "tpos.initial");

	/* build the "args" array. */
	const int num_args_max = 2 * IDL_list_length(
		IDL_OP_DCL(inf->node).parameter_dcls) + 1;
	V *args = g_new(V, num_args_max), *ret_args = NULL;
	int max_arg = -1, arg_offset = 0;
	bool have_ret_by_val = false;
	if(!oneway && reply->ret_type != NULL) {
		/* take an arg slot for the return value. */
		if(!reply->ret_by_ref) {
			args[0] = NULL;		/* will be filled in with fncall value */
			max_arg = 0;
			have_ret_by_val = true;
		} else {
			emit_out_param(ctx, &args[0], reply->ret_type);
		}
		ret_args = &args[0];
		arg_offset = 1;
	}
	V *d_args = &args[arg_offset];
	for(IDL_tree cur = IDL_OP_DCL(inf->node).parameter_dcls;
		cur != NULL;
		cur = IDL_LIST(cur).next)
	{
		IDL_tree pdecl = IDL_LIST(cur).data,
			type = get_type_spec(IDL_PARAM_DCL(pdecl).param_type_spec);
		enum IDL_param_attr attr = IDL_PARAM_DCL(pdecl).attr;
		int nd_args = IDL_NODE_TYPE(type) == IDLN_TYPE_SEQUENCE ? 2 : 1;
		struct msg_param *p;
		if(attr == IDL_PARAM_IN) {
			p = find_pdecl(req->params, pdecl);
			assert(p != NULL);
			/* zero them off to be safe. (TODO: could use a value of a type
			 * that is never valid here, to catch errors in a prettier way than
			 * "boom, segfault")
			 */
			for(int i=0; i<nd_args; i++) d_args[p->arg_ix + i] = NULL;
		} else /* out, inout */ {
			assert(!oneway);
			p = find_pdecl(reply->params, pdecl);
			assert(p != NULL);
			emit_out_param(ctx, &d_args[p->arg_ix], type);
		}
		max_arg = MAX(max_arg, p->arg_ix + nd_args - 1 + arg_offset);
	}

	/* the decoder. */
	build_msg_decoder(ctx, NULL, d_args, req, 0, stritems, false);

	/* the function call. */
	V *call_args = args;
	int call_num_args = max_arg + 1;
	if(have_ret_by_val) {
		/* skip the return value argument. */
		assert(max_arg >= 0);
		call_args = &args[1];
		call_num_args--;
	}
	/* dereference in-parameter inline sequences' length values.
	 *
	 * TODO: should this be done with sequences that are transferred as string
	 * items, also?
	 */
	GLIST_FOREACH(cur, req->seq) {
		struct msg_param *seq = cur->data;
		if(IDL_PARAM_DCL(seq->param_dcl).attr != IDL_PARAM_IN) continue;
		V len = LLVMBuildLoad(ctx->builder, d_args[seq->arg_ix + 1],
			"inlseq.len");
		d_args[seq->arg_ix + 1] = len;
	}
	if(inf->num_reply_msgs > 1) {
		/* if exceptions can be raised, clear the indicator. */
		LLVMBuildStore(ctx->builder, CONST_WORD(0),
			LLVMBuildStructGEP(ctx->builder, ctx->supp_ctx_ptr,
				SUPP_EXN_TAG_IX, "exn.tag.ptr"));
	}
	V fnptr = LLVMBuildLoad(ctx->builder,
		LLVMBuildStructGEP(ctx->builder, ctx->vtab_arg, inf->vtab_offset,
				tmp_f(pr, "%s.offs", opname)),
			tmp_f(pr, "%s.fnptr", opname));
	/* FIXME: previously, LLVMCountParams(fnptr) would work here. as of llvm
	 * 3.8 however, that causes a segfault. so the assertion is gone until
	 * LLVM gets its shit back together.
	 */
	// assert(call_num_args == LLVMCountParams(fnptr));
	V fncall = LLVMBuildCall(ctx->builder, fnptr, call_args, call_num_args,
		IS_VOID_TYPEREF(rv_type) ? "" : tmp_f(pr, "%s.call", opname));

	if(oneway) {
		LLVMBuildBr(ctx->builder, ctx->wait_bb);
		goto end;
	}

	/* check raised exceptions: first, the negativereturn one. */
	int num_exns = inf->num_reply_msgs - 1;
	if(num_exns > 0 && find_neg_exn(inf->node) != NULL) {
		/* examine NegativeReturn exception trigger */
		V ok_cond = LLVMBuildICmp(ctx->builder, LLVMIntSGE, fncall,
			CONST_INT(0), "rcneg.cond");
		BB chain = add_sibling_block(ctx, "%s.%s", opname,
			num_exns == 1 ? "pack_reply" : "tag_exn_chk");
		BB msgerr_bb = get_msgerr_bb(ctx);
		branch_set_phi(ctx, ctx->errval_phi, fncall);
		LLVMBuildCondBr(ctx->builder, ok_cond, chain, msgerr_bb);
		LLVMPositionBuilderAtEnd(ctx->builder, chain);
		num_exns--;
	}

	/* then no-reply and complex exceptions. */
	if(num_exns > 0) {
		V exn_tag = LLVMBuildLoad(ctx->builder,
			LLVMBuildStructGEP(ctx->builder, ctx->supp_ctx_ptr,
				SUPP_EXN_TAG_IX, "exn.tag.ptr"), "exn.tag");
		IDL_tree nre = find_noreply_exn(inf->node);
		if(nre != NULL) num_exns--;
		BB chain = add_sibling_block(ctx, "%s.pack_reply", opname);
		if(num_exns == 0) {
			assert(nre != NULL);
			/* the nonreply one alone. */
			V noreply_cond = LLVMBuildICmp(ctx->builder, LLVMIntEQ,
				exn_tag, CONST_WORD(~0ull), "noreply.cond");
			LLVMBuildCondBr(ctx->builder, noreply_cond, ctx->wait_bb, chain);
		} else {
			/* default: reply_bb, for propagating the exception contents. */
			branch_set_phi(ctx, ctx->reply_tag, exn_tag);
			V sw = LLVMBuildSwitch(ctx->builder, exn_tag,
				ctx->reply_bb, nre != NULL ? 2 : 1);
			if(nre != NULL) {
				/* when ~0ull, goto wait_bb */
				LLVMAddCase(sw, CONST_WORD(~0ull), ctx->wait_bb);
			}
			/* when 0, goto pack_reply */
			LLVMAddCase(sw, CONST_WORD(0), chain);
		}
		LLVMPositionBuilderAtEnd(ctx->builder, chain);
	}

	/* pack outputs from the non-exceptional return. */
	if(have_ret_by_val) {
		/* fixup a by-val return value */
		assert(is_value_type(reply->ret_type));
		assert(args[0] == NULL);
		args[0] = LLVMBuildTruncOrBitCast(ctx->builder, fncall,
			llvm_value_type(ctx, reply->ret_type),
			tmp_f(pr, "%s.retval", opname));
	}
	V enc_tag = build_msg_encoder(ctx, reply, ret_args, d_args, true);
	branch_set_phi(ctx, ctx->reply_tag, enc_tag);
	LLVMBuildBr(ctx->builder, ctx->reply_bb);

end:
	g_free(args);
	return bb;
}


/* (this actually allocates memory that is pointed to by the string items in
 * the buffer registers.)
 */
static void build_alloc_stritems(
	struct llvm_ctx *ctx,
	struct stritem_info *stritems)
{
	if(stritems == NULL) return;
	for(int i=0; stritems[i].length >= 0; i++) {
		/* TODO: the "4" is sizeof(wchar_t); get this from the ABI spec */
		int len = stritems[i].length + (stritems[i].stringlike ? 4 : 0);
		/* TODO: allocate small blocks (by some reasonable criteria... a single
		 * cache line's size, or something) with build_local_storage()
		 */
		stritems[i].memptr = build_malloc_storage(ctx,
			LLVMInt8TypeInContext(ctx->ctx),
			LLVMConstInt(ctx->i32t, len, 0),
			tmp_f(ctx->pr, "stritem%d.ptr", i));
	}
}


static void build_store_br(
	struct llvm_ctx *ctx,
	LLVMValueRef value,
	int br)
{
	assert(br <= 32 && br >= 0);
	LLVMValueRef br_addr = UTCB_ADDR_VAL(ctx,
		CONST_INT(BR_OFFSET(br)), tmp_f(ctx->pr, "br%d.ptr", br));
	LLVMBuildStore(ctx->builder, value, br_addr);
}


static void build_set_strbufs(
	struct llvm_ctx *ctx,
	const struct stritem_info *stritems)
{
	assert(stritems != NULL && stritems[0].length >= 0);
	for(int i=0; stritems[i].length >= 0; i++) {
		V si[2];
		build_simple_string_item(ctx, si, stritems[i].memptr,
			CONST_WORD(stritems[i].length), NULL);
		if(stritems[i+1].length >= 0) {
			/* non-last string buffers have low bit, "C", set. */
			si[0] = LLVMBuildOr(ctx->builder, si[0], CONST_WORD(1),
				"strbuf.info.cont");
		}
		build_store_br(ctx, si[0], i * 2 + 1);
		build_store_br(ctx, si[1], i * 2 + 2);
	}
}


static gint method_by_tagmask_cmp(gconstpointer ap, gconstpointer bp)
{
	const struct method_info *a = ap, *b = bp;
	if(a->request->tagmask < b->request->tagmask) return -1;
	else if(a->request->tagmask > b->request->tagmask) return 1;
	else return 0;
}


/* FIXME: this mechanism doesn't distinguish between ops that can send a msg
 * error back, and ops that're declared oneway. i.e. a oneway operation that
 * takes a string item parameter will want to chuck -EINVAL through this
 * routine, leading to reply_bb, when it really shouldn't.
 *
 * perhaps some context ought to be carried through here, or something.
 */
static void build_dispatcher_msgerr(struct llvm_ctx *ctx)
{
	if(ctx->reply_tag != NULL) {
		LLVMValueRef msgerr_tag = LLVMBuildOr(ctx->builder,
			CONST_WORD(1), CONST_WORD(1 << 16), "msgerr.tag");
		LLVMBuildStore(ctx->builder,
			LLVMBuildNeg(ctx->builder, ctx->errval_phi, "rcneg.val"),
			UTCB_ADDR_VAL(ctx, CONST_INT(MR_OFFSET(1)), "mr1.addr"));
		branch_set_phi(ctx, ctx->reply_tag, msgerr_tag);
		LLVMBuildBr(ctx->builder, ctx->reply_bb);
	} else {
		/* no message sends a "invalid format" reply back, so jump right back
		 * to wherever.
		 */
		LLVMBuildBr(ctx->builder, ctx->wait_bb);
	}
}


LLVMValueRef build_dispatcher_function(
	struct llvm_ctx *ctx,
	const struct iface_info *iface)
{
	/* compute min_u for build_store_received_regs() over all ops' request
	 * halves.
	 */
	int min_u = 66;
	GLIST_FOREACH(cur, iface->ops) {
		const struct method_info *inf = cur->data;
		int mu = msg_min_u(inf->request);
		min_u = MIN(min_u, mu);
	}
	min_u--;

	LLVMTypeRef param = LLVMPointerType(get_vtable_type(ctx, iface), 0),
		fn_type = LLVMFunctionType(ctx->wordt, &param, 1, 0);

	char *dispname = dispatcher_name(ctx->ns, iface->node, NULL);
	LLVMValueRef fn = LLVMAddFunction(ctx->module, dispname, fn_type);
	ctx->vtab_arg = LLVMGetFirstParam(fn);
	g_free(dispname);

	begin_function(ctx, fn);
	ctx->build_msgerr_bb = &build_dispatcher_msgerr;
	ctx->msgerr_bb = NULL;

	BB loop_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "loop"),
		exit_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "exit"),
		ret_ec_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "ret_errcode"),
		dispatch_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "dispatch");
	ctx->wait_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "wait");
	ctx->reply_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "reply");

	/* the entry block. */
	/* support context & its pointer */
	V alloc_supp_fn = get_alloc_supp_ctx_fn(ctx),
		supp_size = LLVMBuildTruncOrBitCast(ctx->builder,
			LLVMSizeOf(llvm_supp_ctx_type(ctx)), ctx->i32t,
			"supp.ctx.size.int");
	LLVMBuildCall(ctx->builder, alloc_supp_fn, &supp_size, 1, "");
	ctx->supp_ctx_ptr = build_fetch_supp_ctx(ctx);
	/* store xfer timeouts at point of entry, i.e. as they are given by the
	 * caller.
	 */
	LLVMValueRef xfer_timeouts_addr = UTCB_ADDR_VAL(ctx,
			CONST_INT(TCR_XFER_TIMEOUTS), "xferto.addr"),
		stored_timeouts = LLVMBuildLoad(ctx->builder, xfer_timeouts_addr,
			"stored_timeouts");
	/* string buffers */
	struct stritem_info *stritems = dispatcher_stritems(iface->ops);
	build_alloc_stritems(ctx, stritems);
	const bool have_stringbufs = stritems != NULL && stritems[0].length >= 0;
	/* acceptor word (TODO: map/grant items) */
	LLVMValueRef acceptor = LLVMConstInt(ctx->wordt,
		have_stringbufs ? 1 : 0, 0);
	/* (this block will be closed as ctx->alloc_bb down near function end.) */

	/* non-reply IPC wait block. */
	LLVMPositionBuilderAtEnd(ctx->builder, ctx->wait_bb);
	LLVMBuildStore(ctx->builder, stored_timeouts, xfer_timeouts_addr);
	build_store_br(ctx, acceptor, 0);
	if(have_stringbufs) build_set_strbufs(ctx, stritems);
	LLVMValueRef ipc_from, ipc_mr1, ipc_mr2,
		ipc_tag = build_l4_ipc_call(ctx,
			CONST_WORD(0), CONST_WORD(0), LLVMConstNot(CONST_WORD(0)),
			CONST_WORD(0), &ipc_from, &ipc_mr1, &ipc_mr2);
	build_store_received_regs(ctx, min_u, ipc_mr1, ipc_mr2);
	LLVMBuildBr(ctx->builder, loop_bb);

	/* the main dispatch-replywait loop. */
	LLVMPositionBuilderAtEnd(ctx->builder, loop_bb);
	ctx->from = LLVMBuildPhi(ctx->builder, ctx->wordt, "from.phi");
	ctx->mr1 = LLVMBuildPhi(ctx->builder, ctx->wordt, "mr1.phi");
	ctx->mr2 = LLVMBuildPhi(ctx->builder, ctx->wordt, "mr2.phi");
	ctx->tag = LLVMBuildPhi(ctx->builder, ctx->wordt, "tag.phi");
	LLVMAddIncoming(ctx->from, &ipc_from, &ctx->wait_bb, 1);
	LLVMAddIncoming(ctx->mr1, &ipc_mr1, &ctx->wait_bb, 1);
	LLVMAddIncoming(ctx->mr2, &ipc_mr2, &ctx->wait_bb, 1);
	LLVMAddIncoming(ctx->tag, &ipc_tag, &ctx->wait_bb, 1);
	LLVMBuildCondBr(ctx->builder, build_ipcfailed_cond(ctx, ctx->tag),
		ret_ec_bb, dispatch_bb);

	LLVMPositionBuilderAtEnd(ctx->builder, ctx->reply_bb);
	if(!iface->has_replies) {
		/* no replies in this interface; go straight back to waiting. */
		LLVMBuildBr(ctx->builder, ctx->wait_bb);
		ctx->reply_tag = NULL;
	} else {
		/* send reply, receive next message. */
		/* message registers were already loaded, since ia32 only requires the tag
		 * in a cpu register.
		 */
		ctx->reply_tag = LLVMBuildPhi(ctx->builder, ctx->wordt,
			"replytag.phi");
		LLVMBuildStore(ctx->builder, stored_timeouts, xfer_timeouts_addr);
		build_store_br(ctx, acceptor, 0);
		if(have_stringbufs) build_set_strbufs(ctx, stritems);
		/* ~0 is L4_anythread. (it could also be a symbolic constant.) */
		uint32_t replywait_tos = (0x400 << 16) | 0;	/* zerotime, never */
		ipc_tag = build_l4_ipc_call(ctx,
			ctx->from, CONST_WORD(replywait_tos), LLVMConstNot(CONST_WORD(0)),
			ctx->reply_tag, &ipc_from, &ipc_mr1, &ipc_mr2);
		build_store_received_regs(ctx, min_u, ipc_mr1, ipc_mr2);
		LLVMAddIncoming(ctx->from, &ipc_from, &ctx->reply_bb, 1);
		LLVMAddIncoming(ctx->mr1, &ipc_mr1, &ctx->reply_bb, 1);
		LLVMAddIncoming(ctx->mr2, &ipc_mr2, &ctx->reply_bb, 1);
		LLVMAddIncoming(ctx->tag, &ipc_tag, &ctx->reply_bb, 1);
		LLVMBuildBr(ctx->builder, loop_bb);
	}

	/* exit_bb's interface */
	LLVMPositionBuilderAtEnd(ctx->builder, exit_bb);
	LLVMValueRef retval = LLVMBuildPhi(ctx->builder, ctx->wordt, "retval");

	/* return L4_ErrorCode(); */
	LLVMPositionBuilderAtEnd(ctx->builder, ret_ec_bb);
	LLVMValueRef errorcode = LLVMBuildLoad(ctx->builder,
		UTCB_ADDR_VAL(ctx, CONST_INT(TCR_ERROR_CODE), "errcode.addr"), "ec");
	LLVMAddIncoming(retval, &errorcode, &ret_ec_bb, 1);
	LLVMBuildBr(ctx->builder, exit_bb);

	/* recognize interfaces that've got tag-mask dispatching. */
	GList *tm_list = g_list_copy(iface->tagmask_ops);
	LLVMBasicBlockRef tm_dispatch_bb = NULL;
	if(tm_list != NULL) {
		tm_dispatch_bb = LLVMAppendBasicBlockInContext(ctx->ctx,
			fn, "tagmask.dispatch");
	}

	/* dispatch according to ctx->tag. */
	LLVMPositionBuilderAtEnd(ctx->builder, dispatch_bb);
	/* update the skel context. */
	LLVMBuildStore(ctx->builder, ctx->from,
		LLVMBuildStructGEP(ctx->builder, ctx->supp_ctx_ptr,
			SUPP_LAST_SENDER_IX, "supp.last_sender.ptr"));
	LLVMBuildStore(ctx->builder, ctx->tag,
		LLVMBuildStructGEP(ctx->builder, ctx->supp_ctx_ptr,
			SUPP_LAST_TAG_IX, "supp.last_tag.ptr"));
	/* TODO: get the correct value */
	LLVMValueRef labelswitch = LLVMBuildSwitch(ctx->builder,
		build_label_from_tag(ctx, ctx->tag),
		tm_list != NULL ? tm_dispatch_bb : exit_bb, 2);

	/* tag-mask dispatching. */
	if(tm_list != NULL) {
		LLVMPositionBuilderAtEnd(ctx->builder, tm_dispatch_bb);
		tm_list = g_list_sort(tm_list, &method_by_tagmask_cmp);
		uint32_t prior = NO_TAGMASK;
		LLVMValueRef mask_switch = NULL;
		GLIST_FOREACH(cur, tm_list) {
			const struct method_info *inf = cur->data;
			if(prior != inf->request->tagmask) {
				/* chain start. */
				prior = inf->request->tagmask;
				tm_dispatch_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn,
					tmp_f(ctx->pr, "tagmask.%x.next", prior));
				/* TODO: compute the right number of branches? */
				LLVMValueRef masked_label = LLVMBuildLShr(ctx->builder,
					LLVMBuildAnd(ctx->builder, ctx->tag,
						LLVMConstInt(ctx->wordt, prior, 0),
						"tag.masked"),
					LLVMConstInt(ctx->i32t, 16, 0),
					"label.masked");
				mask_switch = LLVMBuildSwitch(ctx->builder,
					masked_label, tm_dispatch_bb, 2);
				LLVMPositionBuilderAtEnd(ctx->builder, tm_dispatch_bb);
			}

			/* chain member. */
			LLVMAddCase(mask_switch,
				LLVMConstInt(ctx->wordt, inf->request->label, 0),
				build_op_decode(ctx, fn, inf, stritems));
		}
		g_list_free(tm_list);
	}

	/* return 42666 (a µIDL special value) on unrecognized label. */
	if(tm_dispatch_bb != NULL) {
		LLVMPositionBuilderAtEnd(ctx->builder, tm_dispatch_bb);
	}
	LLVMBasicBlockRef current = LLVMGetInsertBlock(ctx->builder);
	LLVMValueRef unknownlabel = LLVMConstInt(ctx->wordt,
		MUIDL_UNKNOWN_LABEL, 0);
	LLVMAddIncoming(retval, &unknownlabel, &current, 1);
	if(tm_dispatch_bb != NULL) {
		/* must branch explicitly. the non-bb path exits from the switch
		 * instruction.
		 */
		LLVMBuildBr(ctx->builder, exit_bb);
	}

	int top_label = -1;
	LLVMValueRef sublabelswitch = NULL;
	GLIST_FOREACH(cur, iface->ops) {
		struct method_info *inf = cur->data;
		/* tag-mask dispatching is handled above */
		if(inf->request->tagmask != NO_TAGMASK) continue;
		if(inf->request->sublabel == NO_SUBLABEL) {
			LLVMBasicBlockRef decode_bb = build_op_decode(ctx, fn, inf,
				stritems);
			LLVMAddCase(labelswitch,
				LLVMConstInt(ctx->wordt, inf->request->label, 0),
				decode_bb);
		} else {
			if(top_label != -1 && top_label != inf->request->label) {
				/* restore pre-first state */
				top_label = -1;
				sublabelswitch = NULL;
			}
			if(top_label == -1) {
				/* begin a new sub-switch. */
				assert(sublabelswitch == NULL);
				top_label = inf->request->label;
				LLVMBasicBlockRef sub_bb = LLVMAppendBasicBlockInContext(
					ctx->ctx, fn, tmp_f(ctx->pr, "sub%04x.dispatch",
						(unsigned)top_label));
				LLVMPositionBuilderAtEnd(ctx->builder, sub_bb);
				/* TODO: get the correct value for "2" */
				sublabelswitch = LLVMBuildSwitch(ctx->builder,
					ctx->mr1, exit_bb, 2);
				LLVMAddIncoming(retval, &unknownlabel, &sub_bb, 1);
				LLVMAddCase(labelswitch,
					LLVMConstInt(ctx->wordt, top_label, 0), sub_bb);
			}
			assert(sublabelswitch != NULL);
			assert(top_label == inf->request->label);
			LLVMAddCase(sublabelswitch,
				LLVMConstInt(ctx->wordt, inf->request->sublabel, 0),
				build_op_decode(ctx, fn, inf, stritems));
		}
	}

	/* close off the alloc (entry) block and emit free insns for all blocks in
	 * the malloc list
	 */
	LLVMPositionBuilderAtEnd(ctx->builder, exit_bb);
	end_function(ctx, ctx->wait_bb);
	LLVMBuildRet(ctx->builder, retval);

	LLVMDisposeBuilder(ctx->builder);
	ctx->builder = NULL;
	g_free(stritems);
	return fn;
}
