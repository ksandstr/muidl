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

#include "muidl.h"
#include "llvmutil.h"
#include "l4x2.h"


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


/* dst should have two LLVMTypeRefs' worth of space to allow for sequences
 * (pointer + length).
 */
static void vtable_in_param_type(
	struct llvm_ctx *ctx,
	LLVMTypeRef *dst,
	int *pos_p,
	IDL_tree type)
{
	if(is_value_type(type)) {
		dst[(*pos_p)++] = llvm_value_type(ctx, type);
	} else if(is_rigid_type(ctx->ns, type)) {
		dst[(*pos_p)++] = LLVMPointerType(llvm_rigid_type(ctx, type), 0);
	} else if(IDL_NODE_TYPE(type) == IDLN_TYPE_STRING) {
		dst[(*pos_p)++] = LLVMPointerType(LLVMInt8TypeInContext(ctx->ctx), 0);
	} else if(IDL_NODE_TYPE(type) == IDLN_TYPE_WIDE_STRING) {
		/* TODO: get wchar_t size from ABI! */
		dst[(*pos_p)++] = LLVMPointerType(ctx->i32t, 0);
	} else if(IDL_NODE_TYPE(type) == IDLN_TYPE_SEQUENCE) {
		IDL_tree seqtype = get_type_spec(
			IDL_TYPE_SEQUENCE(type).simple_type_spec);
		dst[(*pos_p)++] = LLVMPointerType(llvm_value_type(ctx, seqtype), 0);
		dst[(*pos_p)++] = ctx->i32t;
	} else {
		/* should not be reached, either. something fell out. */
		NOTDEFINED(type);
	}
}


static void vtable_out_param_type(
	struct llvm_ctx *ctx,
	LLVMTypeRef *dst,
	int *pos_p,
	IDL_tree type)
{
	if(is_rigid_type(ctx->ns, type)) {
		dst[(*pos_p)++] = LLVMPointerType(llvm_rigid_type(ctx, type), 0);
	} else if(IDL_NODE_TYPE(type) == IDLN_TYPE_SEQUENCE) {
		IDL_tree seqtype = get_type_spec(
			IDL_TYPE_SEQUENCE(type).simple_type_spec);
		dst[(*pos_p)++] = LLVMPointerType(llvm_rigid_type(ctx, seqtype), 0);
		dst[(*pos_p)++] = LLVMPointerType(ctx->i32t, 0);
	} else if(IS_MAPGRANT_TYPE(type)) {
		dst[(*pos_p)++] = LLVMPointerType(ctx->mapgrant, 0);
	} else if(IDL_NODE_TYPE(type) == IDLN_TYPE_STRING) {
		dst[(*pos_p)++] = LLVMPointerType(LLVMInt8TypeInContext(ctx->ctx), 0);
	} else if(IDL_NODE_TYPE(type) == IDLN_TYPE_WIDE_STRING) {
		/* TODO: use an ABI-specific wchar_t type */
		dst[(*pos_p)++] = LLVMPointerType(ctx->i32t, 0);
	} else {
		NOTDEFINED(type);
	}
}


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
	else if(is_rigid_type(ctx->ns, ret_type)) {
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
static LLVMTypeRef get_vtable_type(struct llvm_ctx *ctx, IDL_tree iface)
{
	GList *methods = all_methods_of_iface(ctx->ns, iface);
	int num_fields = g_list_length(methods), f_offs = 0;
	LLVMTypeRef field_types[num_fields];
	GList *cur = g_list_first(methods);
	while(f_offs < num_fields) {
		assert(cur != NULL);
		IDL_tree op = cur->data,
			idl_rettyp = get_type_spec(IDL_OP_DCL(op).op_type_spec),
			param_list = IDL_OP_DCL(op).parameter_dcls;
		/* each parameter can be a sequence */
		const int n_args_max = IDL_list_length(param_list) * 2
			+ (idl_rettyp != NULL ? 1 : 0);
		LLVMTypeRef arg_types[n_args_max];
		int arg_pos = 0;
		bool ret_actual = false;
		LLVMTypeRef rettyp = vtable_return_type(ctx, op, &ret_actual);
		if(idl_rettyp != NULL && !ret_actual) {
			vtable_out_param_type(ctx, arg_types, &arg_pos, idl_rettyp);
			if(find_neg_exn(op) != NULL) rettyp = ctx->i32t;
			else rettyp = LLVMVoidTypeInContext(ctx->ctx);
		}
		for(IDL_tree p_cur = param_list;
			p_cur != NULL;
			p_cur = IDL_LIST(p_cur).next)
		{
			assert(arg_pos < n_args_max);
			IDL_tree pdecl = IDL_LIST(p_cur).data,
				ptype = get_type_spec(IDL_PARAM_DCL(pdecl).param_type_spec);
			switch(IDL_PARAM_DCL(pdecl).attr) {
				case IDL_PARAM_IN:
					vtable_in_param_type(ctx, arg_types, &arg_pos, ptype);
					break;

				/* inout parameters are passed exactly like out-parameters, but
				 * with a value already present.
				 */
				case IDL_PARAM_OUT:
				case IDL_PARAM_INOUT:
					vtable_out_param_type(ctx, arg_types, &arg_pos, ptype);
					break;
			}
		}

		cur = g_list_next(cur);
		field_types[f_offs++] = LLVMPointerType(
			LLVMFunctionType(rettyp, arg_types, arg_pos, 0), 0);
	}
	assert(cur == NULL);
	g_list_free(methods);

	return LLVMStructTypeInContext(ctx->ctx, field_types, num_fields, 0);
}


static void emit_out_param(
	struct llvm_ctx *ctx,
	LLVMValueRef *dst,
	IDL_tree ptyp)
{
	if(is_value_type(ptyp)) {
		dst[0] = build_local_storage(ctx, llvm_value_type(ctx, ptyp),
			NULL, "out.val.mem");
	} else if(IDL_NODE_TYPE(ptyp) == IDLN_TYPE_SEQUENCE) {
		IDL_tree seqtype = get_type_spec(
			IDL_TYPE_SEQUENCE(ptyp).simple_type_spec);
		int max_size = IDL_INTEGER(
			IDL_TYPE_SEQUENCE(ptyp).positive_int_const).value;
		dst[0] = build_local_storage(ctx,
			llvm_value_type(ctx, seqtype),
			LLVMConstInt(ctx->i32t, max_size, 0),
			"out.seq.mem");
		dst[1] = build_local_storage(ctx, ctx->i32t, NULL, "out.seq.len.mem");
	} else if(IS_MAPGRANT_TYPE(ptyp)) {
		/* this is just a pointer to a struct of 2 words. */
		dst[0] = build_local_storage(ctx, ctx->mapgrant, NULL,
			"out.mapgrant.mem");
	} else {
		/* TODO: add the rest! */
		NOTDEFINED(ptyp);
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
	char *opname = tmp_f(pr, "decode.%s", name);
	LLVMBasicBlockRef bb = LLVMAppendBasicBlockInContext(ctx->ctx,
		function, opname);
	g_free(name);
	opname = &opname[7];	/* skip "decode." for value names */

	LLVMTypeRef rv_type = vtable_return_type(ctx, inf->node, NULL);
	const bool oneway = IDL_OP_DCL(inf->node).f_oneway;
	const struct message_info *req = inf->request, *reply = NULL;
	if(!oneway) {
		assert(inf->num_reply_msgs > 0);
		assert(!IS_EXN_MSG(inf->replies[0]));
		reply = inf->replies[0];
	}

	LLVMPositionBuilderAtEnd(ctx->builder, bb);

	/* find the position of the first inline sequence */
	if(req->seq == NULL) ctx->inline_seq_pos = NULL;
	else {
		/* determined by IDL; we don't depend on the runtime tag value. */
		ctx->inline_seq_pos = CONST_INT(req->tag_u + 1);
	}
	/* and that of the first typed word. */
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
//		printf("%s: param `%s', arg_ix %d (offset %d)\n", __func__,
//			p->name, p->arg_ix, arg_offset);
	}

	/* the decoder. */
	build_msg_decoder(ctx, NULL, d_args, req, stritems, false);

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
		V len = LLVMBuildLoad(ctx->builder, args[seq->arg_ix + 1],
			"inlseq.len");
		args[seq->arg_ix + 1] = len;
	}
	V fnptr = LLVMBuildLoad(ctx->builder,
		LLVMBuildStructGEP(ctx->builder, ctx->vtab_arg, inf->vtab_offset,
				tmp_f(pr, "%s.offs", opname)),
			tmp_f(pr, "%s.fnptr", opname));
	assert(call_num_args == LLVMCountParams(fnptr));
	V fncall = LLVMBuildCall(ctx->builder, fnptr, call_args, call_num_args,
		IS_VOID_TYPEREF(rv_type) ? "" : tmp_f(pr, "%s.call", opname));

	if(oneway) {
		LLVMBuildBr(ctx->builder, ctx->wait_bb);
		goto end;
	}

	LLVMBasicBlockRef pr_bb = LLVMAppendBasicBlockInContext(ctx->ctx,
			function, tmp_f(pr, "%s.pack_reply", opname)),
		ex_chain_bb = pr_bb;	/* exception chain, or result packer */

	/* TODO: exceptions! */

	IDL_tree n_ex = find_neg_exn(inf->node);
	if(n_ex == NULL) LLVMBuildBr(ctx->builder, ex_chain_bb);
	else {
		/* examine NegativeReturn exception trigger */
		LLVMValueRef ok_cond = LLVMBuildICmp(ctx->builder, LLVMIntSGE, fncall,
			LLVMConstInt(ctx->i32t, 0, 1), "rcneg.cond");
		LLVMBasicBlockRef current_bb = LLVMGetInsertBlock(ctx->builder),
			msgerr_bb = get_msgerr_bb(ctx);
		LLVMAddIncoming(ctx->errval_phi, &fncall, &current_bb, 1);
		LLVMBuildCondBr(ctx->builder, ok_cond, ex_chain_bb, msgerr_bb);
	}

	/* pack results from the non-exceptional return. */
	LLVMPositionBuilderAtEnd(ctx->builder, pr_bb);
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


static gint method_by_tagmask_cmp(gconstpointer ap, gconstpointer bp)
{
	const struct method_info *a = ap, *b = bp;
	if(a->request->tagmask < b->request->tagmask) return -1;
	else if(a->request->tagmask > b->request->tagmask) return 1;
	else return 0;
}


static void build_dispatcher_msgerr(struct llvm_ctx *ctx)
{
	LLVMValueRef msgerr_tag = LLVMBuildOr(ctx->builder,
		CONST_WORD(1), CONST_WORD(1 << 16), "msgerr.tag");
	LLVMBuildStore(ctx->builder,
		LLVMBuildNeg(ctx->builder, ctx->errval_phi, "rcneg.val"),
		UTCB_ADDR_VAL(ctx, CONST_INT(MR_OFFSET(1)), "mr1.addr"));
	branch_set_phi(ctx, ctx->reply_tag, msgerr_tag);
	LLVMBuildBr(ctx->builder, ctx->reply_bb);
}


static LLVMValueRef build_dispatcher_function(struct llvm_ctx *ctx, IDL_tree iface)
{
	GList *tagmask_list = NULL,
		*methods = analyse_methods_of_iface(ctx->pr, &tagmask_list, iface);

	LLVMTypeRef param = LLVMPointerType(get_vtable_type(ctx, iface), 0),
		fn_type = LLVMFunctionType(ctx->wordt, &param, 1, 0);

	char *dispname = dispatcher_name(ctx->ns, iface, NULL);
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
		uint_zero = CONST_INT(0);
	LLVMBuildCall(ctx->builder, alloc_supp_fn, &uint_zero, 1, "");
	V supp_ctx_ptr = build_fetch_supp_ctx(ctx);
	/* store xfer timeouts at point of entry, i.e. as they are given by the
	 * caller.
	 */
	LLVMValueRef xfer_timeouts_addr = UTCB_ADDR_VAL(ctx,
			CONST_INT(TCR_XFER_TIMEOUTS), "xferto.addr"),
		stored_timeouts = LLVMBuildLoad(ctx->builder, xfer_timeouts_addr,
			"stored_timeouts");
	/* string buffers */
	struct stritem_info *stritems = dispatcher_stritems(methods);
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

	/* send reply, receive next message. */
	/* message registers were already loaded, since ia32 only requires the tag
	 * in a cpu register.
	 */
	LLVMPositionBuilderAtEnd(ctx->builder, ctx->reply_bb);
	ctx->reply_tag = LLVMBuildPhi(ctx->builder, ctx->wordt, "replytag.phi");
	LLVMBuildStore(ctx->builder, stored_timeouts, xfer_timeouts_addr);
	build_store_br(ctx, acceptor, 0);
	if(have_stringbufs) build_set_strbufs(ctx, stritems);
	/* TODO: these magic constants are actually L4_Timeouts(L4_Never,
	 * L4_Never), and L4_anythread.
	 */
	ipc_tag = build_l4_ipc_call(ctx,
		ctx->from, CONST_WORD(0), LLVMConstNot(CONST_WORD(0)),
		ctx->reply_tag, &ipc_from, &ipc_mr1, &ipc_mr2);
	LLVMAddIncoming(ctx->from, &ipc_from, &ctx->reply_bb, 1);
	LLVMAddIncoming(ctx->mr1, &ipc_mr1, &ctx->reply_bb, 1);
	LLVMAddIncoming(ctx->mr2, &ipc_mr2, &ctx->reply_bb, 1);
	LLVMAddIncoming(ctx->tag, &ipc_tag, &ctx->reply_bb, 1);
	LLVMBuildBr(ctx->builder, loop_bb);

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
	GList *tm_list = NULL;
	for(GList *cur = g_list_first(methods);
		cur != NULL;
		cur = g_list_next(cur))
	{
		struct method_info *inf = cur->data;
		if(inf->request->tagmask != NO_TAGMASK) {
			assert(inf->request->sublabel == NO_SUBLABEL);
			tm_list = g_list_prepend(tm_list, inf);
		}
	}
	tm_list = g_list_reverse(tm_list);
	LLVMBasicBlockRef tm_dispatch_bb = NULL;
	if(tm_list != NULL) {
		tm_dispatch_bb = LLVMAppendBasicBlockInContext(ctx->ctx,
			fn, "tagmask.dispatch");
	}

	/* dispatch according to ctx->tag. */
	LLVMPositionBuilderAtEnd(ctx->builder, dispatch_bb);
	/* update the skel context. */
	LLVMBuildStore(ctx->builder, ctx->from,
		LLVMBuildStructGEP(ctx->builder, supp_ctx_ptr,
			SUPP_LAST_SENDER_IX, "supp.last_sender.ptr"));
	LLVMBuildStore(ctx->builder, ctx->tag,
		LLVMBuildStructGEP(ctx->builder, supp_ctx_ptr,
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
		for(GList *cur = g_list_first(tm_list);
			cur != NULL;
			cur = g_list_next(cur))
		{
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
	LLVMValueRef unknownlabel = LLVMConstInt(ctx->wordt, 42666, 0);
	LLVMAddIncoming(retval, &unknownlabel, &current, 1);
	if(tm_dispatch_bb != NULL) {
		/* must branch explicitly. the non-bb path exits from the switch
		 * instruction.
		 */
		LLVMBuildBr(ctx->builder, exit_bb);
	}

	int top_label = -1;
	LLVMValueRef sublabelswitch = NULL;
	for(GList *cur = g_list_first(methods);
		cur != NULL;
		cur = g_list_next(cur))
	{
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
	g_list_foreach(methods, (GFunc)free_method_info, NULL);
	g_list_free(methods);
	g_list_free(tagmask_list);
	g_free(stritems);
	return fn;
}


gboolean iter_build_dispatchers(IDL_tree_func_data *tf, void *ud)
{
	struct llvm_ctx *ctx = ud;

	/* TODO: add extern functions to the module on first invocation,
	 * somehow
	 */

	switch(IDL_NODE_TYPE(tf->tree)) {
		case IDLN_LIST:
		case IDLN_MODULE:
		case IDLN_SRCFILE:
			return TRUE;

		default: return FALSE;

		case IDLN_INTERFACE:
			build_dispatcher_function(ctx, tf->tree);
			return FALSE;
	}
}
