/*
 * dispatch.c -- emit a LLVM tree for an IDL interface
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

#include <llvm-c/Core.h>
#include <libIDL/IDL.h>

#include "muidl.h"


static LLVMValueRef build_utcb_get(struct llvm_ctx *ctx)
{
	LLVMTypeRef fntype = LLVMFunctionType(ctx->voidptrt, NULL, 0, 0);
	LLVMValueRef func = LLVMConstInlineAsm(fntype, "movl %gs:0,$0\n",
			"=r,~{dirflag},~{fpsr},~{flags}", 0, 0);
	LLVMValueRef call = LLVMBuildCall(ctx->builder, func, NULL, 0, "utcbget");
	LLVMSetTailCall(call, 1);
	return call;
}


/* return node is L4_Word_t mr0 (i.e. message tag). */
static LLVMValueRef build_l4_ipc_call(
	struct llvm_ctx *ctx,
	LLVMValueRef utcb_ptr,
	LLVMValueRef arg_to,
	LLVMValueRef arg_timeouts,
	LLVMValueRef arg_fromspec,
	LLVMValueRef arg_mr0,
	LLVMValueRef *from_p,
	LLVMValueRef *mr1_p,
	LLVMValueRef *mr2_p)
{
	/* FIXME: get word type from target somehow */
	LLVMTypeRef params[5], wordtype = LLVMInt32TypeInContext(ctx->ctx);
	for(int i=0; i<5; i++) params[i] = wordtype;
	LLVMTypeRef ipc_result_type = LLVMStructTypeInContext(ctx->ctx,
		params, 4, 0);
	LLVMTypeRef ipc_type = LLVMFunctionType(ipc_result_type,
		params, 5, 0);
	LLVMValueRef fn = LLVMConstInlineAsm(ipc_type,
		"call __L4_Ipc\n",
		"={ax},={si},={bx},={bp},{ax},{cx},{dx},{si},{di},~{dirflag},~{fpsr},~{flags}",
		1, 0);
	LLVMValueRef args[5] = {
		arg_to, arg_timeouts, arg_fromspec, arg_mr0, utcb_ptr
	};
	LLVMValueRef result = LLVMBuildCall(ctx->builder, fn, args, 5, "l4ipc");
	LLVMSetTailCall(result, 1);

	if(from_p != NULL) {
		*from_p = LLVMBuildExtractValue(ctx->builder, result, 0, "from");
	}
	if(mr1_p != NULL) {
		*mr1_p = LLVMBuildExtractValue(ctx->builder, result, 2, "mr1");
	}
	if(mr2_p != NULL) {
		*mr2_p = LLVMBuildExtractValue(ctx->builder, result, 3, "mr2");
	}
	return LLVMBuildExtractValue(ctx->builder, result, 1, "mr0");
}


static LLVMValueRef build_utcb_address(
	struct llvm_ctx *ctx,
	LLVMValueRef utcb,
	int offset)
{
	LLVMValueRef off = LLVMConstInt(ctx->wordt, abs(offset), 0);
	char trail[16], offname[32], addrname[32], wordptrname[32];
	snprintf(trail, sizeof(trail), ".%c%d", offset < 0 ? 'm' : 'p', abs(offset));
	snprintf(offname, sizeof(offname), "utcb.offset%s", trail);
	snprintf(addrname, sizeof(addrname), "utcb.addr%s", trail);
	snprintf(wordptrname, sizeof(wordptrname), "utcb.addr.wordptr%s", trail);
	if(offset < 0) off = LLVMBuildNeg(ctx->builder, off, offname);
	return LLVMBuildPointerCast(ctx->builder,
			LLVMBuildGEP(ctx->builder, utcb, &off, 1, addrname),
			LLVMPointerType(ctx->wordt, 0), wordptrname);
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


static LLVMValueRef build_label_from_tag(
	struct llvm_ctx *ctx,
	LLVMValueRef mr0)
{
	return LLVMBuildLShr(ctx->builder,
		LLVMBuildAnd(ctx->builder, mr0,
			LLVMConstInt(ctx->wordt, 0xffff0000, 0), "label.shifted"),
		LLVMConstInt(ctx->wordt, 16, 0),
		"label");
}


static LLVMValueRef build_u_from_tag(
	struct llvm_ctx *ctx,
	LLVMValueRef mr0)
{
	return LLVMBuildAnd(ctx->builder, mr0,
		LLVMConstInt(ctx->wordt, 0x3f, 0), "tag.u");
}


static LLVMValueRef build_t_from_tag(
	LLVMBuilderRef builder,
	LLVMTypeRef wordtype,
	LLVMValueRef mr0)
{
	return LLVMBuildAnd(builder,
			LLVMConstInt(wordtype, 0x3f, 0),
			LLVMBuildLShr(builder, mr0, LLVMConstInt(wordtype, 6, 0),
				"tag.u.raw"),
			"tag.u");
}


/* FIXME: move this into a llvmutil.c or some such
 * FIXME: also get the LLVM context from somewhere.
 */
static LLVMTypeRef llvm_value_type(IDL_tree type)
{
	if(type == NULL) return LLVMVoidType();
	switch(IDL_NODE_TYPE(type)) {
		case IDLN_TYPE_INTEGER: {
			static short bitlens[] = {
				[IDL_INTEGER_TYPE_SHORT] = 16,
				[IDL_INTEGER_TYPE_LONG] = 32,
				[IDL_INTEGER_TYPE_LONGLONG] = 64,
			};
			int t = IDL_TYPE_INTEGER(type).f_type;
			assert(t < G_N_ELEMENTS(bitlens));
			/* NOTE: discards signedness info */
			return LLVMIntType(bitlens[t]);
		}

		case IDLN_NATIVE: {
			/* each of these is the size of a single word, which is all LLVM
			 * wants to know.
			 */
			if(IS_WORD_TYPE(type) || IS_FPAGE_TYPE(type)
				|| IS_TIME_TYPE(type))
			{
				/* FIXME: get word size from arch spec! */
				return LLVMIntType(32);
			} else {
				fprintf(stderr, "%s: native type `%s' not supported\n",
					__FUNCTION__, NATIVE_NAME(type));
				exit(EXIT_FAILURE);
			}
			break;
		}

		case IDLN_TYPE_FLOAT:
			switch(IDL_TYPE_FLOAT(type).f_type) {
				case IDL_FLOAT_TYPE_FLOAT: return LLVMFloatType();
				case IDL_FLOAT_TYPE_DOUBLE: return LLVMDoubleType();
				case IDL_FLOAT_TYPE_LONGDOUBLE: return LLVMFP128Type();
			}
			g_assert_not_reached();

		case IDLN_TYPE_BOOLEAN:
		case IDLN_TYPE_OCTET:
		case IDLN_TYPE_CHAR:
			return LLVMIntType(8);
		case IDLN_TYPE_WIDE_CHAR:
			return LLVMIntType(32);

		/* FIXME: should be the native int type */
		case IDLN_TYPE_ENUM: return LLVMIntType(32);

		default:
			NOTDEFINED(type);
	}
}


static void emit_in_parameter(
	struct llvm_ctx *ctx,
	LLVMTypeRef *dst,
	int *pos_p,
	IDL_tree type)
{
	if(is_value_type(type)) {
		dst[(*pos_p)++] = llvm_value_type(type);
	} else if(is_rigid_type(ctx->ns, type)) {
		/* FIXME: handle structs, arrays, unions */
		NOTDEFINED(type);
	} else {
		/* FIXME: handle sequences, strings, wide strings */
		NOTDEFINED(type);
	}
}


static void emit_out_parameter(
	struct llvm_ctx *ctx,
	LLVMTypeRef *dst,
	int *pos_p,
	IDL_tree type)
{
	printf("warning: not emitting llvm out-parameter for <%s>\n",
		IDL_NODE_TYPE_NAME(type));
}


/* TODO: wrap this to cache things in @ctx, and move it into a llvmutil.c or
 * some such
 */
static LLVMTypeRef get_vtable_type(struct llvm_ctx *ctx, IDL_tree iface)
{
	GList *methods = all_methods_of_iface(ctx->ns, iface);
	for(GList *cur = g_list_first(methods);
		cur != NULL;
		cur = g_list_next(cur))
	{
		IDL_tree op = cur->data,
			rettyp = get_type_spec(IDL_OP_DCL(op).op_type_spec),
			param_list = IDL_OP_DCL(op).parameter_dcls;
		/* each can be an out sequence, the return value too */
		const int n_params_max = IDL_list_length(param_list) * 2
			+ (rettyp != NULL ? 2 : 0);
		LLVMTypeRef param_types[n_params_max];
		int p_pos = 0;
		if(rettyp != NULL) {
			emit_out_parameter(ctx, param_types, &p_pos, rettyp);
		}
		for(IDL_tree p_cur = param_list;
			p_cur != NULL;
			p_cur = IDL_LIST(p_cur).next)
		{
			assert(p_pos < n_params_max);
			IDL_tree pdecl = IDL_LIST(p_cur).data,
				ptype = get_type_spec(IDL_PARAM_DCL(pdecl).param_type_spec);
			switch(IDL_PARAM_DCL(pdecl).attr) {
				case IDL_PARAM_IN:
					emit_in_parameter(ctx, param_types, &p_pos, ptype);
					break;

				/* inout parameters are passed exactly like out-parameters, but
				 * with a value already present.
				 */
				case IDL_PARAM_OUT:
				case IDL_PARAM_INOUT:
					emit_out_parameter(ctx, param_types, &p_pos, ptype);
					break;
			}
		}
	}
	g_list_free(methods);

	LLVMTypeRef assoc_fn_type = LLVMFunctionType(ctx->i32t, &ctx->i32t, 1, 0),
		fn_types[] = {
			LLVMPointerType(assoc_fn_type, 0),
			LLVMPointerType(assoc_fn_type, 0),
		};

	return LLVMStructTypeInContext(ctx->ctx, fn_types, 2, 0);
}


static LLVMBasicBlockRef build_op_decode(
	struct llvm_ctx *ctx,
	LLVMValueRef function,
	const struct method_info *inf)
{
	struct print_ctx *pr = ctx->pr;
	char *name = IDL_NODE_TYPE(inf->node) == IDLN_OP_DCL
		? decapsify(IDL_IDENT(IDL_OP_DCL(inf->node).ident).str)
		: decapsify(IDL_IDENT(IDL_EXCEPT_DCL(inf->node).ident).str);
	char *opname = g_strdup_printf("decode.%s", name);
	LLVMBasicBlockRef bb = LLVMAppendBasicBlockInContext(ctx->ctx,
		function, opname);
	g_free(name);

	/* (TODO: we're allowed to do this, right?) */
	LLVMPositionBuilderAtEnd(ctx->builder, bb);

	/* collection of arguments.
	 *
	 * FIXME: this only does the "in" half. the vtable prototype has arguments
	 * for "out" halves as well.
	 */
	const struct message_info *req = inf->request;
	int num_args = req->num_untyped + req->num_inline_seq * 2
		+ req->num_long * 2, arg_pos = 0;
	LLVMValueRef args[num_args];
	for(int i=0; i < req->num_untyped; i++) {
		const struct untyped_param *u = req->untyped[i];
		assert(is_value_type(u->type));
		/* simple types are carried in a single argument. */
		LLVMTypeRef vt = llvm_value_type(u->type);
		int mr = u->first_reg;
		assert(u->first_reg == u->last_reg);
		LLVMValueRef reg;
		if(mr == 1) reg = ctx->mr1;
		else if(mr == 2) reg = ctx->mr2;
		else {
			reg = LLVMBuildLoad(ctx->builder,
				build_utcb_address(ctx, ctx->utcb, mr * 4),
				tmp_f(pr, "mr%d", mr));
		}
		args[arg_pos++] = LLVMBuildIntCast(ctx->builder, reg, vt,
			tmp_f(pr, "arg.u%d", i));
	}

	/* the function call.
	 *
	 * FIXME: get vtable offset somehow!
	 */
	LLVMValueRef fnptr = LLVMBuildLoad(ctx->builder,
		LLVMBuildStructGEP(ctx->builder, ctx->vtab_arg, 0,
				tmp_f(pr, "%s.offs", opname)),
			tmp_f(pr, "%s.fnptr", opname));
	LLVMValueRef fncall = LLVMBuildCall(ctx->builder, fnptr,
		args, arg_pos, tmp_f(pr, "%s.call", opname));
	LLVMValueRef ok_cond = LLVMBuildICmp(ctx->builder, LLVMIntSGE, fncall,
		LLVMConstInt(ctx->i32t, 0, 1), "rcneg.cond");

	/* FIXME: load reply registers in UTCB */

	LLVMAddIncoming(ctx->reply_tag, &ctx->zero, &bb, 1);
	LLVMBuildCondBr(ctx->builder, ok_cond, ctx->reply_bb, ctx->msgerr_bb);

	return bb;
}


LLVMValueRef build_dispatcher_function(struct llvm_ctx *ctx, IDL_tree iface)
{
	GList *tagmask_list = NULL,
		*methods = analyse_methods_of_iface(ctx->pr, &tagmask_list, iface);

	LLVMTypeRef param = LLVMPointerType(get_vtable_type(ctx, iface), 0),
		fn_type = LLVMFunctionType(ctx->wordt, &param, 1, 0);

	char *dispname = dispatcher_name(ctx->ns, iface, NULL);
	LLVMValueRef fn = LLVMAddFunction(ctx->module, dispname, fn_type);
	ctx->vtab_arg = LLVMGetFirstParam(fn);
	g_free(dispname);

	ctx->builder = LLVMCreateBuilderInContext(ctx->ctx);
	LLVMBasicBlockRef bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "EntryBlock"),
		loop_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "loop"),
		exit_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "exit"),
		ret_ec_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "ret_errcode"),
		dispatch_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "dispatch");
	ctx->reply_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "reply");
	ctx->msgerr_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "msgerr");

	LLVMPositionBuilderAtEnd(ctx->builder, bb);
	ctx->utcb = build_utcb_get(ctx);
	LLVMValueRef xfer_timeouts_addr = build_utcb_address(ctx, ctx->utcb, -32),
		stored_timeouts = LLVMBuildLoad(ctx->builder,
			xfer_timeouts_addr, "stored_timeouts"),
		acceptor = LLVMConstInt(ctx->i32t, 0, 0);
	LLVMBuildStore(ctx->builder, acceptor,
		build_utcb_address(ctx, ctx->utcb, -64));
	LLVMBuildStore(ctx->builder, stored_timeouts, xfer_timeouts_addr);
	LLVMValueRef ipc_from, ipc_mr1, ipc_mr2,
		ipc_tag = build_l4_ipc_call(ctx, ctx->utcb,
			ctx->zero, LLVMConstNot(ctx->zero), LLVMConstNot(ctx->zero), ctx->zero,
			&ipc_from, &ipc_mr1, &ipc_mr2);
	LLVMBuildBr(ctx->builder, loop_bb);

	/* the main dispatch-replywait loop. */
	LLVMPositionBuilderAtEnd(ctx->builder, loop_bb);
	ctx->from = LLVMBuildPhi(ctx->builder, ctx->wordt, "from.phi");
	ctx->mr1 = LLVMBuildPhi(ctx->builder, ctx->wordt, "mr1.phi");
	ctx->mr2 = LLVMBuildPhi(ctx->builder, ctx->wordt, "mr2.phi");
	ctx->tag = LLVMBuildPhi(ctx->builder, ctx->wordt, "tag.phi");
	LLVMAddIncoming(ctx->from, &ipc_from, &bb, 1);
	LLVMAddIncoming(ctx->mr1, &ipc_mr1, &bb, 1);
	LLVMAddIncoming(ctx->mr2, &ipc_mr2, &bb, 1);
	LLVMAddIncoming(ctx->tag, &ipc_tag, &bb, 1);
	LLVMBuildCondBr(ctx->builder, build_ipcfailed_cond(ctx, ctx->tag),
		ret_ec_bb, dispatch_bb);

	/* send reply, receive next message. */
	/* message registers were already loaded, since ia32 only requires the tag
	 * in a cpu register.
	 */
	LLVMPositionBuilderAtEnd(ctx->builder, ctx->reply_bb);
	ctx->reply_tag = LLVMBuildPhi(ctx->builder, ctx->wordt, "replytag.phi");
	ipc_tag = build_l4_ipc_call(ctx, ctx->utcb,
		ctx->from, LLVMConstNot(ctx->zero), LLVMConstNot(ctx->zero),
		ctx->reply_tag, &ipc_from, &ipc_mr1, &ipc_mr2);
	LLVMAddIncoming(ctx->from, &ipc_from, &ctx->reply_bb, 1);
	LLVMAddIncoming(ctx->mr1, &ipc_mr1, &ctx->reply_bb, 1);
	LLVMAddIncoming(ctx->mr2, &ipc_mr2, &ctx->reply_bb, 1);
	LLVMAddIncoming(ctx->tag, &ipc_tag, &ctx->reply_bb, 1);
	LLVMBuildBr(ctx->builder, loop_bb);

	/* exit */
	LLVMPositionBuilderAtEnd(ctx->builder, exit_bb);
	LLVMValueRef retval = LLVMBuildPhi(ctx->builder, ctx->wordt, "retval");
	LLVMBuildRet(ctx->builder, retval);

	/* return L4_ErrorCode(); */
	LLVMPositionBuilderAtEnd(ctx->builder, ret_ec_bb);
	LLVMValueRef errorcode = LLVMBuildLoad(ctx->builder,
		build_utcb_address(ctx, ctx->utcb, -36), "errcode");
	LLVMAddIncoming(retval, &errorcode, &ret_ec_bb, 1);
	LLVMBuildBr(ctx->builder, exit_bb);

	/* dispatch according to ctx->tag. */
	LLVMPositionBuilderAtEnd(ctx->builder, dispatch_bb);
	/* FIXME: get the correct value */
	LLVMValueRef labelswitch = LLVMBuildSwitch(ctx->builder,
		build_label_from_tag(ctx, ctx->tag), exit_bb, 2);
	LLVMValueRef unknownlabel = LLVMConstInt(ctx->wordt, 42666, 0);
	LLVMAddIncoming(retval, &unknownlabel, &dispatch_bb, 1);

	/* FIXME: support for tag-mask labelled operations, such as for the L4.X2
	 * pager protocol
	 */
	for(GList *cur = g_list_first(methods);
		cur != NULL;
		cur = g_list_next(cur))
	{
		struct method_info *inf = cur->data;
		/* FIXME: handle sublabels! */
		LLVMBasicBlockRef decode_bb = build_op_decode(ctx, fn, inf);
		LLVMAddCase(labelswitch,
			LLVMConstInt(ctx->wordt, inf->request->label, 0),
			decode_bb);
	}

#if !1
	/* decode deassociate(). */
	LLVMPositionBuilderAtEnd(ctx->builder, decode_deassoc_bb);
	arg0 = mr1_phi;
	fnptr = LLVMBuildLoad(ctx->builder,
		LLVMBuildStructGEP(ctx->builder, vtab_arg, 1, "deassoc.offs"),
		"deassoc.fnptr");
	fncall = LLVMBuildCall(ctx->builder, fnptr, &arg0, 1, "deassoc.call");
	ok_cond = LLVMBuildICmp(ctx->builder, LLVMIntSGE, fncall,
		LLVMConstInt(ctx->i32t, 0, 1), "rcneg.cond");
	LLVMAddIncoming(reply_tag_phi, &ctx->zero, &decode_deassoc_bb, 1);
	LLVMBuildCondBr(ctx->builder, ok_cond, ctx->reply_bb, msgerr_bb);
#endif

	/* send a MSG_ERROR. */
	LLVMPositionBuilderAtEnd(ctx->builder, ctx->msgerr_bb);
	LLVMValueRef msgerr_tag = LLVMBuildOr(ctx->builder,
		LLVMConstInt(ctx->wordt, 1, 0), LLVMConstInt(ctx->wordt, 1 << 16, 0),
		"msgerr.tag");
	LLVMAddIncoming(ctx->reply_tag, &msgerr_tag, &ctx->msgerr_bb, 1);
	LLVMBuildBr(ctx->builder, ctx->reply_bb);

	LLVMDisposeBuilder(ctx->builder);
	ctx->builder = NULL;
	g_list_foreach(methods, (GFunc)free_method_info, NULL);
	g_list_free(methods);
	g_list_free(tagmask_list);
	return fn;
}
