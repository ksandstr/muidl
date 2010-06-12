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


/* dst should have two LLVMTypeRefs' worth of space to allow for sequences
 * (pointer + length) and split longlongs on 32-bit architectures (low half,
 * high half).
 */
static void vtable_in_param_type(
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


static void vtable_out_param_type(
	struct llvm_ctx *ctx,
	LLVMTypeRef *dst,
	int *pos_p,
	IDL_tree type)
{
	if(is_value_type(type)) {
		dst[(*pos_p)++] = LLVMPointerType(llvm_value_type(type), 0);
	} else {
		printf("warning: not emitting llvm out-parameter for <%s>\n",
			IDL_NODE_TYPE_NAME(type));
	}
}


/* TODO: wrap this to cache things in @ctx, and move it into a llvmutil.c or
 * some such
 */
static LLVMTypeRef get_vtable_type(struct llvm_ctx *ctx, IDL_tree iface)
{
	GList *methods = all_methods_of_iface(ctx->ns, iface);
	int num_fields = g_list_length(methods), f_offs = 0;
	LLVMTypeRef field_types[num_fields];
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
			vtable_out_param_type(ctx, param_types, &p_pos, rettyp);
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
					vtable_in_param_type(ctx, param_types, &p_pos, ptype);
					break;

				/* inout parameters are passed exactly like out-parameters, but
				 * with a value already present.
				 */
				case IDL_PARAM_OUT:
				case IDL_PARAM_INOUT:
					vtable_out_param_type(ctx, param_types, &p_pos, ptype);
					break;
			}
		}

		field_types[f_offs++] = LLVMPointerType(
			LLVMFunctionType(ctx->i32t, param_types, p_pos, 0), 0);
	}
	assert(f_offs == num_fields);
	g_list_free(methods);

	return LLVMStructTypeInContext(ctx->ctx, field_types, num_fields, 0);
}


static LLVMValueRef build_ipc_input_val(struct llvm_ctx *ctx, int mr)
{
	if(mr == 0) return ctx->tag;
	else if(mr == 1) return ctx->mr1;
	else if(mr == 2) return ctx->mr2;
	else {
		return LLVMBuildLoad(ctx->builder,
			build_utcb_address(ctx, ctx->utcb, mr * 4),
			tmp_f(ctx->pr, "mr%d", mr));
	}
}


static struct untyped_param *find_untyped(
	const struct message_info *msg,
	IDL_tree node)
{
	for(int i=0; i<msg->num_untyped; i++) {
		if(msg->untyped[i]->param_dcl == node) {
			return msg->untyped[i];
		}
	}
	return NULL;
}


static void emit_in_param(
	struct llvm_ctx *ctx,
	LLVMValueRef *args,
	int *arg_pos_p,
	const struct method_info *inf,
	IDL_tree pdecl)
{
	/* FIXME: repair the untyped/seq/long param thing; they should not
	 * be flattened out by kind like that.
	 */
	struct untyped_param *u = find_untyped(inf->request, pdecl);
	if(u != NULL) {
		LLVMTypeRef typ = llvm_value_type(u->type);
		assert(LLVMGetTypeKind(typ) == LLVMIntegerTypeKind);
		/* integer types. */
		if(LLVMGetIntTypeWidth(typ) <= LLVMGetIntTypeWidth(ctx->wordt)) {
			LLVMValueRef reg = build_ipc_input_val(ctx, u->first_reg);
			if(IDL_TYPE_INTEGER(u->type).f_signed) {
				/* signed cast. */
				reg = LLVMBuildIntCast(ctx->builder, reg, typ,
					"intparm.cast.sign");
			} else {
				reg = LLVMBuildTruncOrBitCast(ctx->builder, reg, typ,
					"bitparm.cast.nosign");
			}
			args[(*arg_pos_p)++] = reg;
		} else {
			/* unpack a two-word parameter. */
			LLVMValueRef low = build_ipc_input_val(ctx, u->first_reg),
				high = build_ipc_input_val(ctx, u->first_reg + 1);
			assert(u->last_reg == u->first_reg + 1);
			/* FIXME: stash this in the context */
			LLVMTypeRef i64t = LLVMInt64TypeInContext(ctx->ctx);
			low = LLVMBuildBitCast(ctx->builder, low, i64t,
				"longparm.lo.cast");
			high = LLVMBuildBitCast(ctx->builder, high, i64t,
				"longparm.hi.cast");
			args[(*arg_pos_p)++] = LLVMBuildOr(ctx->builder, low,
				LLVMBuildShl(ctx->builder, high, LLVMConstInt(i64t, 32, 0),
					"longparm.hi.shift"),
				"longparm.value");
		}
		return;
	}

	printf("can't hack seq/long in-parameter\n");
	abort();
}


static void emit_out_param(
	struct llvm_ctx *ctx,
	LLVMValueRef *args,
	int *arg_pos_p,
	IDL_tree ptyp)
{
	if(is_value_type(ptyp)) {
		LLVMValueRef memptr = LLVMBuildAlloca(ctx->builder,
			llvm_value_type(ptyp), "outparam.mem");
		args[(*arg_pos_p)++] = memptr;
	} else {
		printf("can't hack seq/long out-parameter\n");
		abort();
	}
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

	/* collection of arguments according to op decl. */
	const struct message_info *req = inf->request;
	const int num_args_max = 1 + req->num_untyped
		+ req->num_inline_seq * 2 + req->num_long * 2;
	LLVMValueRef args[num_args_max], retvalptr = NULL;
	int arg_pos = 0;
	if(inf->return_type != NULL) {
		/* a parameter for the return value. */
		emit_out_param(ctx, args, &arg_pos, inf->return_type);
		if(arg_pos > 0) retvalptr = args[0];
	}
	for(IDL_tree cur = IDL_OP_DCL(inf->node).parameter_dcls;
		cur != NULL;
		cur = IDL_LIST(cur).next)
	{
		IDL_tree p = IDL_LIST(cur).data;
		enum IDL_param_attr attr = IDL_PARAM_DCL(p).attr;
		if(attr == IDL_PARAM_IN) {
			emit_in_param(ctx, args, &arg_pos, inf, p);
		} else if(attr == IDL_PARAM_OUT) {
			emit_out_param(ctx, args, &arg_pos,
				get_type_spec(IDL_PARAM_DCL(p).param_type_spec));
		} else /* inout */ {
			/* hax! */
			int start = arg_pos;
			emit_out_param(ctx, args, &arg_pos,
				get_type_spec(IDL_PARAM_DCL(p).param_type_spec));
			LLVMValueRef in_args[2];
			int in_arg_pos = 0;
			emit_in_param(ctx, in_args, &in_arg_pos, inf, p);
			assert(in_arg_pos == arg_pos - start);
			/* insert tab A in slot B */
			for(int i=0; i<in_arg_pos; i++) {
				LLVMBuildStore(ctx->builder, in_args[i], args[start + i]);
			}
		}
	}
	assert(arg_pos >= IDL_list_length(IDL_OP_DCL(inf->node).parameter_dcls));

	/* the function call. */
	LLVMValueRef fnptr = LLVMBuildLoad(ctx->builder,
		LLVMBuildStructGEP(ctx->builder, ctx->vtab_arg, inf->vtab_offset,
				tmp_f(pr, "%s.offs", opname)),
			tmp_f(pr, "%s.fnptr", opname));
	LLVMValueRef fncall = LLVMBuildCall(ctx->builder, fnptr,
		args, arg_pos, tmp_f(pr, "%s.call", opname));
	LLVMAddIncoming(ctx->fncall_phi, &fncall, &bb, 1);

	/* test for negative return value. */
	LLVMValueRef ok_cond = LLVMBuildICmp(ctx->builder, LLVMIntSGE, fncall,
		LLVMConstInt(ctx->i32t, 0, 1), "rcneg.cond");
	if(inf->num_reply_msgs == 0) {
		/* oneway void messages don't have a reply part. */
		LLVMBuildCondBr(ctx->builder, ok_cond, ctx->wait_bb, ctx->msgerr_bb);
		return bb;	/* also we're done! */
	}

	if(inf->num_reply_msgs > 0
		&& IDL_NODE_TYPE(inf->replies[0]->node) == IDLN_OP_DCL)
	{
		/* return value, out-parameters, out-halves of inout parameters */
		LLVMBasicBlockRef pr_bb = LLVMAppendBasicBlockInContext(ctx->ctx,
			function, tmp_f(pr, "%s.packreply", opname));
		LLVMBuildCondBr(ctx->builder, ok_cond, pr_bb, ctx->msgerr_bb);
		LLVMPositionBuilderAtEnd(ctx->builder, pr_bb);
		int mr_pos = 1;
		if(retvalptr != NULL) {
			LLVMValueRef val = LLVMBuildLoad(ctx->builder,
				retvalptr, tmp_f(pr, "%s.retval", opname));
			val = LLVMBuildBitCast(ctx->builder, val, ctx->wordt,
				tmp_f(pr, "%s.retval.word", opname));
			LLVMBuildStore(ctx->builder, val,
				build_utcb_address(ctx, ctx->utcb, mr_pos * 4));
			mr_pos++;
		}
		/* TODO: out parameters */
		/* label 0, t 0, u = mr_pos - 1 */
		LLVMValueRef tag = LLVMConstInt(ctx->wordt, mr_pos - 1, 0);
		LLVMAddIncoming(ctx->reply_tag, &tag, &pr_bb, 1);
		LLVMBuildBr(ctx->builder, ctx->reply_bb);
	}
	/* TODO: exceptions! */

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
	ctx->wait_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "wait");
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
	LLVMBuildBr(ctx->builder, ctx->wait_bb);

	LLVMPositionBuilderAtEnd(ctx->builder, ctx->wait_bb);
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

	/* send a MSG_ERROR. */
	LLVMPositionBuilderAtEnd(ctx->builder, ctx->msgerr_bb);
	ctx->fncall_phi = LLVMBuildPhi(ctx->builder, ctx->i32t, "fncall.phi");
	LLVMValueRef msgerr_tag = LLVMBuildOr(ctx->builder,
		LLVMConstInt(ctx->wordt, 1, 0), LLVMConstInt(ctx->wordt, 1 << 16, 0),
		"msgerr.tag");
	LLVMBuildStore(ctx->builder,
		LLVMBuildNeg(ctx->builder, ctx->fncall_phi, "rcneg.val"),
		build_utcb_address(ctx, ctx->utcb, 4));	/* mr0 on 32-bit systems */
	LLVMAddIncoming(ctx->reply_tag, &msgerr_tag, &ctx->msgerr_bb, 1);
	LLVMBuildBr(ctx->builder, ctx->reply_bb);

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

	LLVMDisposeBuilder(ctx->builder);
	ctx->builder = NULL;
	g_list_foreach(methods, (GFunc)free_method_info, NULL);
	g_list_free(methods);
	g_list_free(tagmask_list);
	return fn;
}
