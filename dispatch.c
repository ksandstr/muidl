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
			"=r,~{dirflag},~{fpsr},~{flags}", 0);
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
		1);
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


/* TODO: make this cache things in @ctx, and move it into a llvmutil.c or some
 * such
 */
static LLVMTypeRef get_vtable_type(struct llvm_ctx *ctx, IDL_tree iface)
{
	/* TODO: derive from the interface... */

	LLVMTypeRef assoc_fn_type = LLVMFunctionType(ctx->i32t, &ctx->i32t, 1, 0),
		fn_types[] = {
			LLVMPointerType(assoc_fn_type, 0),
			LLVMPointerType(assoc_fn_type, 0),
		};

	return LLVMStructTypeInContext(ctx->ctx, fn_types, 2, 0);
}


LLVMValueRef build_dispatcher_function(struct llvm_ctx *ctx, IDL_tree iface)
{
	LLVMTypeRef param = LLVMPointerType(get_vtable_type(ctx, iface), 0),
		fn_type = LLVMFunctionType(ctx->wordt, &param, 1, 0);

	char *dispname = dispatcher_name(ctx->ns, iface, NULL);
	LLVMValueRef fn = LLVMAddFunction(ctx->module, dispname, fn_type),
		vtab_arg = LLVMGetFirstParam(fn);
	g_free(dispname);

	ctx->builder = LLVMCreateBuilderInContext(ctx->ctx);
	LLVMBasicBlockRef bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "EntryBlock"),
		loop_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "loop"),
		exit_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "exit"),
		ret_ec_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "ret_errcode"),
		dispatch_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "dispatch"),
		reply_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "reply"),
		msgerr_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "msgerr"),
		decode_assoc_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "decode.associate"),
		decode_deassoc_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "decode.deassociate");

	LLVMPositionBuilderAtEnd(ctx->builder, bb);
	LLVMValueRef utcb = build_utcb_get(ctx),
		xfer_timeouts_addr = build_utcb_address(ctx, utcb, -32),
		stored_timeouts = LLVMBuildLoad(ctx->builder,
			xfer_timeouts_addr, "stored_timeouts"),
		acceptor = LLVMConstInt(ctx->i32t, 0, 0);
	LLVMBuildStore(ctx->builder, acceptor,
		build_utcb_address(ctx, utcb, -64));
	LLVMBuildStore(ctx->builder, stored_timeouts, xfer_timeouts_addr);
	LLVMValueRef ipc_from, ipc_mr1, ipc_mr2,
		ipc_tag = build_l4_ipc_call(ctx, utcb,
			ctx->zero, LLVMConstNot(ctx->zero), LLVMConstNot(ctx->zero), ctx->zero,
			&ipc_from, &ipc_mr1, &ipc_mr2);
	LLVMBuildBr(ctx->builder, loop_bb);

	/* the main dispatch-replywait loop. */
	LLVMPositionBuilderAtEnd(ctx->builder, loop_bb);
	LLVMValueRef from_phi = LLVMBuildPhi(ctx->builder, ctx->wordt, "from.phi"),
		mr1_phi = LLVMBuildPhi(ctx->builder, ctx->wordt, "mr1.phi"),
		mr2_phi = LLVMBuildPhi(ctx->builder, ctx->wordt, "mr2.phi"),
		tag_phi = LLVMBuildPhi(ctx->builder, ctx->wordt, "tag.phi");
	LLVMAddIncoming(from_phi, &ipc_from, &bb, 1);
	LLVMAddIncoming(mr1_phi, &ipc_mr1, &bb, 1);
	LLVMAddIncoming(mr2_phi, &ipc_mr2, &bb, 1);
	LLVMAddIncoming(tag_phi, &ipc_tag, &bb, 1);
	LLVMBuildCondBr(ctx->builder, build_ipcfailed_cond(ctx, tag_phi),
		ret_ec_bb, dispatch_bb);

	/* send reply, receive next message. */
	/* message registers were already loaded, since ia32 only requires the tag
	 * in a cpu register.
	 */
	LLVMPositionBuilderAtEnd(ctx->builder, reply_bb);
	/* TODO: compute the right reply tag. */
	LLVMValueRef reply_tag_phi = LLVMBuildPhi(ctx->builder, ctx->wordt,
		"replytag.phi");
	ipc_tag = build_l4_ipc_call(ctx, utcb,
		from_phi, LLVMConstNot(ctx->zero), LLVMConstNot(ctx->zero),
		reply_tag_phi, &ipc_from, &ipc_mr1, &ipc_mr2);
	LLVMAddIncoming(from_phi, &ipc_from, &reply_bb, 1);
	LLVMAddIncoming(mr1_phi, &ipc_mr1, &reply_bb, 1);
	LLVMAddIncoming(mr2_phi, &ipc_mr2, &reply_bb, 1);
	LLVMAddIncoming(tag_phi, &ipc_tag, &reply_bb, 1);
	LLVMBuildBr(ctx->builder, loop_bb);

	/* exit */
	LLVMPositionBuilderAtEnd(ctx->builder, exit_bb);
	LLVMValueRef retval = LLVMBuildPhi(ctx->builder, ctx->wordt, "retval");
	LLVMBuildRet(ctx->builder, retval);

	/* return L4_ErrorCode(); */
	LLVMPositionBuilderAtEnd(ctx->builder, ret_ec_bb);
	LLVMValueRef errorcode = LLVMBuildLoad(ctx->builder,
		build_utcb_address(ctx, utcb, -36), "errcode");
	LLVMAddIncoming(retval, &errorcode, &ret_ec_bb, 1);
	LLVMBuildBr(ctx->builder, exit_bb);

	/* dispatch according to tag_phi. */
	LLVMPositionBuilderAtEnd(ctx->builder, dispatch_bb);
	/* FIXME: get the correct value */
	LLVMValueRef labelswitch = LLVMBuildSwitch(ctx->builder,
		build_label_from_tag(ctx, tag_phi), exit_bb, 2);
	LLVMValueRef unknownlabel = LLVMConstInt(ctx->wordt, 42666, 0);
	LLVMAddIncoming(retval, &unknownlabel, &dispatch_bb, 1);
	LLVMAddCase(labelswitch, LLVMConstInt(ctx->wordt, 0xf0ea, 0), decode_assoc_bb);
	LLVMAddCase(labelswitch, LLVMConstInt(ctx->wordt, 0xf0eb, 0), decode_deassoc_bb);

	/* decode associate(). */
	LLVMPositionBuilderAtEnd(ctx->builder, decode_assoc_bb);
	LLVMValueRef arg0 = mr1_phi,
		fnptr = LLVMBuildLoad(ctx->builder,
			LLVMBuildStructGEP(ctx->builder, vtab_arg, 0, "assoc.offs"),
			"assoc.fnptr"),
		fncall = LLVMBuildCall(ctx->builder, fnptr, &arg0, 1, "assoc.call"),
		ok_cond = LLVMBuildICmp(ctx->builder, LLVMIntSGE, fncall,
			LLVMConstInt(ctx->i32t, 0, 1), "rcneg.cond");
	LLVMAddIncoming(reply_tag_phi, &ctx->zero, &decode_assoc_bb, 1);
	LLVMBuildCondBr(ctx->builder, ok_cond, reply_bb, msgerr_bb);

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
	LLVMBuildCondBr(ctx->builder, ok_cond, reply_bb, msgerr_bb);

	/* send a MSG_ERROR. */
	LLVMPositionBuilderAtEnd(ctx->builder, msgerr_bb);
	LLVMValueRef msgerr_tag = LLVMBuildOr(ctx->builder,
		LLVMConstInt(ctx->wordt, 1, 0), LLVMConstInt(ctx->wordt, 1 << 16, 0),
		"msgerr.tag");
	LLVMAddIncoming(reply_tag_phi, &msgerr_tag, &msgerr_bb, 1);
	LLVMBuildBr(ctx->builder, reply_bb);

	LLVMDisposeBuilder(ctx->builder);
	ctx->builder = NULL;
	return fn;
}
