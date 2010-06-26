/*
 * l4x2.c -- utility functions related to the L4.X2 microkernel
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

#include <llvm-c/Core.h>

#include "muidl.h"


LLVMValueRef build_utcb_get(struct llvm_ctx *ctx)
{
	LLVMTypeRef fntype = LLVMFunctionType(ctx->wordt, NULL, 0, 0);
	LLVMValueRef func = LLVMConstInlineAsm(fntype, "movl %gs:0,$0\n",
			"=r,~{dirflag},~{fpsr},~{flags}", 0, 0);
	LLVMValueRef call = LLVMBuildCall(ctx->builder, func, NULL, 0, "utcbget");
	LLVMSetTailCall(call, 1);
	return LLVMBuildIntToPtr(ctx->builder, call,
		LLVMPointerType(ctx->wordt, 0), "utcb.wordp");
}


LLVMValueRef build_l4_ipc_call(
	struct llvm_ctx *ctx,
	LLVMValueRef arg_to,
	LLVMValueRef arg_timeouts,
	LLVMValueRef arg_fromspec,
	LLVMValueRef arg_mr0,
	LLVMValueRef *from_p,
	LLVMValueRef *mr1_p,
	LLVMValueRef *mr2_p)
{
	LLVMTypeRef params[5];
	for(int i=0; i<5; i++) params[i] = ctx->wordt;
	LLVMTypeRef ipc_result_type = LLVMStructTypeInContext(ctx->ctx,
		params, 4, 0);
	LLVMTypeRef ipc_type = LLVMFunctionType(ipc_result_type,
		params, 5, 0);
	LLVMValueRef fn = LLVMConstInlineAsm(ipc_type,
		"call __L4_Ipc\n",
		"={ax},={si},={bx},={bp},{ax},{cx},{dx},{si},{di},~{dirflag},~{fpsr},~{flags}",
		1, 0);
	LLVMValueRef args[5] = {
		arg_to, arg_timeouts, arg_fromspec, arg_mr0,
		LLVMBuildPtrToInt(ctx->builder, ctx->utcb, ctx->wordt,
			"l4ipc.utcb"),
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


LLVMValueRef build_utcb_load(
	struct llvm_ctx *ctx,
	LLVMValueRef ix,
	const char *name)
{
	return LLVMBuildLoad(ctx->builder,
		LLVMBuildGEP(ctx->builder, ctx->utcb, &ix, 1, "utcb.addr.in"),
		name);
}


LLVMValueRef build_u_from_tag(
	struct llvm_ctx *ctx,
	LLVMValueRef mr0)
{
	return LLVMBuildAnd(ctx->builder, mr0,
		LLVMConstInt(ctx->wordt, 0x3f, 0), "tag.u");
}


#if 0
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
#endif


void build_simple_string_item(
	struct llvm_ctx *ctx,
	LLVMValueRef *dest,
	LLVMValueRef data_ptr,
	LLVMValueRef data_len,
	LLVMValueRef cache_hint)
{
	if(cache_hint == NULL) cache_hint = ctx->zero;

	dest[0] = LLVMBuildPtrToInt(ctx->builder, data_ptr, ctx->wordt,
		"stritem.simple.ptr");
	dest[1] = LLVMBuildOr(ctx->builder,
		LLVMBuildShl(ctx->builder, data_len,
			LLVMConstInt(ctx->i32t, 10, 0), "stritem.simple.len.shl"),
		LLVMBuildShl(ctx->builder, cache_hint,
			LLVMConstInt(ctx->i32t, 1, 0), "stritem.simple.ch.shl"),
		"stritem.simple.info");
}
