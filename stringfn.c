/*
 * stringfn.c -- LLVM analogs of some <string.h> functions
 * Copyright 2011  Kalle A. Sandström <ksandstr@iki.fi>
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

#include "muidl.h"
#include "llvmutil.h"


LLVMValueRef get_strlen_fn(struct llvm_ctx *ctx)
{
	V fn = LLVMGetNamedFunction(ctx->module, "strlen");
	if(fn != NULL) return fn;

	T charptr = LLVMPointerType(LLVMInt8TypeInContext(ctx->ctx), 0),
		sizet = ctx->wordt;		/* FIXME: get from ABI; should be size_t */
	T fntype = LLVMFunctionType(sizet, &charptr, 1, 0);
	fn = LLVMAddFunction(ctx->module, "strlen", fntype);

	return fn;
}


LLVMValueRef get_memcpy_fn(struct llvm_ctx *ctx)
{
	V fn = LLVMGetNamedFunction(ctx->module, "memcpy");
	if(fn != NULL) return fn;

	/* TODO: get size_t from ABI? */
	T voidptr = ctx->voidptrt, sizet = ctx->wordt;
	T argtypes[] = { voidptr, voidptr, sizet },
		fntype = LLVMFunctionType(voidptr, argtypes, 3, 0);
	fn = LLVMAddFunction(ctx->module, "memcpy", fntype);

	return fn;
}
