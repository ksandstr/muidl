/*
 * llvmutil.h -- utility things for use with the LLVM C API (header file)
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

#ifndef SEEN_LLVMUTIL_H
#define SEEN_LLVMUTIL_H

#include <llvm-c/Core.h>

#include "muidl.h"


/* shorthand for LLVM types.
 *
 * NOTE: these are only for declaring variables. formal function parameter
 * types should still be written out.
 */

#define V LLVMValueRef
#define T LLVMTypeRef
#define BB LLVMBasicBlockRef


/* constant macros. more useful than piles of LLVMConstInt(ctx->i32t, ...). */
#define CONST_INT(n) LLVMConstInt(ctx->i32t, (n), 1)
#define CONST_UINT(n) LLVMConstInt(ctx->i32t, (n), 0)
#define CONST_WORD(n) LLVMConstInt(ctx->wordt, (n), 0)

/* casting */
#define WORD(v) LLVMBuildZExtOrBitCast(ctx->builder, (v), ctx->wordt, "cast.w")


/* helper functions from llvmutil.c */

/* set a value @val on @phi from origin LLVMGetInsertBlock(ctx->builder). */
extern void branch_set_phi(
	struct llvm_ctx *ctx,
	LLVMValueRef phi,
	LLVMValueRef val);

extern LLVMBasicBlockRef add_sibling_block(
	struct llvm_ctx *ctx,
	const char *name_fmt,
	...);

/* build free insns for LLVMValueRef data in ctx->malloc_ptrs, then truncate
 * the list.
 */
extern void build_free_mallocs(struct llvm_ctx *ctx);

#endif
