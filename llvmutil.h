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

#include "defs.h"


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

/* NOTE: offset is in _words_, since that's the unit the UTCB is addressed
 * with.
 */
#define UTCB_ADDR_VAL(ctx, offset, name) \
	({ V _off = (offset); \
	   LLVMBuildGEP((ctx)->builder, (ctx)->utcb, &_off, 1, (name)); })


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

/* get the msgerr basic block. this function must be called before
 * ctx->errval_phi is referenced.
 */
extern LLVMBasicBlockRef get_msgerr_bb(struct llvm_ctx *ctx);

/* context management. */
extern struct llvm_ctx *create_llvm_ctx(struct print_ctx *pr);
extern void dispose_llvm_ctx(struct llvm_ctx *ctx);

/* function start helper. the caller should close the entry block with a call
 * to end_function() after the function has been built.
 */
extern LLVMBasicBlockRef begin_function(
	struct llvm_ctx *ctx,
	LLVMValueRef fn);

/* called in the block where pre-return cleanups should occur. closes the entry
 * block with a branch to the specified basic block.
 */
extern void end_function(struct llvm_ctx *ctx, LLVMBasicBlockRef start_bb);

#endif
