/*
 * llvmutil.c -- utility things for use with the LLVM C API
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

#include <stdarg.h>
#include <glib.h>
#include <libIDL/IDL.h>
#include <llvm-c/Core.h>

#include "muidl.h"
#include "llvmutil.h"


void branch_set_phi(
	struct llvm_ctx *ctx,
	LLVMValueRef phi,
	LLVMValueRef val)
{
	BB current = LLVMGetInsertBlock(ctx->builder);
	LLVMAddIncoming(phi, &val, &current, 1);
}


LLVMBasicBlockRef add_sibling_block(
	struct llvm_ctx *ctx,
	const char *name_fmt,
	...)
{
	va_list al;
	va_start(al, name_fmt);
	char *name = tmp_vf(ctx->pr, name_fmt, al);
	V fn = LLVMGetBasicBlockParent(LLVMGetInsertBlock(ctx->builder));
	/* TODO: assert that "fn" is a function reference */
	return LLVMAppendBasicBlockInContext(ctx->ctx, fn, name);
}


LLVMBasicBlockRef get_msgerr_bb(struct llvm_ctx *ctx)
{
	if(ctx->msgerr_bb == NULL) {
		ctx->msgerr_bb = add_sibling_block(ctx, "msgerr");

		BB prior = LLVMGetInsertBlock(ctx->builder);
		LLVMPositionBuilderAtEnd(ctx->builder, ctx->msgerr_bb);
		ctx->errval_phi = LLVMBuildPhi(ctx->builder, ctx->i32t, "errval.phi");
		(*ctx->build_msgerr_bb)(ctx);
		LLVMPositionBuilderAtEnd(ctx->builder, prior);
	}

	return ctx->msgerr_bb;
}


void build_free_mallocs(struct llvm_ctx *ctx)
{
	if(ctx->malloc_ptrs == NULL) return;

	for(GList *cur = g_list_first(ctx->malloc_ptrs);
		cur != NULL;
		cur = g_list_next(cur))
	{
		LLVMValueRef ptr = cur->data;
		LLVMBuildFree(ctx->builder, ptr);
	}
	g_list_free(ctx->malloc_ptrs);
	ctx->malloc_ptrs = NULL;
}


/* TODO: [v2] get the word type from target info */
struct llvm_ctx *create_llvm_ctx(struct print_ctx *pr)
{
	struct llvm_ctx *ctx = g_new0(struct llvm_ctx, 1);

	ctx->pr = pr;
	ctx->ns = pr->ns;
	ctx->ctx = LLVMContextCreate();
	ctx->struct_decoder_fns = g_hash_table_new_full(
		&g_str_hash, &g_str_equal, &g_free, NULL);
	ctx->i32t = LLVMInt32TypeInContext(ctx->ctx);
	ctx->wordt = ctx->i32t;
	ctx->voidptrt = LLVMPointerType(LLVMInt8TypeInContext(ctx->ctx), 0);
	ctx->zero = LLVMConstInt(ctx->i32t, 0, 0);
	T mapgrant_fields[] = { ctx->wordt, ctx->wordt };
	ctx->mapgrant = LLVMStructTypeInContext(ctx->ctx, mapgrant_fields, 2, 1);

	return ctx;
}


void dispose_llvm_ctx(struct llvm_ctx *ctx)
{
	g_hash_table_destroy(ctx->struct_decoder_fns);
	LLVMContextDispose(ctx->ctx);
	g_free(ctx);
}
