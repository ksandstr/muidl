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
#include <ccan/talloc/talloc.h>
#include <ccan/strmap/strmap.h>

#include "defs.h"
#include "l4x2.h"
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
	assert(ctx->build_msgerr_bb != NULL);

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

	GLIST_FOREACH(cur, ctx->malloc_ptrs) {
		LLVMValueRef ptr = cur->data;
		LLVMBuildFree(ctx->builder, ptr);
	}
	g_list_free(ctx->malloc_ptrs);
	ctx->malloc_ptrs = NULL;
}


static LLVMTypeRef llvm_mapgrant_type(struct llvm_ctx *ctx)
{
	T fields[2] = { ctx->wordt, ctx->wordt };
	return LLVMStructTypeInContext(ctx->ctx, fields, 2, 0);
}


/* TODO: [v2] get the word type from target info */
struct llvm_ctx *create_llvm_ctx(struct print_ctx *pr)
{
	struct llvm_ctx *ctx = talloc_zero(NULL, struct llvm_ctx);

	ctx->pr = pr;
	ctx->ns = pr->ns;
	ctx->ctx = LLVMContextCreate();
	strmap_init(&ctx->struct_decoder_fns);
	ctx->i32t = LLVMInt32TypeInContext(ctx->ctx);
	ctx->wordt = ctx->i32t;
	ctx->voidptrt = LLVMPointerType(LLVMInt8TypeInContext(ctx->ctx), 0);
	ctx->zero = LLVMConstInt(ctx->i32t, 0, 0);
	ctx->mapgrant = llvm_mapgrant_type(ctx);

	return ctx;
}


void dispose_llvm_ctx(struct llvm_ctx *ctx)
{
	if(ctx == NULL) return;
	strmap_clear(&ctx->struct_decoder_fns);
	LLVMContextDispose(ctx->ctx);
	talloc_free(ctx);
}


LLVMBasicBlockRef begin_function(struct llvm_ctx *ctx, LLVMValueRef fn)
{
	ctx->builder = LLVMCreateBuilderInContext(ctx->ctx);
	ctx->msgerr_bb = NULL;
	ctx->build_msgerr_bb = NULL;
	BB entry_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "EntryBlock");

	LLVMPositionBuilderAtEnd(ctx->builder, entry_bb);
	ctx->utcb = build_utcb_get(ctx);
	ctx->alloc_bb = entry_bb;
	assert(ctx->malloc_ptrs == NULL);

	return entry_bb;
}


void end_function(struct llvm_ctx *ctx, LLVMBasicBlockRef start_bb)
{
	BB prev = LLVMGetInsertBlock(ctx->builder);
	LLVMPositionBuilderAtEnd(ctx->builder, ctx->alloc_bb);
	LLVMBuildBr(ctx->builder, start_bb);
	LLVMPositionBuilderAtEnd(ctx->builder, prev);
	build_free_mallocs(ctx);
}


LLVMAttributeRef llvm_attr(struct llvm_ctx *ctx, const char *attr)
{
	/* TODO: stick these in a hash table or something? */
	LLVMAttributeRef a = LLVMCreateStringAttribute(ctx->ctx,
		attr, strlen(attr), "", 0);
	if(a == NULL) {
		printf("%s: couldn't create attr=`%s'!\n", __func__, attr);
		abort();
	}
	return a;
}
