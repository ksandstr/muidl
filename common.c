/*
 * common.c -- building of functions for the "common" module
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


#include <glib.h>
#include <libIDL/IDL.h>

#include "muidl.h"
#include "llvmutil.h"


static void build_packed_struct_decoder(struct llvm_ctx *ctx, IDL_tree styp)
{
	V fn = get_struct_decoder_fn(ctx, styp);

	ctx->builder = LLVMCreateBuilderInContext(ctx->ctx);
	BB entry_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "EntryBlock"),
		unpack_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "unpack");
	ctx->alloc_bb = entry_bb;

	LLVMPositionBuilderAtEnd(ctx->builder, unpack_bb);
	ctx->utcb = build_utcb_get(ctx);
	ctx->zero = CONST_WORD(0);
	V params[2];
	LLVMGetParams(fn, params);
	decode_packed_struct_inline(ctx, params[0], styp, params[1]);

	/* return. */
	build_free_mallocs(ctx);
	LLVMBuildRetVoid(ctx->builder);

	/* close off the alloc block. */
	LLVMPositionBuilderAtEnd(ctx->builder, entry_bb);
	LLVMBuildBr(ctx->builder, unpack_bb);

	LLVMDisposeBuilder(ctx->builder);
}


gboolean iter_build_common_module(IDL_tree_func_data *tf, void *ud)
{
	struct llvm_ctx *ctx = ud;
	switch(IDL_NODE_TYPE(tf->tree)) {
		default: return FALSE;

		case IDLN_LIST:
		case IDLN_MODULE:
		case IDLN_SRCFILE:
		case IDLN_INTERFACE:
			return TRUE;

		case IDLN_TYPE_STRUCT:
			build_packed_struct_decoder(ctx, tf->tree);
			return FALSE;
	}
}
