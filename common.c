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

#include "defs.h"
#include "llvmutil.h"
#include "l4x2.h"


static bool is_struct_used(
	struct llvm_ctx *ctx,
	IDL_tree parent,
	IDL_tree stype);


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
	V params[3];
	LLVMGetParams(fn, params);
	const struct packed_format *fmt = packed_format_of(styp);
	assert(fmt != NULL);
	if(fmt->num_bits >= BITS_PER_WORD) params[2] = CONST_INT(0);
	decode_packed_struct_inline(ctx, params[0], styp, params[1], params[2]);

	/* return. */
	build_free_mallocs(ctx);
	LLVMBuildRetVoid(ctx->builder);

	/* close off the alloc block. */
	LLVMPositionBuilderAtEnd(ctx->builder, entry_bb);
	LLVMBuildBr(ctx->builder, unpack_bb);

	LLVMDisposeBuilder(ctx->builder);
}


static void build_packed_struct_encoder(struct llvm_ctx *ctx, IDL_tree styp)
{
	V fn = get_struct_encoder_fn(ctx, styp);

	ctx->builder = LLVMCreateBuilderInContext(ctx->ctx);
	BB entry_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "EntryBlock"),
		pack_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "pack");
	ctx->alloc_bb = entry_bb;

	LLVMPositionBuilderAtEnd(ctx->builder, pack_bb);
	ctx->utcb = build_utcb_get(ctx);
	ctx->zero = CONST_WORD(0);
	V params[3], retval;
	LLVMGetParams(fn, params);
	const struct packed_format *fmt = packed_format_of(styp);
	assert(fmt != NULL);
	if(fmt->num_bits < BITS_PER_WORD) {
		retval = encode_packed_struct_inline(ctx, params[1], params[2],
			styp, params[0]);
	} else {
		encode_packed_struct_inline(ctx, params[1], NULL, styp, params[0]);
		retval = NULL;
	}

	/* return. */
	build_free_mallocs(ctx);
	if(retval != NULL) LLVMBuildRet(ctx->builder, retval);
	else LLVMBuildRetVoid(ctx->builder);

	/* close off the alloc block. */
	LLVMPositionBuilderAtEnd(ctx->builder, entry_bb);
	LLVMBuildBr(ctx->builder, pack_bb);

	LLVMDisposeBuilder(ctx->builder);
}


struct if_struct_data {
	bool found;
	const char *ref_id;
	IDL_tree stype, parent;
	struct llvm_ctx *ctx;
};

/* TODO: this could be quicker if results were memoized by stype->repo_id,
 * but meh.
 */
static gboolean see_if_struct(IDL_tree_func_data *tf, void *ud)
{
	struct if_struct_data *data = ud;

	if(data->found) return FALSE;

	if(IDL_NODE_TYPE(tf->tree) == IDLN_TYPE_STRUCT
		&& tf->tree != data->stype)
	{
		IDL_LIST_FOREACH(cur, IDL_TYPE_STRUCT(tf->tree).member_list) {
			IDL_tree m = IDL_LIST(cur).data,
				type = get_type_spec(IDL_MEMBER(m).type_spec);
			if(IDL_NODE_TYPE(type) != IDLN_TYPE_STRUCT) continue;
			const char *id = IDL_IDENT(
				IDL_TYPE_STRUCT(type).ident).repo_id;
			if(strcmp(data->ref_id, id) == 0) {
				/* but this is only a hit if the other struct is used
				 * itself.
				 */
				data->found = is_struct_used(data->ctx, data->parent,
					tf->tree);
				break;
			}
		}
		return FALSE;
	} else if(IDL_NODE_TYPE(tf->tree) == IDLN_OP_DCL) {
		IDL_LIST_FOREACH(cur, IDL_OP_DCL(tf->tree).parameter_dcls) {
			IDL_tree p = IDL_LIST(cur).data,
				type = get_type_spec(IDL_PARAM_DCL(p).param_type_spec);
			if(IDL_NODE_TYPE(type) != IDLN_TYPE_STRUCT) continue;
			const char *id = IDL_IDENT(
				IDL_TYPE_STRUCT(type).ident).repo_id;
			if(strcmp(data->ref_id, id) == 0) {
				/* found a direct consumer, yay */
				data->found = true;
				break;
			}
		}
		return FALSE;
	} else {
		return TRUE;
	}
}


static bool is_struct_used(
	struct llvm_ctx *ctx,
	IDL_tree parent,
	IDL_tree stype)
{
	struct if_struct_data ifs_data = {
		.found = false,
		.ref_id = IDL_IDENT(IDL_TYPE_STRUCT(stype).ident).repo_id,
		.stype = stype, .parent = parent,
		.ctx = ctx,
	};
	IDL_tree_walk_in_order(parent, &see_if_struct, &ifs_data);
	return ifs_data.found;
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

		case IDLN_TYPE_STRUCT: {
			/* only emit those structs that are mentioned in opdcl parameters,
			 * or reachable structs, of the same module.
			 */
			IDL_tree parent = IDL_get_parent_node(tf->tree, IDLN_INTERFACE, NULL);
			if(parent == NULL) {
				/* ... if not inside an interface, a module is fine too. */
				parent = IDL_get_parent_node(tf->tree, IDLN_MODULE, NULL);
			}
			if(parent == NULL) {
				/* or just the toplevel list... */
				parent = ctx->pr->tree;
			}
			assert(parent != NULL);
			if(is_struct_used(ctx, parent, tf->tree)) {
				build_packed_struct_decoder(ctx, tf->tree);
				build_packed_struct_encoder(ctx, tf->tree);
			}
			return FALSE;
		}
	}
}
