/*
 * mapping.c -- encoding and decoding of the mapping type
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

#include <errno.h>
#include <llvm-c/Core.h>

#include "muidl.h"
#include "llvmutil.h"
#include "l4x2.h"


LLVMValueRef build_encode_mapping(
	struct llvm_ctx *ctx,
	LLVMValueRef t_pos,
	IDL_tree type,
	LLVMValueRef mapping_ptr,
	bool is_last)
{
	/* fetch and explode the snd_base_word. */
	V sndbase_raw = LLVMBuildLoad(ctx->builder,
			LLVMBuildStructGEP(ctx->builder, mapping_ptr, 0, "sndbase.ptr"),
			"sndbase.raw.val"),
		sndbase_actual = LLVMBuildAnd(ctx->builder, sndbase_raw,
			CONST_WORD(~0x3ffull), "sndbase.actual.val"),
		is_grant = LLVMBuildAnd(ctx->builder, sndbase_raw, CONST_WORD(1),
			"is.grant.bit"),
		ent_count = LLVMBuildLShr(ctx->builder,
			LLVMBuildAnd(ctx->builder, sndbase_raw, CONST_WORD(0x1e), "entcnt.raw"),
			CONST_WORD(1), "entcnt.val");

	/* where the fpages start at. */
	V fpages_ptr = LLVMBuildStructGEP(ctx->builder, mapping_ptr, 1,
		"fpages.ptr");
	/* map/grant item type bits, info word */
	V grant_cond = LLVMBuildICmp(ctx->builder, LLVMIntNE,
		CONST_WORD(0), is_grant, "is.grant.cond");
	V type_bits = LLVMBuildSelect(ctx->builder, grant_cond,
			CONST_WORD(0xa), CONST_WORD(0x8), "mgitem.type.bits"),
		info_word = LLVMBuildOr(ctx->builder, sndbase_actual, type_bits,
			"mapping.info.word");

	/* loop termination condition: t_pos' reaches t_pos0 + ent_count * 2 */
	V pos_limit = LLVMBuildAdd(ctx->builder, t_pos,
		LLVMBuildMul(ctx->builder, ent_count, CONST_WORD(2), "wordcnt"),
		"tpos.lim");

	BB loop = add_sibling_block(ctx, "mapping.enc.loop"),
		entry = LLVMGetInsertBlock(ctx->builder),
		after = add_sibling_block(ctx, "mapping.enc.after");
	LLVMBuildBr(ctx->builder, loop);
	LLVMPositionBuilderAtEnd(ctx->builder, loop);
	V loop_t_pos = LLVMBuildPhi(ctx->builder, ctx->wordt, "loop.tpos"),
		loop_ix = LLVMBuildPhi(ctx->builder, ctx->wordt, "loop.ix");
	LLVMAddIncoming(loop_t_pos, &t_pos, &entry, 1);
	LLVMAddIncoming(loop_ix, &ctx->zero, &entry, 1);

	/* loop tail bits; loop_cond is used for is_last processing */
	V t_pos_new = LLVMBuildAdd(ctx->builder, loop_t_pos, CONST_WORD(2),
			"loop.tpos.bump"),
		ix_new = LLVMBuildAdd(ctx->builder, loop_ix, CONST_WORD(1),
			"loop.ix.bump"),
		loop_cond = LLVMBuildICmp(ctx->builder, LLVMIntULT,
			t_pos_new, pos_limit, "loop.cond");
	branch_set_phi(ctx, loop_t_pos, t_pos_new);
	branch_set_phi(ctx, loop_ix, ix_new);

	V ixes[] = { ctx->zero, loop_ix },
		fpage = LLVMBuildLoad(ctx->builder,
			LLVMBuildGEP(ctx->builder, fpages_ptr, ixes, 2, "fpage.ptr"),
			"fpage.val");
	V words[2] = { info_word, fpage };
	if(is_last) {
		/* is this the last mapitem? loop_cond = !last */
		words[0] = LLVMBuildOr(ctx->builder, words[0],
			LLVMBuildSelect(ctx->builder, loop_cond, CONST_WORD(1),
				CONST_WORD(0), "last.bit"), "info.with.last");
	}
	build_store_mrs(ctx, loop_t_pos, words, 2);
	LLVMBuildCondBr(ctx->builder, loop_cond, loop, after);

	LLVMPositionBuilderAtEnd(ctx->builder, after);
	return t_pos_new;
}
