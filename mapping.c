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


LLVMValueRef build_decode_mapping(
	struct llvm_ctx *ctx,
	LLVMValueRef t_pos,
	LLVMValueRef t_max,
	IDL_tree type,
	LLVMValueRef mapping_ptr,
	bool is_last)
{
	V fpages_ptr = LLVMBuildStructGEP(ctx->builder, mapping_ptr, 1,
		"fpages.ptr");
	V ent_count = LLVMBuildUDiv(ctx->builder,
		LLVMBuildSub(ctx->builder, t_max, t_pos, "mapping.word.count"),
		CONST_WORD(2), "mapping.ent.count");
	/* use the first mapitem's sndbase, type bits */
	V first_info = LLVMBuildLoad(ctx->builder,
			UTCB_ADDR_VAL(ctx, t_pos, "first.info.ptr"), "first.info"),
		first_type = LLVMBuildAnd(ctx->builder, first_info,
			CONST_WORD(0xe), "first.type"),
		first_sndbase = LLVMBuildAnd(ctx->builder, first_info,
			CONST_WORD(~0x3ffull), "first.sndbase");
	/* identify the typed item, go to msgerr if not mapitem or grantitem */
	V is_map = LLVMBuildICmp(ctx->builder, LLVMIntEQ, first_type,
			CONST_WORD(0x8), "is.map.cond"),
		is_grant = LLVMBuildICmp(ctx->builder, LLVMIntEQ, first_type,
			CONST_WORD(0xa), "is.grant.cond"),
		is_either = LLVMBuildOr(ctx->builder, is_map, is_grant,
			"is.map.or.grant.cond");
	BB valid_first_item = add_sibling_block(ctx, "mapping.first.valid"),
		msgerr_bb = get_msgerr_bb(ctx);
	/* FIXME: what does a message decoder do when a typed item is of invalid
	 * type? this goes for string items also. should the C bit be ignored here
	 * like this?
	 */
	branch_set_phi(ctx, ctx->errval_phi, CONST_INT(-EINVAL));
	LLVMBuildCondBr(ctx->builder, is_either, valid_first_item, msgerr_bb);

	LLVMPositionBuilderAtEnd(ctx->builder, valid_first_item);
	V type_bit = LLVMBuildSelect(ctx->builder, is_map,
		CONST_WORD(0), CONST_WORD(1), "sndbaseword.type.bit");
	LLVMBuildStore(ctx->builder,
		LLVMBuildOr(ctx->builder, type_bit,
			LLVMBuildOr(ctx->builder, first_sndbase,
				LLVMBuildShl(ctx->builder, ent_count, CONST_WORD(1),
					"ent.cnt.bits"),
				"ent.cnt.sndbase.bits"),
			"ent.cnt.sndbase.type.bits"),
		LLVMBuildStructGEP(ctx->builder, mapping_ptr, 0, "sndbaseword.ptr"));

	BB loop = add_sibling_block(ctx, "mapping.decode.loop");
	LLVMPositionBuilderAtEnd(ctx->builder, loop);
	V loop_ix = LLVMBuildPhi(ctx->builder, ctx->i32t, "loop.ix");
	LLVMPositionBuilderAtEnd(ctx->builder, valid_first_item);
	branch_set_phi(ctx, loop_ix, CONST_INT(0));
	LLVMBuildBr(ctx->builder, loop);

	LLVMPositionBuilderAtEnd(ctx->builder, loop);
	V mr_num = LLVMBuildAdd(ctx->builder, t_pos,
			LLVMBuildMul(ctx->builder, loop_ix, CONST_INT(2), "loop.ix.words"),
			"loop.mr.num"),
		fpage_num = LLVMBuildAdd(ctx->builder, mr_num, CONST_INT(1),
			"fpage.mr.num");
	V fpage = LLVMBuildLoad(ctx->builder,
		UTCB_ADDR_VAL(ctx, fpage_num, "fpage.mr.ptr"), "fpage.raw");
	V ixes[2] = { ctx->zero, loop_ix };
	LLVMBuildStore(ctx->builder, fpage,
		LLVMBuildGEP(ctx->builder, fpages_ptr, ixes, 2, "out.fpage.ptr"));

	/* bump loop, check cond */
	V loop_ix_next = LLVMBuildAdd(ctx->builder, loop_ix, CONST_INT(1),
			"loop.ix.next"),
		loop_cond = LLVMBuildICmp(ctx->builder, LLVMIntULT, loop_ix_next,
			ent_count, "loop.cond");
	BB after = add_sibling_block(ctx, "mapping.decode.after");
	branch_set_phi(ctx, loop_ix, loop_ix_next);
	LLVMBuildCondBr(ctx->builder, loop_cond, loop, after);

	LLVMPositionBuilderAtEnd(ctx->builder, after);
	return LLVMBuildAdd(ctx->builder, ent_count, t_pos, "t.pos.after");
}
