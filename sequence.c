/*
 * sequence.c -- encoding and decoding of sequence types
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


LLVMValueRef build_decode_inline_sequence(
	struct llvm_ctx *ctx,
	LLVMValueRef *args,
	const struct msg_param *seq,
	LLVMValueRef upos,
	bool is_last,
	LLVMValueRef errval_phi,
	LLVMBasicBlockRef err_bb)
{
	const int bpe = seq->X.seq.bits_per_elem,
		epw = seq->X.seq.elems_per_word;
	/* compute length of sequence in words and items */
	LLVMValueRef seq_words, seq_len;
	if(!is_last || bpe < BITS_PER_WORD) {
		/* not subject to trickery; take a length word. */
		seq_len = LLVMBuildTruncOrBitCast(ctx->builder,
			build_utcb_load(ctx, upos, "inlseq.len.mr"),
			ctx->i32t, "inlseq.len.explicit");
		if(epw == 1) seq_words = seq_len;
		else {
			seq_words = LLVMBuildUDiv(ctx->builder, seq_len,
				CONST_UINT(epw), "inlseq.len.int");
		}
		upos = LLVMBuildAdd(ctx->builder, upos, CONST_UINT(1),
			"inlseq.pos.bump");
	} else {
		/* last sequence of word-length items; compute length from "u". */
		LLVMValueRef u = build_u_from_tag(ctx, ctx->tag);
		seq_words = LLVMBuildSub(ctx->builder, u, upos,
			"inlseq.len.implicit");
		seq_words = LLVMBuildTruncOrBitCast(ctx->builder, seq_words,
			ctx->i32t, "inlseq.len.int");
		seq_len = seq_words;
	}

	LLVMBasicBlockRef loop_bb, exit_bb, odd_tail_bb = NULL,
		skip_loop_bb = NULL;
	if(epw > 1) {
		skip_loop_bb = add_sibling_block(ctx, "inlseq.skipmain.test");
		odd_tail_bb = add_sibling_block(ctx, "inlseq.odd.tail");
	}
	loop_bb = add_sibling_block(ctx, "inlseq.loop");
	exit_bb = add_sibling_block(ctx, "inlseq.loop.exit");

	/* guard against maximum size violations */
	/* FIXME: get EINVAL from µiX headers */
	branch_set_phi(ctx, errval_phi, CONST_INT(-EINVAL));
	LLVMValueRef einval_cond = LLVMBuildICmp(ctx->builder, LLVMIntULT,
		seq_len, CONST_UINT(seq->X.seq.max_elems), "inlseq.len.cond");
	LLVMBuildCondBr(ctx->builder, einval_cond,
		epw > 1 ? skip_loop_bb : loop_bb,
		err_bb);

	if(epw > 1) {
		/* check for less than a word's worth of items, skip main copy loop if
		 * so
		 */
		LLVMPositionBuilderAtEnd(ctx->builder, skip_loop_bb);
		LLVMValueRef cond = LLVMBuildICmp(ctx->builder, LLVMIntULT,
			seq_len, CONST_UINT(epw),
			"inlseq.skipmain.cond");
		LLVMBuildCondBr(ctx->builder, cond, odd_tail_bb, loop_bb);
	}

	LLVMBasicBlockRef before_loop = LLVMGetInsertBlock(ctx->builder);

	/* after decoding, outputs are just the new seqpos. */
	LLVMPositionBuilderAtEnd(ctx->builder, exit_bb);
	LLVMValueRef ret = LLVMBuildPhi(ctx->builder, ctx->i32t,
		"inlseq.final.sp");

	/* the copy loop. */
	LLVMPositionBuilderAtEnd(ctx->builder, loop_bb);
	LLVMTypeRef seq_type = llvm_value_type(ctx, seq->X.seq.elem_type);
	LLVMValueRef seq_mem = build_local_storage(ctx, seq_type,
		CONST_UINT(seq->X.seq.max_elems), "inlseq.mem");
	LLVMValueRef seq_pos, counter;
	seq_pos = LLVMBuildPhi(ctx->builder, ctx->i32t, "loop.seqpos");
	counter = LLVMBuildPhi(ctx->builder, ctx->i32t, "loop.ctr");
	LLVMAddIncoming(seq_pos, &upos, &before_loop, 1);
	LLVMAddIncoming(counter, &ctx->zero, &before_loop, 1);

	if(bpe == BITS_PER_WORD) {
		/* full word case */
		LLVMValueRef item;
		build_read_ipc_parameter_ixval(ctx, &item, seq->X.seq.elem_type,
			seq_pos);
		LLVMBuildStore(ctx->builder, item,
			LLVMBuildGEP(ctx->builder, seq_mem, &counter, 1,
				"seq.mem.at"));
	} else {
		/* many-per-word (packed) case. */
		LLVMValueRef word = build_utcb_load(ctx, seq_pos, "seq.limb");
		for(int i=0; i<epw; i++) {
			int downshift = bpe * i;
			LLVMValueRef item = LLVMBuildTrunc(ctx->builder,
				i == 0 ? word : LLVMBuildLShr(ctx->builder, word,
					CONST_UINT(downshift), "seq.limb.shifted"),
				seq_type, "seq.limb.trunc");
			LLVMValueRef ix = LLVMBuildAdd(ctx->builder, counter,
				CONST_UINT(i), "seq.limb.ix");
			LLVMBuildStore(ctx->builder, item,
				LLVMBuildGEP(ctx->builder, seq_mem, &ix, 1,
					"seq.limb.at"));
		}
	}

	LLVMValueRef next_counter, next_sp;
	next_counter = LLVMBuildAdd(ctx->builder, counter,
		CONST_UINT(epw), "loop.ctr.next");
	next_sp = LLVMBuildAdd(ctx->builder, seq_pos, CONST_UINT(1),
		"loop.seqpos.next");
	branch_set_phi(ctx, counter, next_counter);
	branch_set_phi(ctx, seq_pos, next_sp);
	if(epw == 1) {
		branch_set_phi(ctx, ret, next_sp);
	}
	LLVMValueRef exit_cond = LLVMBuildICmp(ctx->builder,
		LLVMIntULT, next_counter, seq_len, "loop.nextp");
	LLVMBuildCondBr(ctx->builder, exit_cond, loop_bb,
		epw > 1 ? odd_tail_bb : exit_bb);

	if(epw > 1) {
		/* tail of a packed sequence. */
		LLVMPositionBuilderAtEnd(ctx->builder, odd_tail_bb);
		LLVMValueRef odd_offs = LLVMBuildPhi(ctx->builder, ctx->i32t, "odd.off"),
			odd_seqpos = LLVMBuildPhi(ctx->builder, ctx->i32t, "odd.sp"),
			wordval = build_utcb_load(ctx, odd_seqpos, "tail.word");
		LLVMAddIncoming(odd_offs, &ctx->zero, &skip_loop_bb, 1);
		LLVMAddIncoming(odd_seqpos, &upos, &skip_loop_bb, 1);
		LLVMAddIncoming(odd_offs, &next_counter, &loop_bb, 1);
		LLVMAddIncoming(odd_seqpos, &next_sp, &loop_bb, 1);
		branch_set_phi(ctx, ret, odd_seqpos);
		LLVMValueRef sw = LLVMBuildSwitch(ctx->builder,
			LLVMBuildAnd(ctx->builder, seq_len,
				CONST_UINT(epw - 1), "odd.len"),
			exit_bb, epw - 1);

		for(int i=epw - 1; i>0; i--) {
			BB bb = add_sibling_block(ctx, "inlseq.odd.%dcase", i);
			if(i != epw - 1) LLVMBuildBr(ctx->builder, bb);
			LLVMPositionBuilderAtEnd(ctx->builder, bb);
			LLVMAddCase(sw, CONST_UINT(i), bb);
			LLVMValueRef limb = LLVMBuildTrunc(ctx->builder,
				LLVMBuildLShr(ctx->builder, wordval,
					CONST_UINT((i - 1) * bpe),
					tmp_f(ctx->pr, "odd.c%d.shifted", i)),
				seq_type, tmp_f(ctx->pr, "odd.c%d.limb", i));
			LLVMValueRef offs = LLVMBuildAdd(ctx->builder, odd_offs,
				CONST_UINT(i - 1), "odd.limb.offs");
			LLVMBuildStore(ctx->builder, limb,
				LLVMBuildGEP(ctx->builder, seq_mem, &offs, 1,
					"odd.limb.ptr"));
		}

		LLVMValueRef sp_bump = LLVMBuildAdd(ctx->builder, odd_seqpos,
			CONST_UINT(1), "odd.sp.bump");
		branch_set_phi(ctx, ret, sp_bump);
		LLVMBuildBr(ctx->builder, exit_bb);
	}

	LLVMPositionBuilderAtEnd(ctx->builder, exit_bb);

	args[0] = seq_mem;
	args[1] = seq_len;

	return ret;
}


static LLVMValueRef build_load_subval_and_shift(
	struct llvm_ctx *ctx,
	LLVMValueRef mem,
	LLVMValueRef pos,
	int ix,
	const struct msg_param *seq)
{
	LLVMValueRef sub_pos = LLVMBuildAdd(ctx->builder,
		pos, CONST_UINT(ix), "dstitem.sub.pos");
	LLVMValueRef subval = WORD(LLVMBuildLoad(ctx->builder,
		LLVMBuildGEP(ctx->builder, mem, &sub_pos, 1, "dstitem.sub.ptr"),
		"dstitem.sub.val"));
	/* fill from left to right, in little-endian order. this optimizes the
	 * one-odd tail case while not penalizing any other.
	 */
	int upshift = seq->X.seq.bits_per_elem * ix;
	return LLVMBuildShl(ctx->builder, subval, CONST_UINT(upshift),
		"dstitem.sub.val.shifted");
}


/* this code is a bit of a mess with the basic block names and other stuff. */
LLVMValueRef build_encode_inline_sequence(
	struct llvm_ctx *ctx,
	LLVMValueRef mem,
	LLVMValueRef lenptr,
	const struct msg_param *seq,
	LLVMValueRef upos,
	bool is_last)
{
	const int epw = seq->X.seq.elems_per_word;

	LLVMValueRef num_items = LLVMBuildLoad(ctx->builder,
		lenptr, "inlseq.len");
	if(epw > 1 || !is_last) {
		/* not simple && last; emit an item-count word. */
		LLVMBuildStore(ctx->builder, WORD(num_items),
			LLVMBuildGEP(ctx->builder, ctx->utcb,
				&upos, 1, "inlseq.len.word.offs"));
		upos = LLVMBuildAdd(ctx->builder, upos, CONST_UINT(1),
			"inlseq.pos.bump");
	}

	/* loop per word. */
	LLVMBasicBlockRef before_loop_bb = LLVMGetInsertBlock(ctx->builder);
	LLVMValueRef fn = LLVMGetBasicBlockParent(before_loop_bb);
	LLVMBasicBlockRef loop_bb = LLVMAppendBasicBlockInContext(
			ctx->ctx, fn, "inlseq.out.loop"),
		loop_after_bb = LLVMAppendBasicBlockInContext(
			ctx->ctx, fn, "inlseq.out.loop.after");
	LLVMBuildCondBr(ctx->builder,
		LLVMBuildICmp(ctx->builder, LLVMIntUGE, num_items,
			CONST_UINT(epw),
			"inlseq.out.loop.entrycond"),
		loop_bb, loop_after_bb);
	LLVMValueRef isp_before_loop = upos;

	LLVMPositionBuilderAtEnd(ctx->builder, loop_bb);
	LLVMValueRef pos = LLVMBuildPhi(ctx->builder, ctx->i32t,
			"inlseq.out.pos"),
		word_ix = LLVMBuildPhi(ctx->builder, ctx->i32t,
			"inlseq.out.word_ix");
	LLVMAddIncoming(pos, &ctx->zero, &before_loop_bb, 1);
	LLVMAddIncoming(word_ix, &upos, &before_loop_bb, 1);
	LLVMValueRef wordval;
	if(epw == 1) {
		wordval = WORD(LLVMBuildLoad(ctx->builder,
			LLVMBuildGEP(ctx->builder, mem, &pos, 1, "item.ptr"),
			"item.data"));
	} else {
		/* bit-pack elements.
		 * TODO: make this emit 4- and 8-wide cases in a treelike way
		 * instead of a deep dependency chain such as this
		 */
		wordval = ctx->zero;
		for(int i=0; i < epw; i++) {
			wordval = LLVMBuildOr(ctx->builder, wordval,
				build_load_subval_and_shift(ctx, mem, pos, i, seq),
				"item.wordval");
		}
	}
	LLVMBuildStore(ctx->builder, wordval,
		LLVMBuildGEP(ctx->builder, ctx->utcb, &word_ix, 1, "word_ix.ptr"));
	LLVMValueRef next_pos = LLVMBuildAdd(ctx->builder, pos,
			CONST_UINT(epw), "inlseq.out.pos.next"),
		next_word = LLVMBuildAdd(ctx->builder, word_ix,
			CONST_UINT(1), "inlseq.out.word_ix.next");
	branch_set_phi(ctx, pos, next_pos);
	branch_set_phi(ctx, word_ix, next_word);
	/* the fancy epw conditional there is to make sure that
	 * the loop is only executed while there are as many or more than
	 * epw items left.
	 */
	LLVMValueRef exit_cond = LLVMBuildICmp(ctx->builder, LLVMIntULT,
		epw == 1
			? next_pos
			: LLVMBuildAdd(ctx->builder, next_pos,
				CONST_UINT(epw - 1),
				"inlseq.out.pos.next.test"),
		num_items, "exit.cond");
	LLVMBuildCondBr(ctx->builder, exit_cond, loop_bb, loop_after_bb);

	LLVMPositionBuilderAtEnd(ctx->builder, loop_after_bb);
	LLVMValueRef pos_phi = LLVMBuildPhi(ctx->builder, ctx->i32t,
			"inlseq.out.pos.phi"),
		next_word_phi = LLVMBuildPhi(ctx->builder, ctx->i32t,
			"inlseq.out.nextword.phi");
	LLVMAddIncoming(pos_phi, &ctx->zero, &before_loop_bb, 1);
	LLVMAddIncoming(next_word_phi, &isp_before_loop, &before_loop_bb, 1);
	LLVMAddIncoming(pos_phi, &pos, &loop_bb, 1);
	LLVMAddIncoming(next_word_phi, &next_word, &loop_bb, 1);
	pos = pos_phi;
	next_word = next_word_phi;
	if(epw > 1) {
		LLVMBasicBlockRef after_bb = LLVMAppendBasicBlockInContext(
			ctx->ctx, fn, "inlseq.odd.after");

		/* one-round duff's device for trailing elements. not
		 * pretty, nor too compact, but gets the job done.
		 */
		LLVMValueRef remain = LLVMBuildSub(ctx->builder,
			num_items, pos, "inlseq.odd.len");
		/* if there are odd items, add another word. */
		LLVMValueRef store_word = next_word;
		next_word = LLVMBuildSelect(ctx->builder,
			LLVMBuildICmp(ctx->builder, LLVMIntUGT, remain,
				CONST_UINT(0), "inlseq.odd.exists"),
			LLVMBuildAdd(ctx->builder, next_word, CONST_UINT(1),
				"inlseq.odd.word.bump"),
			next_word, "inlseq.odd.word.bump.maybe");

		LLVMValueRef sel = LLVMBuildSwitch(ctx->builder, remain,
			after_bb, epw - 1);

		wordval = ctx->zero;
		LLVMBasicBlockRef prev = LLVMGetInsertBlock(ctx->builder);
		for(int i=epw-1; i > 0; i--) {
			LLVMBasicBlockRef b = LLVMAppendBasicBlockInContext(
				ctx->ctx, fn, tmp_f(ctx->pr, "inlseq.odd.c%d", i));
			if(prev != loop_after_bb) LLVMBuildBr(ctx->builder, b);
			LLVMAddCase(sel, CONST_UINT(i), b);
			LLVMPositionBuilderAtEnd(ctx->builder, b);
			LLVMValueRef word_phi;
			if(prev == loop_after_bb) word_phi = ctx->zero;
			else {
				word_phi = LLVMBuildPhi(ctx->builder, ctx->wordt,
					tmp_f(ctx->pr, "inlseq.odd.wordval.p%d", i));
				LLVMAddIncoming(word_phi, &ctx->zero, &loop_after_bb, 1);
				LLVMAddIncoming(word_phi, &wordval, &prev, 1);
			}
			wordval = LLVMBuildOr(ctx->builder, word_phi,
				build_load_subval_and_shift(ctx, mem, pos, i - 1, seq),
				tmp_f(ctx->pr, "inlseq.odd.wordval.v%d", i));
			prev = b;
		}
		LLVMBasicBlockRef store_bb = LLVMAppendBasicBlockInContext(
			ctx->ctx, fn, "inlseq.odd.store");
		LLVMBuildBr(ctx->builder, store_bb);

		LLVMPositionBuilderAtEnd(ctx->builder, store_bb);
		LLVMBuildStore(ctx->builder, wordval,
			LLVMBuildGEP(ctx->builder, ctx->utcb, &store_word, 1,
				"inlseq.len.ptr"));
		LLVMBuildBr(ctx->builder, after_bb);

		LLVMPositionBuilderAtEnd(ctx->builder, after_bb);
	}

	return next_word;
}
