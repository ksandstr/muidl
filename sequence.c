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


/* TODO: this function will likely be used by the stub generator also. move it
 * into a sequence.c file or some such.
 */
LLVMValueRef build_decode_inline_sequence(
	struct llvm_ctx *ctx,
	LLVMValueRef *args,
	int *arg_pos_p,
	const struct seq_param *seq,
	LLVMValueRef upos,
	bool is_last,
	LLVMValueRef errval_phi,
	LLVMBasicBlockRef err_bb)
{
	/* compute length of sequence in words and items */
	LLVMValueRef seq_words, seq_len;
	if(!is_last || seq->bits_per_elem < BITS_PER_WORD) {
		/* not subject to trickery; take a length word. */
		seq_len = LLVMBuildTruncOrBitCast(ctx->builder,
			build_utcb_load(ctx, upos, "inlseq.len.mr"),
			ctx->i32t, "inlseq.len.explicit");
		if(seq->elems_per_word == 1) seq_words = seq_len;
		else {
			seq_words = LLVMBuildUDiv(ctx->builder, seq_len,
				LLVMConstInt(ctx->i32t, seq->elems_per_word, 0),
				"inlseq.len.int");
		}
		upos = LLVMBuildAdd(ctx->builder, upos,
			LLVMConstInt(ctx->i32t, 1, 0), "inlseq.pos.bump");
	} else {
		/* last sequence of word-length items; compute length from "u". */
		LLVMValueRef u = build_u_from_tag(ctx, ctx->tag);
		seq_words = LLVMBuildSub(ctx->builder, u, upos,
			"inlseq.len.implicit");
		seq_words = LLVMBuildTruncOrBitCast(ctx->builder, seq_words,
			ctx->i32t, "inlseq.len.int");
		seq_len = seq_words;
	}

	LLVMBasicBlockRef entry_bb = LLVMGetInsertBlock(ctx->builder);
	LLVMValueRef fn = LLVMGetBasicBlockParent(entry_bb);
	LLVMBasicBlockRef loop_bb, exit_bb, odd_tail_bb = NULL,
		skip_loop_bb = NULL;
	if(seq->elems_per_word > 1) {
		skip_loop_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn,
			"inlseq.skipmain.test");
		odd_tail_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn,
			"inlseq.odd.tail");
	}
	loop_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "inlseq.loop");
	exit_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "inlseq.loop.exit");

	/* guard against maximum size violations */
	/* FIXME: get EINVAL from µiX headers */
	LLVMValueRef einval = LLVMConstInt(ctx->i32t, -EINVAL, 1);
	LLVMAddIncoming(errval_phi, &einval, &entry_bb, 1);
	LLVMValueRef einval_cond = LLVMBuildICmp(ctx->builder, LLVMIntULT,
		seq_len, LLVMConstInt(ctx->i32t, seq->max_elems, 0),
		"inlseq.len.cond");
	LLVMBuildCondBr(ctx->builder, einval_cond,
		seq->elems_per_word > 1 ? skip_loop_bb : loop_bb,
		err_bb);

	if(seq->elems_per_word > 1) {
		/* check for less than a word's worth of items, skip main copy loop if
		 * so
		 */
		LLVMPositionBuilderAtEnd(ctx->builder, skip_loop_bb);
		LLVMValueRef cond = LLVMBuildICmp(ctx->builder, LLVMIntULT,
			seq_len, LLVMConstInt(ctx->i32t, seq->elems_per_word, 0),
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
	LLVMTypeRef seq_type = llvm_value_type(ctx, seq->elem_type);
	LLVMValueRef seq_mem = build_local_storage(ctx, seq_type,
		LLVMConstInt(ctx->i32t, seq->max_elems, 0), "inlseq.mem");
	LLVMValueRef seq_pos, counter;
	seq_pos = LLVMBuildPhi(ctx->builder, ctx->i32t, "loop.seqpos");
	counter = LLVMBuildPhi(ctx->builder, ctx->i32t, "loop.ctr");
	LLVMAddIncoming(seq_pos, &upos, &before_loop, 1);
	LLVMAddIncoming(counter, &ctx->zero, &before_loop, 1);

	if(seq->bits_per_elem == BITS_PER_WORD) {
		/* full word case */
		LLVMValueRef item;
		build_read_ipc_parameter_ixval(ctx, &item, seq->elem_type,
			seq_pos);
		LLVMBuildStore(ctx->builder, item,
			LLVMBuildGEP(ctx->builder, seq_mem, &counter, 1,
				"seq.mem.at"));
	} else {
		/* many-per-word (packed) case. */
		LLVMValueRef word = build_utcb_load(ctx, seq_pos, "seq.limb");
		for(int i=0; i<seq->elems_per_word; i++) {
			int downshift = seq->bits_per_elem * i;
			LLVMValueRef item = LLVMBuildTrunc(ctx->builder,
				i == 0 ? word : LLVMBuildLShr(ctx->builder, word,
					LLVMConstInt(ctx->i32t, downshift, 0),
					"seq.limb.shifted"),
				seq_type, "seq.limb.trunc");
			LLVMValueRef ix = LLVMBuildAdd(ctx->builder, counter,
				LLVMConstInt(ctx->i32t, i, 0), "seq.limb.ix");
			LLVMBuildStore(ctx->builder, item,
				LLVMBuildGEP(ctx->builder, seq_mem, &ix, 1,
					"seq.limb.at"));
		}
	}

	LLVMValueRef next_counter, next_sp;
	next_counter = LLVMBuildAdd(ctx->builder, counter,
		LLVMConstInt(ctx->i32t, seq->elems_per_word, 0),
		"loop.ctr.next");
	next_sp = LLVMBuildAdd(ctx->builder, seq_pos,
		LLVMConstInt(ctx->i32t, 1, 0), "loop.seqpos.next");
	LLVMBasicBlockRef this_bb = LLVMGetInsertBlock(ctx->builder);
	LLVMAddIncoming(counter, &next_counter, &this_bb, 1);
	LLVMAddIncoming(seq_pos, &next_sp, &this_bb, 1);
	if(seq->elems_per_word == 1) {
		LLVMAddIncoming(ret, &next_sp, &this_bb, 1);
	}
	LLVMValueRef exit_cond = LLVMBuildICmp(ctx->builder,
		LLVMIntULT, next_counter, seq_len, "loop.nextp");
	LLVMBuildCondBr(ctx->builder, exit_cond, loop_bb,
		seq->elems_per_word > 1 ? odd_tail_bb : exit_bb);

	if(seq->elems_per_word > 1) {
		/* tail of a packed sequence. */
		LLVMPositionBuilderAtEnd(ctx->builder, odd_tail_bb);
		LLVMValueRef odd_offs = LLVMBuildPhi(ctx->builder, ctx->i32t, "odd.off"),
			odd_seqpos = LLVMBuildPhi(ctx->builder, ctx->i32t, "odd.sp"),
			wordval = build_utcb_load(ctx, odd_seqpos, "tail.word");
		LLVMAddIncoming(odd_offs, &ctx->zero, &skip_loop_bb, 1);
		LLVMAddIncoming(odd_seqpos, &upos, &skip_loop_bb, 1);
		LLVMAddIncoming(odd_offs, &next_counter, &loop_bb, 1);
		LLVMAddIncoming(odd_seqpos, &next_sp, &loop_bb, 1);
		LLVMAddIncoming(ret, &odd_seqpos, &odd_tail_bb, 1);
		LLVMValueRef sw = LLVMBuildSwitch(ctx->builder,
			LLVMBuildAnd(ctx->builder, seq_len,
				LLVMConstInt(ctx->i32t, seq->elems_per_word - 1, 0),
				"odd.len"),
			exit_bb, seq->elems_per_word - 1);

		for(int i=seq->elems_per_word - 1; i>0; i--) {
			LLVMBasicBlockRef bb = LLVMAppendBasicBlockInContext(
				ctx->ctx, fn, tmp_f(ctx->pr, "inlseq.odd.%dcase", i));
			if(i != seq->elems_per_word - 1) LLVMBuildBr(ctx->builder, bb);
			LLVMPositionBuilderAtEnd(ctx->builder, bb);
			LLVMAddCase(sw, LLVMConstInt(ctx->i32t, i, 0), bb);
			LLVMValueRef limb = LLVMBuildTrunc(ctx->builder,
				LLVMBuildLShr(ctx->builder, wordval,
					LLVMConstInt(ctx->i32t, (i - 1) * seq->bits_per_elem, 0),
					tmp_f(ctx->pr, "odd.c%d.shifted", i)),
				seq_type, tmp_f(ctx->pr, "odd.c%d.limb", i));
			LLVMValueRef offs = LLVMBuildAdd(ctx->builder, odd_offs,
				LLVMConstInt(ctx->i32t, i, 0), "odd.limb.offs");
			LLVMBuildStore(ctx->builder, limb,
				LLVMBuildGEP(ctx->builder, seq_mem, &offs, 1,
					"odd.limb.ptr"));
		}

		LLVMValueRef sp_bump = LLVMBuildAdd(ctx->builder, odd_seqpos,
			LLVMConstInt(ctx->i32t, 1, 0), "odd.sp.bump");
		LLVMBasicBlockRef bb = LLVMGetInsertBlock(ctx->builder);
		LLVMAddIncoming(ret, &sp_bump, &bb, 1);
		LLVMBuildBr(ctx->builder, exit_bb);
	}

	LLVMPositionBuilderAtEnd(ctx->builder, exit_bb);

	args[(*arg_pos_p)++] = seq_mem;
	args[(*arg_pos_p)++] = seq_len;

	return ret;
}
