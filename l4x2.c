/*
 * l4x2.c -- utility functions related to the L4.X2 microkernel
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

#include <llvm-c/Core.h>

#include "muidl.h"
#include "llvmutil.h"
#include "l4x2.h"


LLVMValueRef build_utcb_get(struct llvm_ctx *ctx)
{
	LLVMTypeRef fntype = LLVMFunctionType(ctx->wordt, NULL, 0, 0);
	LLVMValueRef func = LLVMConstInlineAsm(fntype, "movl %gs:0,$0\n",
			"=r,~{dirflag},~{fpsr},~{flags}", 0, 0);
	LLVMValueRef call = LLVMBuildCall(ctx->builder, func, NULL, 0, "utcbget");
	LLVMSetTailCall(call, 1);
	return LLVMBuildIntToPtr(ctx->builder, call,
		LLVMPointerType(ctx->wordt, 0), "utcb.wordp");
}


LLVMValueRef build_l4_ipc_call(
	struct llvm_ctx *ctx,
	LLVMValueRef arg_to,
	LLVMValueRef arg_timeouts,
	LLVMValueRef arg_fromspec,
	LLVMValueRef arg_mr0,
	LLVMValueRef *from_p,
	LLVMValueRef *mr1_p,
	LLVMValueRef *mr2_p)
{
	LLVMTypeRef params[5];
	for(int i=0; i<5; i++) params[i] = ctx->wordt;
	LLVMTypeRef ipc_result_type = LLVMStructTypeInContext(ctx->ctx,
		params, 4, 0);
	LLVMTypeRef ipc_type = LLVMFunctionType(ipc_result_type,
		params, 5, 0);
	LLVMValueRef fn = LLVMConstInlineAsm(ipc_type,
		"call __L4_Ipc\n",
		"={ax},={si},={bx},={bp},{ax},{cx},{dx},{si},{di},~{dirflag},~{fpsr},~{flags}",
		1, 0);
	LLVMValueRef args[5] = {
		arg_to, arg_timeouts, arg_fromspec, arg_mr0,
		LLVMBuildPtrToInt(ctx->builder, ctx->utcb, ctx->wordt,
			"l4ipc.utcb"),
	};
	LLVMValueRef result = LLVMBuildCall(ctx->builder, fn, args, 5, "l4ipc");
	LLVMSetTailCall(result, 1);

	if(from_p != NULL) {
		*from_p = LLVMBuildExtractValue(ctx->builder, result, 0, "from");
	}
	if(mr1_p != NULL) {
		*mr1_p = LLVMBuildExtractValue(ctx->builder, result, 2, "mr1");
	}
	if(mr2_p != NULL) {
		*mr2_p = LLVMBuildExtractValue(ctx->builder, result, 3, "mr2");
	}
	return LLVMBuildExtractValue(ctx->builder, result, 1, "mr0");
}


LLVMValueRef build_utcb_load(
	struct llvm_ctx *ctx,
	LLVMValueRef ix,
	const char *name)
{
	return LLVMBuildLoad(ctx->builder,
		LLVMBuildGEP(ctx->builder, ctx->utcb, &ix, 1, "utcb.addr.in"),
		name);
}


LLVMValueRef build_u_from_tag(
	struct llvm_ctx *ctx,
	LLVMValueRef mr0)
{
	LLVMValueRef u = LLVMBuildAnd(ctx->builder, mr0,
		CONST_WORD(0x3f), "tag.u");
	return LLVMBuildTruncOrBitCast(ctx->builder, u, ctx->i32t, "tag.u.int");
}


LLVMValueRef build_t_from_tag(struct llvm_ctx *ctx, LLVMValueRef mr0)
{
	LLVMValueRef t = LLVMBuildAnd(ctx->builder,
		LLVMBuildLShr(ctx->builder, mr0, CONST_WORD(6), "tag.t.raw"),
		CONST_WORD(0x3f), "tag.t");
	return LLVMBuildTruncOrBitCast(ctx->builder, t, ctx->i32t, "tag.t.int");
}


LLVMValueRef build_label_from_tag(struct llvm_ctx *ctx, LLVMValueRef tag) {
	return LLVMBuildLShr(ctx->builder, tag, CONST_WORD(16), "tag.label");
}


void build_simple_string_item(
	struct llvm_ctx *ctx,
	LLVMValueRef *dest,
	LLVMValueRef data_ptr,
	LLVMValueRef data_len,
	LLVMValueRef cache_hint)
{
	if(cache_hint == NULL) cache_hint = CONST_WORD(0);

	dest[0] = LLVMBuildOr(ctx->builder,
		LLVMBuildShl(ctx->builder, data_len, CONST_WORD(10),
			"stritem.simple.len.shl"),
		LLVMBuildShl(ctx->builder, cache_hint, CONST_WORD(1),
			"stritem.simple.ch.shl"),
		"stritem.simple.info");
	dest[1] = LLVMBuildPtrToInt(ctx->builder, data_ptr, ctx->wordt,
		"stritem.simple.ptr");
}


void build_mapgrant_item(
	struct llvm_ctx *ctx,
	LLVMValueRef *dest,
	LLVMValueRef send_base,
	LLVMValueRef fpage,
	LLVMValueRef is_grant)
{
	V grant_cond = LLVMBuildICmp(ctx->builder, LLVMIntNE,
		CONST_WORD(0), is_grant, "is.grant.cond");
	dest[0] = LLVMBuildOr(ctx->builder, send_base,
		LLVMBuildSelect(ctx->builder, grant_cond,
			CONST_WORD(0xa), CONST_WORD(0x8), "mgitem.type.bits"),
		"mapgrant.info.word");
	dest[1] = fpage;
}


static LLVMValueRef get_stritem_len_fn(struct llvm_ctx *ctx)
{
	if(ctx->stritem_len_fn != NULL) return ctx->stritem_len_fn;

	/* returns (i32 len, i32 new_tpos)
	 * params (word *utcbptr, i32 tpos)
	 *
	 * when return value "new_tpos" > tmax + 1, the result is invalid. the function
	 * should also not be called when tpos > tmax + 1.
	 */
	LLVMTypeRef ret_types[2] = { ctx->i32t, ctx->i32t },
		parm_types[2] = { LLVMPointerType(ctx->wordt, 0), ctx->i32t },
		ret_type = LLVMStructTypeInContext(ctx->ctx, ret_types, 2, 0),
		fn_type = LLVMFunctionType(ret_type, parm_types, 2, 0);
	LLVMValueRef fn = LLVMAddFunction(ctx->module, "__muidl_get_stritem_len",
		fn_type);
	LLVMSetVisibility(fn, LLVMHiddenVisibility);
	LLVMSetLinkage(fn, LLVMInternalLinkage);
	V fn_args[2];
	LLVMGetParams(fn, fn_args);
	LLVMAddAttribute(fn_args[0], LLVMNoCaptureAttribute);
	for(int i=0; i<2; i++) {
		LLVMAddAttribute(fn_args[i], LLVMInRegAttribute);
	}
	ctx->stritem_len_fn = fn;

	LLVMBuilderRef old_builder = ctx->builder;
	ctx->builder = LLVMCreateBuilderInContext(ctx->ctx);
	LLVMBasicBlockRef entry_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn,
			"EntryBlock"),
		loop_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "loop"),
		valid_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "valid"),
		exit_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "exit");

	LLVMPositionBuilderAtEnd(ctx->builder, entry_bb);
	LLVMValueRef old_utcb = ctx->utcb, old_tpos = ctx->tpos;
	ctx->utcb = fn_args[0];
	ctx->tpos = fn_args[1];
	LLVMBuildBr(ctx->builder, loop_bb);

	LLVMPositionBuilderAtEnd(ctx->builder, exit_bb);
	LLVMValueRef exit_len_phi = LLVMBuildPhi(ctx->builder, ctx->i32t,
			"exit.len.phi"),
		exit_tpos_phi = LLVMBuildPhi(ctx->builder, ctx->i32t,
			"exit.tpos.phi");
	LLVMValueRef rvals[2] = { exit_len_phi, exit_tpos_phi };
	LLVMBuildAggregateRet(ctx->builder, rvals, 2);

	LLVMPositionBuilderAtEnd(ctx->builder, loop_bb);
	LLVMValueRef len_phi = LLVMBuildPhi(ctx->builder, ctx->i32t, "len.phi"),
		tpos_phi = LLVMBuildPhi(ctx->builder, ctx->i32t, "tpos.phi");
	LLVMAddIncoming(len_phi, &ctx->zero, &entry_bb, 1);
	LLVMAddIncoming(tpos_phi, &ctx->tpos, &entry_bb, 1);
	ctx->tpos = tpos_phi;
	/* test: if *tpos doesn't look like a string item, conk out. */
	LLVMValueRef infoword = build_utcb_load(ctx, ctx->tpos, "si.info");
	LLVMValueRef is_cond = LLVMBuildICmp(ctx->builder, LLVMIntEQ,
		ctx->zero, LLVMBuildAnd(ctx->builder, infoword,
			CONST_WORD(1 << 4), "infoword.si.mask"),
		"infoword.si.cond");
	/* anything + 100 is sure to be > tmax + 1. */
	LLVMValueRef fucked_tpos = LLVMBuildAdd(ctx->builder, tpos_phi,
		CONST_INT(100), "fucked.tpos");
	branch_set_phi(ctx, exit_len_phi, len_phi);
	branch_set_phi(ctx, exit_tpos_phi, fucked_tpos);
	LLVMBuildCondBr(ctx->builder, is_cond, valid_bb, exit_bb);

	LLVMPositionBuilderAtEnd(ctx->builder, valid_bb);
	LLVMValueRef string_length = LLVMBuildTruncOrBitCast(ctx->builder,
			LLVMBuildLShr(ctx->builder, infoword,
				CONST_INT(10), "si.info.len"),
			ctx->i32t, "si.info.len.int"),
		string_j = LLVMBuildTruncOrBitCast(ctx->builder,
			LLVMBuildAnd(ctx->builder, CONST_WORD(0x1f),
				LLVMBuildLShr(ctx->builder, infoword, CONST_WORD(4),
					"si.info.j.shift"),
				"si.info.j.masked"),
			ctx->i32t, "si.info.j"),
		string_c = LLVMBuildTruncOrBitCast(ctx->builder,
			LLVMBuildAnd(ctx->builder, CONST_WORD(1 << 9),
				infoword, "si.info.c.masked"),
			ctx->i32t, "si.info.c.masked.int"),
		c_cond = LLVMBuildICmp(ctx->builder, LLVMIntNE,
			string_c, CONST_WORD(0), "si.info.c.cond"),
		new_len = LLVMBuildAdd(ctx->builder, len_phi,
			LLVMBuildMul(ctx->builder, string_length,
				LLVMBuildAdd(ctx->builder, string_j,
					CONST_INT(1), "j.plus.one"),
				"len.incr"),
			"len.new"),
		new_tpos = LLVMBuildAdd(ctx->builder, ctx->tpos,
			LLVMBuildSelect(ctx->builder, c_cond,
				LLVMBuildAdd(ctx->builder, CONST_INT(2),
					string_j, "cont.tpos.bump"),
				CONST_INT(2), "tpos.bump"),
			"tpos.new");
	LLVMAddIncoming(len_phi, &new_len, &valid_bb, 1);
	LLVMAddIncoming(tpos_phi, &new_tpos, &valid_bb, 1);
	LLVMAddIncoming(exit_len_phi, &new_len, &valid_bb, 1);
	LLVMAddIncoming(exit_tpos_phi, &new_tpos, &valid_bb, 1);
	LLVMBuildCondBr(ctx->builder, c_cond, loop_bb, exit_bb);

	LLVMDisposeBuilder(ctx->builder);
	ctx->builder = old_builder;
	ctx->utcb = old_utcb;
	ctx->tpos = old_tpos;

	return ctx->stritem_len_fn;
}


LLVMValueRef build_recv_stritem_len(
	struct llvm_ctx *ctx,
	LLVMValueRef *nullpos_p,
	LLVMValueRef tpos)
{
	LLVMValueRef args[2] = { ctx->utcb, tpos };
	LLVMValueRef agg = LLVMBuildCall(ctx->builder,
		get_stritem_len_fn(ctx), args, 2, "stritemlen.rval");
	LLVMSetTailCall(agg, 1);
	*nullpos_p = LLVMBuildExtractValue(ctx->builder, agg, 0,
		"stritemlen.rval.len");
	return LLVMBuildExtractValue(ctx->builder, agg, 1,
		"stritemlen.rval.new.tpos");
}


void build_store_received_regs(
	struct llvm_ctx *ctx,
	int min_u,
	LLVMValueRef mr1,
	LLVMValueRef mr2)
{
	if(min_u < 1) {
		LLVMBuildStore(ctx->builder, mr1,
			UTCB_ADDR_VAL(ctx, CONST_INT(MR_OFFSET(1)), "mr1.addr"));
	}
	if(min_u < 2) {
		LLVMBuildStore(ctx->builder, mr2,
			UTCB_ADDR_VAL(ctx, CONST_INT(MR_OFFSET(2)), "mr2.addr"));
	}
}


LLVMValueRef build_store_mrs(
	struct llvm_ctx *ctx,
	LLVMValueRef pos,
	const LLVMValueRef *words,
	int num_words)
{
	for(int i=0; i<num_words; i++) {
		V t_addr = UTCB_ADDR_VAL(ctx, pos, "store.mr.ptr");
		LLVMBuildStore(ctx->builder, words[i], t_addr);
		pos = LLVMBuildAdd(ctx->builder, pos, CONST_INT(1), "t.pos");
	}
	return pos;
}
