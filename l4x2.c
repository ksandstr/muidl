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

#include <stdio.h>
#include <stdlib.h>
#include <llvm-c/Core.h>

#include "defs.h"
#include "llvmutil.h"
#include "l4x2.h"


LLVMValueRef build_utcb_get(struct llvm_ctx *ctx)
{
	LLVMTypeRef fntype = LLVMFunctionType(ctx->wordt, NULL, 0, 0);
	LLVMValueRef func = LLVMConstInlineAsm(fntype, "movl %gs:0,$0\n",
			"=r,~{dirflag},~{fpsr},~{flags}", 0, 0);
	LLVMValueRef call = LLVMBuildCall(ctx->builder, func, NULL, 0, "utcbget");
	return LLVMBuildIntToPtr(ctx->builder, call,
		LLVMPointerType(ctx->wordt, 0), "utcb.wordp");
}


LLVMValueRef build_l4_ipc_call(struct llvm_ctx *ctx,
	V arg_to, V arg_timeouts, V arg_fromspec, V arg_mr0,
	V *from_p, V *mr1_p, V *mr2_p)
{
	T params[5];
	for(int i=0; i < G_N_ELEMENTS(params); i++) params[i] = ctx->wordt;
	T ipc_result_type = LLVMStructTypeInContext(ctx->ctx, params, 5, 0),
		ipc_type = LLVMFunctionType(ipc_result_type, params, 5, 0);
	/* protect %ebp because LLVM won't. how come something like this persists
	 * for over a decade anyway?
	 */
	char *asmstr =
		"  pushl %ebp\n"
		"\tcall __L4_Ipc\n"
		"\tmovl %ebp, %ecx\n"
		"\tpopl %ebp\n";
	/* (we disregard the UTCB output value from Ipc, because it radically
	 * breaks the compiler somehow somewhere. instead LLVM will reload UTCB,
	 * leaving a spot to optimize for a few cycles' worth.)
	 */
	char *constraint = "={eax},={esi},={ebx},={ecx},={edi}"
		",{eax},{ecx},{edx},{esi},{edi}"
		",~{dirflag},~{fpsr},~{flags},~{memory}";
	V fn = LLVMGetInlineAsm(ipc_type, asmstr, strlen(asmstr),
			constraint, strlen(constraint), 1, 0, LLVMInlineAsmDialectATT),
		args[5] = {
			arg_to, arg_timeouts, arg_fromspec, arg_mr0,
			LLVMBuildPtrToInt(ctx->builder, ctx->utcb, ctx->wordt, "ipc.utcb.in"),
		},
		result = LLVMBuildCall(ctx->builder, fn, args, 5, "ipc.result");

	if(from_p != NULL) {
		*from_p = LLVMBuildExtractValue(ctx->builder, result, 0, "ipc.from.out");
	}
	if(mr1_p != NULL) {
		*mr1_p = LLVMBuildExtractValue(ctx->builder, result, 2, "ipc.mr1.out");
	}
	if(mr2_p != NULL) {
		*mr2_p = LLVMBuildExtractValue(ctx->builder, result, 3, "ipc.mr2.out");
	}
#if 0
	/* re-enabling this smegfaults the compiler. so don't do that. it should
	 * work but doesn't.
	 */
	ctx->utcb = LLVMBuildExtractValue(ctx->builder, result, 4, "ipc.utcb.out");
#endif
	return LLVMBuildExtractValue(ctx->builder, result, 1, "ipc.mr0.out");
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
	V fn = LLVMGetNamedFunction(ctx->module, "__muidl_get_stritem_len");
	if(fn == NULL) {
		T types[] = { ctx->voidptrt, LLVMPointerType(ctx->i32t, 0) },
			fntype = LLVMFunctionType(ctx->i32t, types, 2, 0);
		fn = LLVMAddFunction(ctx->module, "__muidl_get_stritem_len", fntype);
	}
	return fn;
}


LLVMValueRef build_recv_stritem_len(
	struct llvm_ctx *ctx, LLVMValueRef *nullpos_p, LLVMValueRef tpos)
{
	if(ctx->tpos_mem == NULL) {
		ctx->tpos_mem = build_local_storage(ctx, ctx->i32t, NULL, "tpos.mem");
	}
	LLVMBuildStore(ctx->builder, tpos, ctx->tpos_mem);
	V args[2] = { LLVMBuildPointerCast(ctx->builder, ctx->utcb, ctx->voidptrt, "utcb.voidp"), ctx->tpos_mem };
	*nullpos_p = LLVMBuildCall(ctx->builder, get_stritem_len_fn(ctx), args, 2, "stritemlen.rval");
	return LLVMBuildLoad(ctx->builder, ctx->tpos_mem, "tpos.new");
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
