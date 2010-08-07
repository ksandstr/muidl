/*
 * message.c -- encoding and decoding of IDL messages
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

#include <stdbool.h>
#include <libIDL/IDL.h>
#include <llvm-c/Core.h>

#include "muidl.h"
#include "llvmutil.h"


void build_write_ipc_parameter(
	struct llvm_ctx *ctx,
	LLVMValueRef ixval,
	IDL_tree ctyp,
	const LLVMValueRef *val)
{
	if(BITS_PER_WORD < 64 && IS_LONGLONG_TYPE(ctyp)) {
		V high = LLVMBuildLShr(ctx->builder, val[0],
				LLVMConstInt(LLVMInt64TypeInContext(ctx->ctx), 32, 0),
				"i64.high"),
			low = LLVMBuildTrunc(ctx->builder, val[0], ctx->wordt, "i64.low");
		high = LLVMBuildTrunc(ctx->builder, high, ctx->wordt, "i64.high.tr");
		/* little end first. */
		LLVMBuildStore(ctx->builder, low,
			UTCB_ADDR_VAL(ctx, ixval, "i64.mr.low"));
		LLVMBuildStore(ctx->builder, high,
			UTCB_ADDR_VAL(ctx, LLVMBuildAdd(ctx->builder, ixval,
				CONST_INT(1), "foo"), "i64.mr.high"));
	} else if(IS_LONGDOUBLE_TYPE(ctyp)) {
		abort();		/* TODO */
	} else if(IS_MAPGRANT_TYPE(ctyp)) {
		for(int i=0; i<2; i++) {
			LLVMBuildStore(ctx->builder,
				LLVMBuildLoad(ctx->builder,
					LLVMBuildStructGEP(ctx->builder, val[0], i,
						tmp_f(ctx->pr, "mg.field%d.ptr", i)),
					tmp_f(ctx->pr, "mg.field%d.val", i)),
				UTCB_ADDR_VAL(ctx, LLVMBuildAdd(ctx->builder, ixval, CONST_INT(i),
					"mg.store.offs"), "mg.store.addr"));
		}
	} else if(IDL_NODE_TYPE(ctyp) == IDLN_TYPE_STRUCT) {
		encode_packed_struct(ctx, ixval, NULL, ctyp, val[0]);
	} else if(is_value_type(ctyp)) {
		/* the other value types */
		LLVMValueRef reg;
		if(is_integral_type(ctyp)) {
			reg = WORD(val[0]);
		} else {
			/* TODO: type promotions for other single-word encodings */
			NOTDEFINED(ctyp);
		}
		LLVMBuildStore(ctx->builder, reg,
			UTCB_ADDR_VAL(ctx, ixval, "store.mr.addr"));
	} else {
		/* TODO: add nicer things */
		NOTDEFINED(ctyp);
	}
}


/* turn a sequence of out-parameters into L4 instructions that store the
 * appropriate values, including the message tag, in the message registers.
 */
void build_msg_encoder(
	struct llvm_ctx *ctx,
	const struct message_info *msg,
	const LLVMValueRef *args,
	bool is_out_half)
{
	if(is_out_half && msg->ret_type != NULL) {
		int first_reg = msg->sublabel == NO_SUBLABEL ? 1 : 2;
		if(!msg->ret_by_ref) {
			/* by-val types are always value types, which are used by value by
			 * build_write_ipc_parameter()
			 */
			assert(is_value_type(msg->ret_type));
			build_write_ipc_parameter(ctx, CONST_INT(first_reg),
				msg->ret_type, &args[0]);
		} else {
			/* must load value types. */
			V raw;
			if(is_value_type(msg->ret_type)) {
				raw = LLVMBuildLoad(ctx->builder, args[0], "retp.deref");
			} else {
				/* keep the pointerishness. */
				raw = args[0];
			}
			build_write_ipc_parameter(ctx, CONST_INT(first_reg),
				msg->ret_type, &raw);
		}
	}

	/* untyped words. */
	GLIST_FOREACH(cur, msg->untyped) {
		const struct msg_param *u = cur->data;
		IDL_tree type = u->X.untyped.type;
		const int first_reg = u->X.untyped.first_reg;
		assert(IDL_PARAM_DCL(u->param_dcl).attr != IDL_PARAM_IN);
//		printf("param %p: arg_ix %d, regs [%d..%d], argval %p, type <%s>\n",
//			u, u->arg_ix, first_reg, last_reg, args[u->arg_ix],
//			IDL_NODE_TYPE_NAME(type));
		if(is_value_type(type)) {
			V raw;
			if(is_out_half) {
				/* flatten the pointer. */
				raw = LLVMBuildLoad(ctx->builder, args[u->arg_ix], "outp.flat");
			} else {
				/* already flat. */
				raw = args[u->arg_ix];
			}
			build_write_ipc_parameter(ctx, CONST_INT(first_reg), type, &raw);
		} else if(IS_MAPGRANT_TYPE(type) || is_rigid_type(ctx->ns, type)) {
			/* TODO: mapgrantitems are currently always untyped, so we pass
			 * them here. however whether they're map-item literals or mapping
			 * or granting items should depend on a [map] or [grant] attribute
			 * on the parameter declaration.
			 */
			build_write_ipc_parameter(ctx, CONST_INT(first_reg), type,
				&args[u->arg_ix]);
		} else {
			/* anything else... shouldn't appear! */
			/* FIXME: structs, arrays, unions before here though. */
			NOTDEFINED(type);
		}
	}

	/* inline sequences */
	ctx->inline_seq_pos = CONST_INT(msg->tag_u + 1);
	GLIST_FOREACH(cur, msg->seq) {
		const struct msg_param *seq = cur->data;
		ctx->inline_seq_pos = build_encode_inline_sequence(ctx,
			args[seq->arg_ix], args[seq->arg_ix + 1], seq,
			ctx->inline_seq_pos, g_list_next(cur) == NULL);
	}

	/* TODO: typed words (strings, sequences, long structs, unions, arrays,
	 * and wide strings; also map & grant items)
	 *
	 * NOTE: when encoding out-sequences, the second argument is a pointer to
	 * the length value and should be flattened before the call to
	 * build_write_ipc_parameter().
	 */

	/* epilogue */
	/* flags = 0, u = inline_seq_pos - 1, label = 0, t = 0 */
	V label = LLVMBuildShl(ctx->builder, CONST_INT(msg->label),
		CONST_INT(16), "reply.label"),
	  u_val = LLVMBuildSub(ctx->builder, ctx->inline_seq_pos,
				CONST_INT(1), "reply.u"),
	  t_val = LLVMBuildShl(ctx->builder, CONST_WORD(msg->tag_t),
				CONST_INT(6), "reply.t");
	V tag = LLVMBuildOr(ctx->builder, label,
				LLVMBuildOr(ctx->builder, u_val, t_val, "reply.ut.or"),
				"reply.tag");
	if(msg->sublabel != NO_SUBLABEL) {
		LLVMBuildStore(ctx->builder, CONST_WORD(msg->sublabel),
			UTCB_ADDR_VAL(ctx, CONST_INT(1), "mr1.addr"));
	}
	branch_set_phi(ctx, ctx->reply_tag, tag);
	LLVMBuildBr(ctx->builder, ctx->reply_bb);
}
