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

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <errno.h>
#include <glib.h>
#include <libIDL/IDL.h>
#include <llvm-c/Core.h>

#include "muidl.h"
#include "llvmutil.h"
#include "l4x2.h"


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


/* FIXME: move this into another file */
static LLVMValueRef get_strlen_fn(struct llvm_ctx *ctx)
{
	V fn = LLVMGetNamedFunction(ctx->module, "strlen");
	if(fn != NULL) return fn;

	T charptr = LLVMPointerType(LLVMInt8TypeInContext(ctx->ctx), 0),
		sizet = ctx->wordt;		/* FIXME: get from ABI; should be size_t */
	T fntype = LLVMFunctionType(sizet, &charptr, 1, 0);
	fn = LLVMAddFunction(ctx->module, "strlen", fntype);

	return fn;
}


LLVMValueRef build_msg_encoder(
	struct llvm_ctx *ctx,
	const struct message_info *msg,
	const LLVMValueRef *ret_args,
	const LLVMValueRef *args,
	bool is_out_half)
{
	if(is_out_half && msg->ret_type != NULL) {
		assert(ret_args != NULL);
		int first_reg = msg->sublabel == NO_SUBLABEL ? 1 : 2;
		if(!msg->ret_by_ref) {
			assert(is_value_type(msg->ret_type));
			build_write_ipc_parameter(ctx, CONST_INT(first_reg),
				msg->ret_type, ret_args);
		} else {
			V raw;
			if(is_value_type(msg->ret_type)) {
				/* must load value types for bwip() */
				raw = LLVMBuildLoad(ctx->builder, ret_args[0], "retp.deref");
			} else {
				/* keep the pointerishness. */
				raw = ret_args[0];
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
		bool inout = u->param_dcl != NULL
			&& IDL_PARAM_DCL(u->param_dcl).attr == IDL_PARAM_INOUT;
#if 0
		printf("param `%s' (%p): arg_ix %d, regs [%d..%d], argval %p, type <%s>\n",
			u->name, u, u->arg_ix, first_reg, u->X.untyped.last_reg,
			args[u->arg_ix], IDL_NODE_TYPE_NAME(type));
#endif
		if(is_value_type(type)) {
			V raw;
			if(is_out_half || inout) {
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
		V num_items = args[seq->arg_ix + 1];
		if(IDL_PARAM_DCL(seq->param_dcl).attr != IDL_PARAM_IN) {
			num_items = LLVMBuildLoad(ctx->builder, num_items,
				"seq.num.items");
		}
		ctx->inline_seq_pos = build_encode_inline_sequence(ctx,
			args[seq->arg_ix], num_items, seq,
			ctx->inline_seq_pos, g_list_next(cur) == NULL);
	}

	/* long items */
	V t_pos = ctx->inline_seq_pos;
	GLIST_FOREACH(cur, msg->_long) {
		struct msg_param *l = cur->data;
		IDL_tree type = l->X._long.type;
		V words[2];
		if(IDL_NODE_TYPE(type) == IDLN_TYPE_STRING) {
			V ptr = args[l->arg_ix];
			V len = LLVMBuildCall(ctx->builder, get_strlen_fn(ctx),
				&ptr, 1, tmp_f(ctx->pr, "strlen.%s", l->name));
			build_simple_string_item(ctx, words, args[l->arg_ix], len,
				NULL);
		} else {
			/* TODO: map/grant items, sequences, structs, unions, arrays, wide
			 * strings
			 */
			NOTDEFINED(type);
		}
		for(int i=0; i<2; i++) {
			V t_addr = UTCB_ADDR_VAL(ctx, t_pos,
				tmp_f(ctx->pr, "stritem.w%d.ptr", i));
			LLVMBuildStore(ctx->builder, words[i], t_addr);
			t_pos = LLVMBuildAdd(ctx->builder, t_pos, CONST_INT(1),
				"t.pos");
		}
	}

	/* NOTE: when encoding out-sequences, the second argument is a pointer to
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
	return tag;
}


static LLVMValueRef build_ipc_input_val(struct llvm_ctx *ctx, int mr)
{
	if(mr == 0) return ctx->tag;
	else if(mr == 1) return ctx->mr1;
	else if(mr == 2) return ctx->mr2;
	else {
		return LLVMBuildLoad(ctx->builder,
			UTCB_ADDR_VAL(ctx, CONST_INT(MR_OFFSET(mr)), "mr.addr"),
			tmp_f(ctx->pr, "mr%d", mr));
	}
}


/* (see build_read_ipc_parameter_ixval() comment in muidl.h)
 *
 * (also note that this function is most likely a pointless optimization as LLVM
 * seems quite capable of recognizing values that don't need to be computed at
 * run time.)
 */
static void build_read_ipc_parameter(
	struct llvm_ctx *ctx,
	LLVMValueRef *dst,
	IDL_tree ctyp,
	int first_mr)
{
	if(IS_LONGLONG_TYPE(ctyp)) {
		/* unpack a two-word parameter. */
		LLVMValueRef low = build_ipc_input_val(ctx, first_mr),
			high = build_ipc_input_val(ctx, first_mr + 1);
		LLVMTypeRef i64t = LLVMInt64TypeInContext(ctx->ctx);
		low = LLVMBuildBitCast(ctx->builder, low, i64t,
			"longparm.lo.cast");
		high = LLVMBuildBitCast(ctx->builder, high, i64t,
			"longparm.hi.cast");
		dst[0] = LLVMBuildOr(ctx->builder, low,
				LLVMBuildShl(ctx->builder, high, LLVMConstInt(i64t, 32, 0),
					"longparm.hi.shift"),
				"longparm.value");
	} else if(IS_LONGDOUBLE_TYPE(ctyp)) {
		fprintf(stderr, "%s: long doubles are TODO\n", __func__);
		abort();
	} else if(IS_MAPGRANT_TYPE(ctyp)) {
		dst[0] = build_local_storage(ctx, ctx->mapgrant, NULL,
			"mapgrant.mem");
		LLVMBuildStore(ctx->builder, build_ipc_input_val(ctx, first_mr),
			LLVMBuildStructGEP(ctx->builder, dst[0], 0, "mgitem.info.ptr"));
		LLVMBuildStore(ctx->builder, build_ipc_input_val(ctx, first_mr + 1),
			LLVMBuildStructGEP(ctx->builder, dst[0], 1, "mgitem.fpage.ptr"));
	} else if(is_value_type(ctyp)) {
		/* appropriate for all value types. */
		dst[0] = LLVMBuildTruncOrBitCast(ctx->builder,
			build_ipc_input_val(ctx, first_mr),
			llvm_value_type(ctx, ctyp), "shortparm");
	} else {
		build_read_ipc_parameter_ixval(ctx, dst, ctyp,
			CONST_INT(first_mr));
	}
}


/* FIXME: move this out */
static bool pass_param_by_value(IDL_tree pdecl)
{
	return pdecl != NULL
		&& IDL_PARAM_DCL(pdecl).attr == IDL_PARAM_IN
		&& is_value_type(get_type_spec(IDL_PARAM_DCL(pdecl).param_type_spec));
}


void build_msg_decoder(
	struct llvm_ctx *ctx,
	LLVMValueRef *ret_args,
	LLVMValueRef *args,
	const struct message_info *msg,
	const struct stritem_info *stritems,
	bool is_out_half)
{
	assert(is_out_half || msg->ret_type == NULL);

	if(is_out_half && msg->ret_type != NULL) {
		assert(ret_args != NULL);
		/* decode return value for the stub post-ipc part */
		int ret_offs = msg->sublabel == NO_SUBLABEL ? 1 : 2;
		/* build_read_ipc_parameter() uses a pre-set pointer value for storing
		 * decoded rigids, which we have, so provide it.
		 */
		V tmp[2] = { ret_args[0], NULL };
		build_read_ipc_parameter(ctx, tmp, msg->ret_type, ret_offs);
		if(is_value_type(msg->ret_type)) {
			assert(tmp[0] != ret_args[0]);
			LLVMBuildStore(ctx->builder, tmp[0], ret_args[0]);
		}
	}

	GLIST_FOREACH(cur, msg->untyped) {
		struct msg_param *u = cur->data;
		IDL_tree type = u->X.untyped.type;
		V tmp[2] = { args[u->arg_ix], NULL };
		build_read_ipc_parameter(ctx, tmp, type, u->X.untyped.first_reg);
		if(is_value_type(type)) {
			if(pass_param_by_value(u->param_dcl)) args[u->arg_ix] = tmp[0];
			else LLVMBuildStore(ctx->builder, tmp[0], args[u->arg_ix]);
		} else if(tmp[0] != args[u->arg_ix]) {
			/* storage was allocated, possibally. */
			args[u->arg_ix] = tmp[0];
		}
	}

	GLIST_FOREACH(cur, msg->seq) {
		struct msg_param *seq = cur->data;
		BB err_bb = get_msgerr_bb(ctx);
		V *seq_args = &args[seq->arg_ix];
		if(!is_out_half) {
			/* the dispatcher allocates buffers on the stack. for stubs, the
			 * caller has filled in the arg array.
			 *
			 * FIXME: this deviates from the convention used by
			 * build_read_ipc_parameter(), where passing NULL in seq_args[]
			 * here causes the parameter decoding function to allocate local
			 * storage. push this part into the build_decode_inline_sequence().
			 */
			seq_args[0] = build_local_storage(ctx,
				llvm_rigid_type(ctx, seq->X.seq.elem_type),
				CONST_INT(seq->X.seq.max_elems), "inlseq.mem");
			seq_args[1] = build_local_storage(ctx, ctx->i32t,
				NULL, "inlseq.len.ptr");
		}
		V new_upos = build_decode_inline_sequence(ctx,
			seq_args, seq, ctx->inline_seq_pos,
			g_list_next(cur) == NULL, ctx->errval_phi, err_bb);
		ctx->inline_seq_pos = new_upos;
	}

	assert(msg->_long == NULL || stritems != NULL);
	int lp_offset = -1;
	/* this is more safe than clever. it could be made cleverer while not
	 * making it less safe.
	 */
	ctx->tpos = LLVMBuildAdd(ctx->builder, build_u_from_tag(ctx, ctx->tag),
		CONST_WORD(1), "tpos");
	ctx->tmax = LLVMBuildAdd(ctx->builder, ctx->tpos,
		build_t_from_tag(ctx, ctx->tag), "tmax");
	GLIST_FOREACH(cur, msg->_long) {
		struct msg_param *lp = cur->data;
		lp_offset++;
		/* every long parameter is carried in a string item. therefore there
		 * must be at least one; make sure this is true, and fuck off into the
		 * msgerr block if it's not. (the length function returns tpos > tmax +
		 * 1 to indicate this.)
		 */
		IDL_tree type = lp->X._long.type;
		/* TODO: get EINVAL from µiX header rather than <errno.h> */
		LLVMValueRef einval = LLVMConstInt(ctx->i32t, -EINVAL, 0),
			tmax_plus_one = LLVMBuildAdd(ctx->builder, ctx->tmax,
				LLVMConstInt(ctx->i32t, 1, 0), "tmax.plus.one");
		LLVMBasicBlockRef msgerr_bb = get_msgerr_bb(ctx),
			pre_ok_bb = add_sibling_block(ctx, "stritem.preok"),
			cont_bb = add_sibling_block(ctx, "stritem.cont");
		/* precondition: tpos <= tmax + 1 */
		LLVMValueRef tpos_pc = LLVMBuildICmp(ctx->builder, LLVMIntULE,
			ctx->tpos, tmax_plus_one, "stritem.precond");
		branch_set_phi(ctx, ctx->errval_phi, einval);
		LLVMBuildCondBr(ctx->builder, tpos_pc, pre_ok_bb, msgerr_bb);

		/* call lenfn, branch off to msgerr if retval indicates failure */
		LLVMPositionBuilderAtEnd(ctx->builder, pre_ok_bb);
		LLVMValueRef item_len_bytes = NULL;
		ctx->tpos = build_recv_stritem_len(ctx, &item_len_bytes, ctx->tpos);
		LLVMValueRef fail_cond = LLVMBuildICmp(ctx->builder, LLVMIntUGT,
			ctx->tpos, tmax_plus_one, "stritem.fail.cond");
		LLVMAddIncoming(ctx->errval_phi, &einval, &pre_ok_bb, 1);
		LLVMBuildCondBr(ctx->builder, fail_cond, msgerr_bb, cont_bb);

		/* the actual parameter decoding. */
		LLVMPositionBuilderAtEnd(ctx->builder, cont_bb);
		switch(IDL_NODE_TYPE(type)) {
			case IDLN_TYPE_STRING: {
				/* terminate. */
				LLVMBuildStore(ctx->builder,
					LLVMConstInt(LLVMInt8TypeInContext(ctx->ctx), 0, 0),
					LLVMBuildGEP(ctx->builder, stritems[lp_offset].memptr,
						&item_len_bytes, 1, "str.nullpo"));	/* ガ！ */
				args[lp->arg_ix] = stritems[lp_offset].memptr;
				break;
			}

			case IDLN_TYPE_SEQUENCE: {
				/* two arguments. */
				IDL_tree typ = get_type_spec(
					IDL_TYPE_SEQUENCE(type).simple_type_spec);
				/* TODO: use a llvm_rigid_type() instead! */
				LLVMTypeRef itemtype = llvm_value_type(ctx, typ);
				LLVMValueRef ptr = LLVMBuildPointerCast(ctx->builder,
					stritems[lp_offset].memptr,
					LLVMPointerType(itemtype, 0),
					"seq.ptr");
				LLVMValueRef len = LLVMBuildUDiv(ctx->builder,
					item_len_bytes,
					LLVMBuildTruncOrBitCast(ctx->builder,
						LLVMSizeOf(itemtype), ctx->i32t, "seq.item.len.cast"),
					"seq.len");
				args[lp->arg_ix + 0] = ptr;
				/* the length part is passed like a value type. */
				if(IDL_PARAM_DCL(lp->param_dcl).attr == IDL_PARAM_IN) {
					args[lp->arg_ix + 1] = len;
				} else {
					LLVMBuildStore(ctx->builder, len, args[lp->arg_ix + 1]);
				}
				break;
			}

			case IDLN_TYPE_WIDE_STRING:
			case IDLN_TYPE_STRUCT:
			case IDLN_TYPE_UNION:
			case IDLN_TYPE_ARRAY:
				/* TODO */
				fprintf(stderr, "%s: <%s> as l-param not implemented (yet)\n",
					__func__, IDL_NODE_TYPE_NAME(type));
				abort();

			default:
				NOTDEFINED(type);
		}
	}
}
