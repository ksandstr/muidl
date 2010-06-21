/*
 * dispatch.c -- emit a LLVM tree for an IDL interface
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
#include <stdint.h>
#include <errno.h>

#include <llvm-c/Core.h>
#include <libIDL/IDL.h>

#include "muidl.h"


/* returns ptr to data object.
 *
 * TODO: callsites to this here thing should recycle compatible chunks of
 * memory, rather than allocating their own for each. this is a low-hanging
 * space optimization.
 */
static LLVMValueRef build_local_storage(
	struct llvm_ctx *ctx,
	LLVMTypeRef type,
	LLVMValueRef count,
	const char *name)
{
	LLVMBuilderRef b = LLVMCreateBuilderInContext(ctx->ctx);
	LLVMPositionBuilderAtEnd(b, ctx->alloc_bb);
	LLVMValueRef ptr;
	if(count == NULL) ptr = LLVMBuildAlloca(b, type, name);
	else ptr = LLVMBuildArrayAlloca(b, type, count, name);
	LLVMDisposeBuilder(b);
	return ptr;
}


static LLVMValueRef build_utcb_get(struct llvm_ctx *ctx)
{
	LLVMTypeRef fntype = LLVMFunctionType(ctx->voidptrt, NULL, 0, 0);
	LLVMValueRef func = LLVMConstInlineAsm(fntype, "movl %gs:0,$0\n",
			"=r,~{dirflag},~{fpsr},~{flags}", 0, 0);
	LLVMValueRef call = LLVMBuildCall(ctx->builder, func, NULL, 0, "utcbget");
	LLVMSetTailCall(call, 1);
	return call;
}


/* return node is L4_Word_t mr0 (i.e. message tag). */
static LLVMValueRef build_l4_ipc_call(
	struct llvm_ctx *ctx,
	LLVMValueRef utcb_ptr,
	LLVMValueRef arg_to,
	LLVMValueRef arg_timeouts,
	LLVMValueRef arg_fromspec,
	LLVMValueRef arg_mr0,
	LLVMValueRef *from_p,
	LLVMValueRef *mr1_p,
	LLVMValueRef *mr2_p)
{
	/* FIXME: get word type from target somehow */
	LLVMTypeRef params[5], wordtype = LLVMInt32TypeInContext(ctx->ctx);
	for(int i=0; i<5; i++) params[i] = wordtype;
	LLVMTypeRef ipc_result_type = LLVMStructTypeInContext(ctx->ctx,
		params, 4, 0);
	LLVMTypeRef ipc_type = LLVMFunctionType(ipc_result_type,
		params, 5, 0);
	LLVMValueRef fn = LLVMConstInlineAsm(ipc_type,
		"call __L4_Ipc\n",
		"={ax},={si},={bx},={bp},{ax},{cx},{dx},{si},{di},~{dirflag},~{fpsr},~{flags}",
		1, 0);
	LLVMValueRef args[5] = {
		arg_to, arg_timeouts, arg_fromspec, arg_mr0, utcb_ptr
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


static LLVMValueRef build_utcb_address_ixval(
	struct llvm_ctx *ctx,
	LLVMValueRef offset)
{
	return LLVMBuildPointerCast(ctx->builder,
		LLVMBuildGEP(ctx->builder, ctx->utcb, &offset, 1, "utcb.addr"),
		LLVMPointerType(ctx->wordt, 0), "utcb.addr.wordptr");
}


static LLVMValueRef build_utcb_address(
	struct llvm_ctx *ctx,
	int offset)
{
	LLVMValueRef off = LLVMConstInt(ctx->wordt, abs(offset), 0);
	if(offset < 0) off = LLVMBuildNeg(ctx->builder, off, "utcboffset");
	return build_utcb_address_ixval(ctx, off);
}


static LLVMValueRef build_ipcfailed_cond(
	struct llvm_ctx *ctx,
	LLVMValueRef mr0)
{
	return LLVMBuildICmp(ctx->builder, LLVMIntNE,
		ctx->zero,
		LLVMBuildAnd(ctx->builder, mr0,
			LLVMConstInt(ctx->wordt, 1 << 15, 0), "mr0.e"),
		"ipcfailed.cond");
}


static LLVMValueRef build_label_from_tag(
	struct llvm_ctx *ctx,
	LLVMValueRef mr0)
{
	return LLVMBuildLShr(ctx->builder,
		LLVMBuildAnd(ctx->builder, mr0,
			LLVMConstInt(ctx->wordt, 0xffff0000, 0), "label.shifted"),
		LLVMConstInt(ctx->wordt, 16, 0),
		"label");
}


static LLVMValueRef build_u_from_tag(
	struct llvm_ctx *ctx,
	LLVMValueRef mr0)
{
	return LLVMBuildAnd(ctx->builder, mr0,
		LLVMConstInt(ctx->wordt, 0x3f, 0), "tag.u");
}


#if 0
static LLVMValueRef build_t_from_tag(
	LLVMBuilderRef builder,
	LLVMTypeRef wordtype,
	LLVMValueRef mr0)
{
	return LLVMBuildAnd(builder,
			LLVMConstInt(wordtype, 0x3f, 0),
			LLVMBuildLShr(builder, mr0, LLVMConstInt(wordtype, 6, 0),
				"tag.u.raw"),
			"tag.u");
}
#endif


/* returns a LLVM representation corresponding to the C translation of the
 * given IDL type.
 *
 * FIXME: move this into a llvmutil.c or some such once µIDL gets a LLVM stub
 * generator
 */
static LLVMTypeRef llvm_value_type(
	struct llvm_ctx *ctx,
	IDL_tree type)
{
	if(type == NULL) return LLVMVoidTypeInContext(ctx->ctx);
	switch(IDL_NODE_TYPE(type)) {
		case IDLN_TYPE_INTEGER: {
			static short bitlens[] = {
				[IDL_INTEGER_TYPE_SHORT] = 16,
				[IDL_INTEGER_TYPE_LONG] = 32,
				[IDL_INTEGER_TYPE_LONGLONG] = 64,
			};
			int t = IDL_TYPE_INTEGER(type).f_type;
			assert(t < G_N_ELEMENTS(bitlens));
			return LLVMIntTypeInContext(ctx->ctx, bitlens[t]);
		}

		case IDLN_NATIVE: {
			/* each of these is the size of a single word, which is all LLVM
			 * wants to know.
			 */
			if(IS_WORD_TYPE(type) || IS_FPAGE_TYPE(type)
				|| IS_TIME_TYPE(type))
			{
				return ctx->wordt;
			} else {
				fprintf(stderr, "%s: native type `%s' not supported\n",
					__FUNCTION__, NATIVE_NAME(type));
				exit(EXIT_FAILURE);
			}
			break;
		}

		case IDLN_TYPE_FLOAT:
			switch(IDL_TYPE_FLOAT(type).f_type) {
				case IDL_FLOAT_TYPE_FLOAT:
					return LLVMFloatTypeInContext(ctx->ctx);
				case IDL_FLOAT_TYPE_DOUBLE:
					return LLVMDoubleTypeInContext(ctx->ctx);
				case IDL_FLOAT_TYPE_LONGDOUBLE:
					return LLVMFP128TypeInContext(ctx->ctx);
			}
			g_assert_not_reached();

		case IDLN_TYPE_BOOLEAN:
		case IDLN_TYPE_OCTET:
		case IDLN_TYPE_CHAR:
			return LLVMInt8TypeInContext(ctx->ctx);
		case IDLN_TYPE_WIDE_CHAR:
			return ctx->i32t;

		case IDLN_TYPE_ENUM: return LLVMInt16TypeInContext(ctx->ctx);

		default:
			NOTDEFINED(type);
	}
}


static LLVMValueRef build_ipc_input_val(struct llvm_ctx *ctx, int mr)
{
	if(mr == 0) return ctx->tag;
	else if(mr == 1) return ctx->mr1;
	else if(mr == 2) return ctx->mr2;
	else {
		return LLVMBuildLoad(ctx->builder,
			build_utcb_address(ctx, mr * 4),
			tmp_f(ctx->pr, "mr%d", mr));
	}
}


/* ix is a valueref to an utcb index; not message register index. */
static LLVMValueRef build_utcb_load(
	struct llvm_ctx *ctx,
	LLVMValueRef ix,
	const char *name)
{
	return LLVMBuildLoad(ctx->builder,
		build_utcb_address_ixval(ctx, ix), name);
}


static LLVMValueRef build_mr_load(
	struct llvm_ctx *ctx,
	LLVMValueRef mrnum,
	const char *name)
{
	LLVMValueRef offset = LLVMBuildMul(ctx->builder, mrnum,
		LLVMConstInt(ctx->i32t, 4, 0), tmp_f(ctx->pr, "%s.offset", name));
	return build_utcb_load(ctx, offset, name);
}


/* when @ctyp is a value type, @dst[0] is assigned to the value.
 * when @ctyp is a rigid type, @dst[0] must be a pointer to the appropriate
 * type.
 * otherwise, @dst[0] must point to the first element of a buffer of sufficient
 * maximum size, and @dst[1] will be assigned to the length value (i32).
 *
 * this function should be called build_read_ipc_argument() instead; parameters
 * are always translated according to direction attribute anyhow.
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
		/* FIXME: stash this in the context */
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
		fprintf(stderr, "%s: not defined for long double (yet)\n",
			__func__);
		abort();
	} else if(is_value_type(ctyp)) {
		/* appropriate for all value types. */
		dst[0] = LLVMBuildTruncOrBitCast(ctx->builder,
			build_ipc_input_val(ctx, first_mr),
			llvm_value_type(ctx, ctyp), "shortparm");
	} else if(is_rigid_type(ctx->ns, ctyp)) {
		/* TODO */
		NOTDEFINED(ctyp);
	} else {
		/* genuinely not defined */
		NOTDEFINED(ctyp);
	}
}


/* see comment in sister function above */
static void build_read_ipc_parameter_ixval(
	struct llvm_ctx *ctx,
	LLVMValueRef *dst,
	IDL_tree ctyp,
	LLVMValueRef first_mr)
{
	if(IS_LONGLONG_TYPE(ctyp)) {
		/* unpack a two-word parameter. */
		LLVMValueRef low = build_mr_load(ctx, first_mr, "longparm.lo.mr"),
			high = build_mr_load(ctx,
				LLVMBuildAdd(ctx->builder, first_mr,
					LLVMConstInt(ctx->i32t, 1, 0), "longparm.hi.mr.ix"),
				"longparm.hi.mr");
		/* TODO: stash this in the context */
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
		fprintf(stderr, "%s: not defined for long double (yet)\n",
			__func__);
		abort();
	} else if(is_value_type(ctyp)) {
		/* appropriate for all value types. */
		dst[0] = LLVMBuildTruncOrBitCast(ctx->builder,
			build_mr_load(ctx, first_mr, "shortparm.mr"),
			llvm_value_type(ctx, ctyp), "shortparm");
	} else if(is_rigid_type(ctx->ns, ctyp)) {
		NOTDEFINED(ctyp);
	} else {
		/* genuinely not defined */
		NOTDEFINED(ctyp);
	}
}


/* FIXME: move to util.c or types.c or somewhere */
static bool is_integral_type(IDL_tree typ)
{
	switch(IDL_NODE_TYPE(typ)) {
		case IDLN_TYPE_INTEGER:
		case IDLN_TYPE_OCTET:
		case IDLN_TYPE_BOOLEAN:
			return true;

		default:
			return IS_WORD_TYPE(typ);
	}
}


static bool is_signed(IDL_tree typ)
{
	assert(is_integral_type(typ));
	return IDL_NODE_TYPE(typ) == IDLN_TYPE_CHAR
		|| IDL_NODE_TYPE(typ) == IDLN_TYPE_WIDE_CHAR
		|| (IDL_NODE_TYPE(typ) == IDLN_TYPE_INTEGER
			&& IDL_TYPE_INTEGER(typ).f_signed);
}


/* returns # of MRs used.
 *
 * when is_value_type(ctyp), @val[0] is a C representation of @ctyp.
 * when is_rigid_type(..., ctyp), @val[0] is a pointer to the same.
 * otherwise, @val[0] is a pointer to the first element, and @val[1] is the
 * number of elements as i32.
 */
static int build_write_ipc_parameter(
	struct llvm_ctx *ctx,
	const LLVMValueRef *val,
	IDL_tree ctyp,
	int first_mr)
{
	/* double-word types (TODO) */
	if(IS_LONGLONG_TYPE(ctyp)) {
		abort();
	} else if(IS_LONGDOUBLE_TYPE(ctyp)) {
		abort();
	}

	/* single-word types */
	LLVMValueRef reg;
	if(is_integral_type(ctyp)) {
		if(is_signed(ctyp)) {
			reg = LLVMBuildSExtOrBitCast(ctx->builder, val[0],
				ctx->wordt, "cast.word.s");
		} else {
			reg = LLVMBuildZExtOrBitCast(ctx->builder, val[0],
				ctx->wordt, "cast.word.z");
		}
	} else {
		/* FIXME! */
		NOTDEFINED(ctyp);
	}
	LLVMBuildStore(ctx->builder, reg,
		build_utcb_address(ctx, first_mr * 4));
	return 1;
}


/* dst should have two LLVMTypeRefs' worth of space to allow for sequences
 * (pointer + length).
 */
static void vtable_in_param_type(
	struct llvm_ctx *ctx,
	LLVMTypeRef *dst,
	int *pos_p,
	IDL_tree type)
{
	/* FIXME: handle long long, long double types */
	if(is_value_type(type)) {
		dst[(*pos_p)++] = llvm_value_type(ctx, type);
	} else if(is_rigid_type(ctx->ns, type)) {
		/* FIXME: handle structs, arrays, unions */
		NOTDEFINED(type);
	} else if(IDL_NODE_TYPE(type) == IDLN_TYPE_SEQUENCE) {
		IDL_tree seqtype = get_type_spec(
			IDL_TYPE_SEQUENCE(type).simple_type_spec);
		dst[(*pos_p)++] = LLVMPointerType(llvm_value_type(ctx, seqtype), 0);
		dst[(*pos_p)++] = ctx->i32t;
	} else {
		/* FIXME: handle strings and wide strings */
		NOTDEFINED(type);
	}
}


static void vtable_out_param_type(
	struct llvm_ctx *ctx,
	LLVMTypeRef *dst,
	int *pos_p,
	IDL_tree type)
{
	if(is_value_type(type)) {
		dst[(*pos_p)++] = LLVMPointerType(llvm_value_type(ctx, type), 0);
	} else if(IDL_NODE_TYPE(type) == IDLN_TYPE_SEQUENCE) {
		IDL_tree seqtype = get_type_spec(
			IDL_TYPE_SEQUENCE(type).simple_type_spec);
		dst[(*pos_p)++] = LLVMPointerType(llvm_value_type(ctx, seqtype), 0);
		dst[(*pos_p)++] = LLVMPointerType(ctx->i32t, 0);
	} else {
		printf("warning: not emitting llvm out-parameter for <%s>\n",
			IDL_NODE_TYPE_NAME(type));
	}
}


static LLVMTypeRef vtable_return_type(
	struct llvm_ctx *ctx,
	IDL_tree op,
	bool *actual_p)
{
	char *ctyp = return_type(ctx->ns, op, actual_p, true);
	LLVMTypeRef t;
	if(strcmp(ctyp, "int") == 0) t = ctx->i32t;
	else if(strcmp(ctyp, "void") == 0) t = LLVMVoidTypeInContext(ctx->ctx);
	else {
		t = llvm_value_type(ctx, get_type_spec(IDL_OP_DCL(op).op_type_spec));
	}
	g_free(ctyp);
	return t;
}


/* TODO: wrap this to cache things in @ctx, and move it into a llvmutil.c or
 * some such
 */
static LLVMTypeRef get_vtable_type(struct llvm_ctx *ctx, IDL_tree iface)
{
	GList *methods = all_methods_of_iface(ctx->ns, iface);
	int num_fields = g_list_length(methods), f_offs = 0;
	LLVMTypeRef field_types[num_fields];
	GList *cur = g_list_first(methods);
	while(f_offs < num_fields) {
		assert(cur != NULL);
		IDL_tree op = cur->data,
			idl_rettyp = get_type_spec(IDL_OP_DCL(op).op_type_spec),
			param_list = IDL_OP_DCL(op).parameter_dcls;
		/* each parameter can be a sequence */
		const int n_args_max = IDL_list_length(param_list) * 2
			+ (idl_rettyp != NULL ? 1 : 0);
		LLVMTypeRef arg_types[n_args_max];
		int arg_pos = 0;
		bool ret_actual = false;
		LLVMTypeRef rettyp = vtable_return_type(ctx, op, &ret_actual);
		if(!ret_actual && idl_rettyp != NULL) {
			vtable_out_param_type(ctx, arg_types, &arg_pos, idl_rettyp);
		}
		for(IDL_tree p_cur = param_list;
			p_cur != NULL;
			p_cur = IDL_LIST(p_cur).next)
		{
			assert(arg_pos < n_args_max);
			IDL_tree pdecl = IDL_LIST(p_cur).data,
				ptype = get_type_spec(IDL_PARAM_DCL(pdecl).param_type_spec);
			switch(IDL_PARAM_DCL(pdecl).attr) {
				case IDL_PARAM_IN:
					vtable_in_param_type(ctx, arg_types, &arg_pos, ptype);
					break;

				/* inout parameters are passed exactly like out-parameters, but
				 * with a value already present.
				 */
				case IDL_PARAM_OUT:
				case IDL_PARAM_INOUT:
					vtable_out_param_type(ctx, arg_types, &arg_pos, ptype);
					break;
			}
		}

		cur = cur->next;
		field_types[f_offs++] = LLVMPointerType(
			LLVMFunctionType(rettyp, arg_types, arg_pos, 0), 0);
	}
	assert(cur == NULL);
	g_list_free(methods);

	return LLVMStructTypeInContext(ctx->ctx, field_types, num_fields, 0);
}


static struct untyped_param *find_untyped(
	const struct message_info *msg,
	IDL_tree node)
{
	for(int i=0; i<msg->num_untyped; i++) {
		if(msg->untyped[i]->param_dcl == node) {
			return msg->untyped[i];
		}
	}
	return NULL;
}


static void emit_in_param(
	struct llvm_ctx *ctx,
	LLVMValueRef *args,
	int *arg_pos_p,
	const struct method_info *inf,
	IDL_tree pdecl)
{
	const struct message_info *req = inf->request;

	/* FIXME: repair the untyped/seq/long param thing; they should not
	 * be flattened out by kind like that.
	 */
	struct untyped_param *u = find_untyped(req, pdecl);
	if(u != NULL) {
		build_read_ipc_parameter(ctx, &args[(*arg_pos_p)++],
			u->type, u->first_reg);
		return;
	}

	/* inline sequence? */
	const struct seq_param *seq = NULL;
	for(int i=0; i < req->num_inline_seq; i++) {
		if(req->seq[i]->param_dcl == pdecl) {
			seq = req->seq[i];
			break;
		}
	}
	if(seq != NULL) {
		/* this only works if inline sequences appear as parameters to this
		 * function in the same order as they were in IDL. if not,
		 * inline_seq_pos will be wrong.
		 */
		LLVMValueRef seq_words, seq_end_elem;
		if(seq != req->seq[req->num_inline_seq - 1]
			|| seq->bits_per_elem < BITS_PER_WORD)
		{
			/* not subject to trickery; take a length word. */
			seq_end_elem = build_mr_load(ctx, ctx->inline_seq_pos,
				"inlseq.len.mr");
			seq_end_elem = LLVMBuildTruncOrBitCast(ctx->builder,
				seq_end_elem, ctx->i32t, "inlseq.len.explicit");
			if(seq->elems_per_word == 1) seq_words = seq_end_elem;
			else {
				seq_words = LLVMBuildUDiv(ctx->builder, seq_end_elem,
					LLVMConstInt(ctx->i32t, seq->elems_per_word, 0),
					"inlseq.len.int");
			}
			ctx->inline_seq_pos = LLVMBuildAdd(ctx->builder,
				ctx->inline_seq_pos, LLVMConstInt(ctx->i32t, 1, 0),
				"inlseq.pos.bump");
		} else {
			/* last sequence of word-length items; compute length from "u". */
			LLVMValueRef u = build_u_from_tag(ctx, ctx->tag);
			seq_words = LLVMBuildSub(ctx->builder, u,
				ctx->inline_seq_pos, "inlseq.len.implicit");
			seq_words = LLVMBuildTruncOrBitCast(ctx->builder, seq_words,
				ctx->i32t, "inlseq.len.int");
			seq_end_elem = seq_words;
		}

		LLVMBasicBlockRef before_bb = LLVMGetInsertBlock(ctx->builder);
		LLVMValueRef fn = LLVMGetBasicBlockParent(before_bb);
		LLVMBasicBlockRef loop_bb, after_loop_bb;
		loop_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "inlseq.loop");
		after_loop_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn,
			"inlseq.loop.after");

		/* guard against maximum size violations */
		/* FIXME: get EINVAL from µiX headers */
		LLVMValueRef einval = LLVMConstInt(ctx->i32t, -EINVAL, 1);
		LLVMAddIncoming(ctx->fncall_phi, &einval, &before_bb, 1);
		LLVMValueRef einval_cond = LLVMBuildICmp(ctx->builder,
			LLVMIntULT, seq_end_elem,
			LLVMConstInt(ctx->i32t, seq->max_elems, 0),
			"inlseq.len.cond");
		LLVMBuildCondBr(ctx->builder, einval_cond, loop_bb,
			ctx->msgerr_bb);

		/* the copy loop. */
		LLVMPositionBuilderAtEnd(ctx->builder, loop_bb);
		LLVMTypeRef seq_type = llvm_value_type(ctx, seq->elem_type);
		LLVMValueRef seq_mem = build_local_storage(ctx, seq_type,
			LLVMConstInt(ctx->i32t, seq->max_elems, 0), "inlseq.mem");
		LLVMValueRef seq_pos, counter;
		seq_pos = LLVMBuildPhi(ctx->builder, ctx->i32t, "loop.seqpos");
		counter = LLVMBuildPhi(ctx->builder, ctx->i32t, "loop.ctr");
		LLVMAddIncoming(seq_pos, &ctx->inline_seq_pos, &before_bb, 1);
		LLVMAddIncoming(counter, &ctx->zero, &before_bb, 1);

		if(seq->bits_per_elem == BITS_PER_WORD) {
			/* full word case */
			LLVMValueRef item;
			build_read_ipc_parameter_ixval(ctx, &item, seq->elem_type,
				seq_pos);
			LLVMBuildStore(ctx->builder, item,
				LLVMBuildGEP(ctx->builder, seq_mem, &counter, 1,
					"seq.mem.at"));
		} else {
			/* many-per-word case */
			LLVMValueRef word = build_mr_load(ctx, seq_pos, "seq.limb");
			for(int i=0; i<seq->elems_per_word; i++) {
				LLVMValueRef item = LLVMBuildTrunc(ctx->builder,
					i == 0 ? word : LLVMBuildLShr(ctx->builder, word,
						LLVMConstInt(ctx->i32t, seq->bits_per_elem * i, 0),
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
		LLVMValueRef exit_cond = LLVMBuildICmp(ctx->builder,
			LLVMIntULT, next_counter, seq_end_elem, "loop.nextp");
		LLVMBuildCondBr(ctx->builder, exit_cond, loop_bb, after_loop_bb);
		LLVMPositionBuilderAtEnd(ctx->builder, after_loop_bb);

		args[(*arg_pos_p)++] = seq_mem;
		args[(*arg_pos_p)++] = seq_end_elem;
		ctx->inline_seq_pos = next_sp;

		return;
	}

	fprintf(stderr, "can't hax this in-parameter\n");
	abort();
}


static void emit_out_param(
	struct llvm_ctx *ctx,
	LLVMValueRef *args,
	int *arg_pos_p,
	IDL_tree ptyp)
{
	if(is_value_type(ptyp)) {
		args[(*arg_pos_p)++] = build_local_storage(ctx,
			llvm_value_type(ctx, ptyp), NULL, "outparam.mem");
	} else if(IDL_NODE_TYPE(ptyp) == IDLN_TYPE_SEQUENCE) {
		IDL_tree seqtype = get_type_spec(
			IDL_TYPE_SEQUENCE(ptyp).simple_type_spec);
		int max_size = IDL_INTEGER(
			IDL_TYPE_SEQUENCE(ptyp).positive_int_const).value;
		args[(*arg_pos_p)++] = build_local_storage(ctx,
			llvm_value_type(ctx, seqtype),
			LLVMConstInt(ctx->i32t, max_size, 0),
			"outparam.seq.mem");
		args[(*arg_pos_p)++] = build_local_storage(ctx,
			ctx->i32t, NULL, "outparam.seq.len.mem");
	} else {
		printf("can't hack seq/long out-parameter\n");
		abort();
	}
}


static int first_arg_index(IDL_tree op, IDL_tree pdecl)
{
	int ix = 0;
	for(IDL_tree cur = IDL_OP_DCL(op).parameter_dcls;
		cur != NULL;
		cur = IDL_LIST(cur).next)
	{
		IDL_tree p = IDL_LIST(cur).data;
		if(p == pdecl) return ix;
		IDL_tree type = get_type_spec(IDL_PARAM_DCL(p).param_type_spec);
		/* sequences are 2 args. everything else is just one. */
		ix += IDL_NODE_TYPE(type) == IDLN_TYPE_SEQUENCE ? 2 : 1;
	}
	return -1;
}


static LLVMBasicBlockRef build_op_decode(
	struct llvm_ctx *ctx,
	LLVMValueRef function,
	const struct method_info *inf)
{
	struct print_ctx *pr = ctx->pr;
	char *name = !IS_EXN_MSG(inf)
		? decapsify(IDL_IDENT(IDL_OP_DCL(inf->node).ident).str)
		: decapsify(IDL_IDENT(IDL_EXCEPT_DCL(inf->node).ident).str);
	char *opname = tmp_f(pr, "decode.%s", name);
	LLVMBasicBlockRef bb = LLVMAppendBasicBlockInContext(ctx->ctx,
		function, opname);
	g_free(name);
	opname = &opname[7];	/* skip "decode." for value names */

	/* (TODO: we're allowed to do this, right?) */
	LLVMPositionBuilderAtEnd(ctx->builder, bb);

	/* collection of arguments according to op decl. */
	const struct message_info *req = inf->request;
	bool rv_actual = false;
	LLVMTypeRef rv_type = vtable_return_type(ctx, inf->node, &rv_actual);

	ctx->inline_seq_pos = LLVMConstInt(ctx->i32t,
		inf->request->untyped_words + 1, 0);
	const int num_args_max = 1 + req->num_untyped
		+ req->num_inline_seq * 2 + req->num_long * 2;
	LLVMValueRef args[num_args_max], retvalptr = NULL;
	int arg_pos = 0;
	if(inf->return_type != NULL && !rv_actual) {
		/* a parameter for the return value. */
		emit_out_param(ctx, args, &arg_pos, inf->return_type);
		if(arg_pos > 0) retvalptr = args[0];
	}
	for(IDL_tree cur = IDL_OP_DCL(inf->node).parameter_dcls;
		cur != NULL;
		cur = IDL_LIST(cur).next)
	{
		IDL_tree p = IDL_LIST(cur).data;
		enum IDL_param_attr attr = IDL_PARAM_DCL(p).attr;
		if(attr == IDL_PARAM_IN) {
			emit_in_param(ctx, args, &arg_pos, inf, p);
		} else if(attr == IDL_PARAM_OUT) {
			emit_out_param(ctx, args, &arg_pos,
				get_type_spec(IDL_PARAM_DCL(p).param_type_spec));
		} else /* inout */ {
			/* hax! */
			int start = arg_pos;
			emit_out_param(ctx, args, &arg_pos,
				get_type_spec(IDL_PARAM_DCL(p).param_type_spec));
			LLVMValueRef in_args[2];
			int in_arg_pos = 0;
			emit_in_param(ctx, in_args, &in_arg_pos, inf, p);
			assert(in_arg_pos == arg_pos - start);
			/* insert tab A in slot B */
			for(int i=0; i<in_arg_pos; i++) {
				LLVMBuildStore(ctx->builder, in_args[i], args[start + i]);
			}
		}
	}
	assert(arg_pos >= IDL_list_length(IDL_OP_DCL(inf->node).parameter_dcls));

	/* the function call. */
	LLVMValueRef fnptr = LLVMBuildLoad(ctx->builder,
		LLVMBuildStructGEP(ctx->builder, ctx->vtab_arg, inf->vtab_offset,
				tmp_f(pr, "%s.offs", opname)),
			tmp_f(pr, "%s.fnptr", opname));
	LLVMValueRef fncall = LLVMBuildCall(ctx->builder, fnptr, args, arg_pos,
		IS_VOID_TYPEREF(rv_type) ? "" : tmp_f(pr, "%s.call", opname));

	if(IDL_OP_DCL(inf->node).f_oneway) {
		LLVMBuildBr(ctx->builder, ctx->wait_bb);
		return bb;
	}

	LLVMBasicBlockRef pr_bb = LLVMAppendBasicBlockInContext(ctx->ctx,
			function, tmp_f(pr, "%s.pack_reply", opname)),
		ex_chain_bb = pr_bb;	/* exception chain, or result packer */

	/* TODO: exceptions! */

	IDL_tree n_ex = find_neg_exn(inf->node);
	if(n_ex == NULL) LLVMBuildBr(ctx->builder, ex_chain_bb);
	else {
		/* examine NegativeReturn exception trigger */
		LLVMValueRef ok_cond = LLVMBuildICmp(ctx->builder, LLVMIntSGE, fncall,
			LLVMConstInt(ctx->i32t, 0, 1), "rcneg.cond");
		LLVMAddIncoming(ctx->fncall_phi, &fncall, &bb, 1);
		LLVMBuildCondBr(ctx->builder, ok_cond, ex_chain_bb,
			ctx->msgerr_bb);
	}

	/* pack results from the non-exceptional return. */
	LLVMPositionBuilderAtEnd(ctx->builder, pr_bb);
	assert(inf->num_reply_msgs > 0);
	assert(!IS_EXN_MSG(inf->replies[0]));
	const struct message_info *reply = inf->replies[0];
	int mr_pos = 1;
	/* return value */
	if(inf->return_type != NULL) {
		LLVMValueRef val;
		char *rv_name = tmp_f(pr, "%s.retval", opname);
		if(rv_actual) {
			val = LLVMBuildTruncOrBitCast(ctx->builder, fncall,
				llvm_value_type(ctx, inf->return_type), rv_name);
		} else {
			assert(retvalptr != NULL);
			val = LLVMBuildLoad(ctx->builder, retvalptr, rv_name);
		}
		mr_pos += build_write_ipc_parameter(ctx, &val,
			inf->return_type, mr_pos);
	}
	/* those out-parameters and out-halves of inout parameters which are
	 * either value or rigid types, i.e. a fixed number of words each.
	 */
	int arg_ix = 0;
	for(IDL_tree cur = IDL_OP_DCL(reply->node).parameter_dcls;
		cur != NULL;
		cur = IDL_LIST(cur).next, arg_ix++)
	{
		IDL_tree p = IDL_LIST(cur).data;
		if(IDL_PARAM_DCL(p).attr == IDL_PARAM_IN) continue;
		IDL_tree typ = get_type_spec(IDL_PARAM_DCL(p).param_type_spec);
		/* TODO: mapgrantitems are currently always typed (so we skip them
		 * here), but this should really depend on whether there's a [map]
		 * attribute in the parameter declaration.
		 */
		if(IS_MAPGRANT_TYPE(typ)) continue;
		if(is_value_type(typ)) {
			LLVMValueRef rval = LLVMBuildLoad(ctx->builder,
				args[arg_ix], tmp_f(ctx->pr, "arg%d.raw", arg_ix));
			mr_pos += build_write_ipc_parameter(ctx, &rval, typ, mr_pos);
		} else if(is_rigid_type(ctx->ns, typ)) {
			/* TODO: distinguish between inline rigid types and those passed as
			 * string items due to size or content or something
			 */
			mr_pos += build_write_ipc_parameter(ctx, &args[arg_ix], typ,
				mr_pos);
		}
	}

	/* inline sequences */
	/* FIXME: make sure reply->untyped_words + 1 == mr_pos; otherwise
	 * cruel overwriting will occur
	 */
	ctx->inline_seq_pos = LLVMConstInt(ctx->i32t,
		reply->untyped_words + 1, 0);
	for(int seq_i=0; seq_i < reply->num_inline_seq; seq_i++) {
		const struct seq_param *seq = reply->seq[seq_i];
		int first_arg = first_arg_index(reply->node, seq->param_dcl);
		assert(first_arg >= 0);
		LLVMValueRef mem = args[first_arg], lenptr = args[first_arg + 1];
		LLVMValueRef num_items = LLVMBuildLoad(ctx->builder,
			lenptr, "inlseq.len");
		if(seq->elems_per_word > 1 || seq_i + 1 < reply->num_inline_seq) {
			/* not simple && last; emit an item-count word. */
			LLVMBuildStore(ctx->builder,
				LLVMBuildZExtOrBitCast(ctx->builder, num_items,
					ctx->wordt, "inlseq.len.word"),
				build_utcb_address_ixval(ctx,
					LLVMBuildMul(ctx->builder, ctx->inline_seq_pos,
						LLVMConstInt(ctx->i32t, 4, 0), "inlseq.len.word.offs")));
			ctx->inline_seq_pos = LLVMBuildAdd(ctx->builder,
				ctx->inline_seq_pos, LLVMConstInt(ctx->i32t, 1, 0),
				"inlseq.pos.bump");
		}

		/* loop per word. */
		LLVMBasicBlockRef before_bb = LLVMGetInsertBlock(ctx->builder);
		LLVMValueRef fn = LLVMGetBasicBlockParent(before_bb);
		LLVMBasicBlockRef loop_bb = LLVMAppendBasicBlockInContext(
				ctx->ctx, fn, "inlseq.out.loop"),
			loop_after_bb = LLVMAppendBasicBlockInContext(
				ctx->ctx, fn, "inlseq.out.loop.after");
		LLVMBuildBr(ctx->builder, loop_bb);

		LLVMPositionBuilderAtEnd(ctx->builder, loop_bb);
		LLVMValueRef pos = LLVMBuildPhi(ctx->builder, ctx->i32t,
				"inlseq.out.pos"),
			word_ix = LLVMBuildPhi(ctx->builder, ctx->i32t,
				"inlseq.out.word_ix");
		LLVMAddIncoming(pos, &ctx->zero, &before_bb, 1);
		LLVMAddIncoming(word_ix, &ctx->inline_seq_pos, &before_bb, 1);
		LLVMValueRef wordval = LLVMBuildLoad(ctx->builder,
			LLVMBuildGEP(ctx->builder, mem, &pos, 1, "item.ptr"),
			"item.wordval");
		if(seq->elems_per_word == 1) {
			LLVMBuildStore(ctx->builder, wordval,
				build_utcb_address_ixval(ctx,
					LLVMBuildMul(ctx->builder, word_ix,
						LLVMConstInt(ctx->i32t, 4, 0), "word_ix.offs")));
		} else {
			/* FIXME */
			fprintf(stderr, "fasdkjfhaskjdhfakjshfjsa\n");
		}
		LLVMValueRef next_pos = LLVMBuildAdd(ctx->builder, pos,
				LLVMConstInt(ctx->i32t, seq->elems_per_word, 0),
				"inlseq.out.pos.next"),
			next_word = LLVMBuildAdd(ctx->builder, word_ix,
				LLVMConstInt(ctx->i32t, 1, 0), "inlseq.out.word_ix.next");
		LLVMBasicBlockRef current = LLVMGetInsertBlock(ctx->builder);
		LLVMAddIncoming(pos, &next_pos, &current, 1);
		LLVMAddIncoming(word_ix, &next_word, &current, 1);
		LLVMValueRef exit_cond = LLVMBuildICmp(ctx->builder,
			LLVMIntULT, next_pos, num_items, "exit.cond");
		LLVMBuildCondBr(ctx->builder, exit_cond, loop_bb, loop_after_bb);

		LLVMPositionBuilderAtEnd(ctx->builder, loop_after_bb);
		if(seq->elems_per_word > 1) {
			/* FIXME: make loop for trailing elements */
			fprintf(stderr, "fasdjfashdfkjahlskjhlkjhkjhlkjhlkjhlkjh!!!!\n");
		}

		ctx->inline_seq_pos = next_word;
	}

	/* TODO: encode typed words (long & complex sequences, strings, wide
	 * strings)
	 */
	/* label 0, t 0, u = mr_pos - 1 */
	LLVMBasicBlockRef current = LLVMGetInsertBlock(ctx->builder);
	LLVMValueRef tag = LLVMConstInt(ctx->wordt, mr_pos - 1, 0);
	LLVMAddIncoming(ctx->reply_tag, &tag, &current, 1);
	LLVMBuildBr(ctx->builder, ctx->reply_bb);

	return bb;
}


LLVMValueRef build_dispatcher_function(struct llvm_ctx *ctx, IDL_tree iface)
{
	GList *tagmask_list = NULL,
		*methods = analyse_methods_of_iface(ctx->pr, &tagmask_list, iface);

	LLVMTypeRef param = LLVMPointerType(get_vtable_type(ctx, iface), 0),
		fn_type = LLVMFunctionType(ctx->wordt, &param, 1, 0);

	char *dispname = dispatcher_name(ctx->ns, iface, NULL);
	LLVMValueRef fn = LLVMAddFunction(ctx->module, dispname, fn_type);
	ctx->vtab_arg = LLVMGetFirstParam(fn);
	g_free(dispname);

	ctx->builder = LLVMCreateBuilderInContext(ctx->ctx);
	LLVMBasicBlockRef bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "EntryBlock"),
		loop_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "loop"),
		exit_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "exit"),
		ret_ec_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "ret_errcode"),
		dispatch_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "dispatch");
	ctx->wait_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "wait");
	ctx->reply_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "reply");
	ctx->msgerr_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn, "msgerr");

	LLVMPositionBuilderAtEnd(ctx->builder, bb);
	ctx->utcb = build_utcb_get(ctx);
	LLVMValueRef xfer_timeouts_addr = build_utcb_address(ctx, -32),
		stored_timeouts = LLVMBuildLoad(ctx->builder,
			xfer_timeouts_addr, "stored_timeouts"),
		acceptor = LLVMConstInt(ctx->i32t, 0, 0);
	LLVMBuildStore(ctx->builder, acceptor, build_utcb_address(ctx, -64));
	LLVMBuildStore(ctx->builder, stored_timeouts, xfer_timeouts_addr);
	ctx->alloc_bb = bb;

	LLVMPositionBuilderAtEnd(ctx->builder, ctx->wait_bb);
	LLVMValueRef ipc_from, ipc_mr1, ipc_mr2,
		ipc_tag = build_l4_ipc_call(ctx, ctx->utcb,
			ctx->zero, LLVMConstNot(ctx->zero), LLVMConstNot(ctx->zero), ctx->zero,
			&ipc_from, &ipc_mr1, &ipc_mr2);
	LLVMBuildBr(ctx->builder, loop_bb);

	/* the main dispatch-replywait loop. */
	LLVMPositionBuilderAtEnd(ctx->builder, loop_bb);
	ctx->from = LLVMBuildPhi(ctx->builder, ctx->wordt, "from.phi");
	ctx->mr1 = LLVMBuildPhi(ctx->builder, ctx->wordt, "mr1.phi");
	ctx->mr2 = LLVMBuildPhi(ctx->builder, ctx->wordt, "mr2.phi");
	ctx->tag = LLVMBuildPhi(ctx->builder, ctx->wordt, "tag.phi");
	LLVMAddIncoming(ctx->from, &ipc_from, &ctx->wait_bb, 1);
	LLVMAddIncoming(ctx->mr1, &ipc_mr1, &ctx->wait_bb, 1);
	LLVMAddIncoming(ctx->mr2, &ipc_mr2, &ctx->wait_bb, 1);
	LLVMAddIncoming(ctx->tag, &ipc_tag, &ctx->wait_bb, 1);
	LLVMBuildCondBr(ctx->builder, build_ipcfailed_cond(ctx, ctx->tag),
		ret_ec_bb, dispatch_bb);

	/* send reply, receive next message. */
	/* message registers were already loaded, since ia32 only requires the tag
	 * in a cpu register.
	 */
	LLVMPositionBuilderAtEnd(ctx->builder, ctx->reply_bb);
	ctx->reply_tag = LLVMBuildPhi(ctx->builder, ctx->wordt, "replytag.phi");
	ipc_tag = build_l4_ipc_call(ctx, ctx->utcb,
		ctx->from, LLVMConstNot(ctx->zero), LLVMConstNot(ctx->zero),
		ctx->reply_tag, &ipc_from, &ipc_mr1, &ipc_mr2);
	LLVMAddIncoming(ctx->from, &ipc_from, &ctx->reply_bb, 1);
	LLVMAddIncoming(ctx->mr1, &ipc_mr1, &ctx->reply_bb, 1);
	LLVMAddIncoming(ctx->mr2, &ipc_mr2, &ctx->reply_bb, 1);
	LLVMAddIncoming(ctx->tag, &ipc_tag, &ctx->reply_bb, 1);
	LLVMBuildBr(ctx->builder, loop_bb);

	/* send a MSG_ERROR. */
	LLVMPositionBuilderAtEnd(ctx->builder, ctx->msgerr_bb);
	ctx->fncall_phi = LLVMBuildPhi(ctx->builder, ctx->i32t, "fncall.phi");
	LLVMValueRef msgerr_tag = LLVMBuildOr(ctx->builder,
		LLVMConstInt(ctx->wordt, 1, 0), LLVMConstInt(ctx->wordt, 1 << 16, 0),
		"msgerr.tag");
	LLVMBuildStore(ctx->builder,
		LLVMBuildNeg(ctx->builder, ctx->fncall_phi, "rcneg.val"),
		build_utcb_address(ctx, 4));	/* mr1 on ia32 */
	LLVMAddIncoming(ctx->reply_tag, &msgerr_tag, &ctx->msgerr_bb, 1);
	LLVMBuildBr(ctx->builder, ctx->reply_bb);

	/* exit */
	LLVMPositionBuilderAtEnd(ctx->builder, exit_bb);
	LLVMValueRef retval = LLVMBuildPhi(ctx->builder, ctx->wordt, "retval");
	LLVMBuildRet(ctx->builder, retval);

	/* return L4_ErrorCode(); */
	LLVMPositionBuilderAtEnd(ctx->builder, ret_ec_bb);
	LLVMValueRef errorcode = LLVMBuildLoad(ctx->builder,
		build_utcb_address(ctx, -36), "errcode");
	LLVMAddIncoming(retval, &errorcode, &ret_ec_bb, 1);
	LLVMBuildBr(ctx->builder, exit_bb);

	/* dispatch according to ctx->tag. */
	LLVMPositionBuilderAtEnd(ctx->builder, dispatch_bb);
	/* FIXME: get the correct value */
	LLVMValueRef labelswitch = LLVMBuildSwitch(ctx->builder,
		build_label_from_tag(ctx, ctx->tag), exit_bb, 2);
	LLVMValueRef unknownlabel = LLVMConstInt(ctx->wordt, 42666, 0);
	LLVMAddIncoming(retval, &unknownlabel, &dispatch_bb, 1);

	/* FIXME: support for tag-mask labelled operations, such as for the L4.X2
	 * pager protocol
	 */
	for(GList *cur = g_list_first(methods);
		cur != NULL;
		cur = g_list_next(cur))
	{
		struct method_info *inf = cur->data;
		/* FIXME: handle sublabels! */
		LLVMBasicBlockRef decode_bb = build_op_decode(ctx, fn, inf);
		LLVMAddCase(labelswitch,
			LLVMConstInt(ctx->wordt, inf->request->label, 0),
			decode_bb);
	}

	/* close off the alloc (entry) block. */
	LLVMPositionBuilderAtEnd(ctx->builder, ctx->alloc_bb);
	LLVMBuildBr(ctx->builder, ctx->wait_bb);

	LLVMDisposeBuilder(ctx->builder);
	ctx->builder = NULL;
	g_list_foreach(methods, (GFunc)free_method_info, NULL);
	g_list_free(methods);
	g_list_free(tagmask_list);
	return fn;
}
