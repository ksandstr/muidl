/*
 * types.c -- type handling functions
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
#include <libIDL/IDL.h>

#include "muidl.h"
#include "llvmutil.h"


/* returns a LLVM representation corresponding to the C translation of the
 * given IDL type.
 */
LLVMTypeRef llvm_value_type(struct llvm_ctx *ctx, IDL_tree type)
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


/* TODO: cache outputs by the struct type's repo id, like in
 * packed_format_of()
 *
 */
LLVMTypeRef llvm_struct_type(
	struct llvm_ctx *ctx,
	char ***names_p,
	IDL_tree type)
{
	assert(IDL_NODE_TYPE(type) == IDLN_TYPE_STRUCT);
	GArray *types = g_array_new(FALSE, FALSE, sizeof(T));
	GPtrArray *names = names_p != NULL ? g_ptr_array_new() : NULL;
	for(IDL_tree cur = IDL_TYPE_STRUCT(type).member_list;
		cur != NULL;
		cur = IDL_LIST(cur).next)
	{
		IDL_tree member = IDL_LIST(cur).data,
			mtype = get_type_spec(IDL_MEMBER(member).type_spec);
		T mt = llvm_rigid_type(ctx, mtype);
		for(IDL_tree dcl_cur = IDL_MEMBER(member).dcls;
			dcl_cur != NULL;
			dcl_cur = IDL_LIST(dcl_cur).next)
		{
			IDL_tree dcl = IDL_LIST(dcl_cur).data;
			if(IDL_NODE_TYPE(dcl) == IDLN_IDENT) {
				g_array_append_val(types, mt);
				if(names_p != NULL) {
					g_ptr_array_add(names, g_strdup(IDL_IDENT(dcl).str));
				}
			} else if(IDL_NODE_TYPE(dcl) == IDLN_TYPE_ARRAY) {
				T ary = LLVMArrayType(mt, ARRAY_TYPE_LENGTH(dcl));
				g_array_append_val(types, ary);
				if(names_p != NULL) {
					g_ptr_array_add(names, g_strdup(IDL_IDENT(
						IDL_TYPE_ARRAY(dcl).ident).str));
				}
			} else {
				NOTDEFINED(member);
			}
		}
	}

	if(names_p != NULL) {
		char **namev = g_new(char *, names->len + 1);
		for(int i=0; i<names->len; i++) namev[i] = names->pdata[i];
		namev[names->len] = NULL;
		g_ptr_array_free(names, TRUE);
		(*names_p) = namev;
	}

	/* TODO: examine a "packed" attribute */
	T ret = LLVMStructTypeInContext(ctx->ctx, &g_array_index(types, T, 0),
		types->len, 0);
	g_array_free(types, TRUE);
	return ret;
}


LLVMTypeRef llvm_rigid_type(struct llvm_ctx *ctx, IDL_tree type)
{
	if(is_value_type(type)) return llvm_value_type(ctx, type);
	switch(IDL_NODE_TYPE(type)) {
		case IDLN_TYPE_STRUCT:
			return llvm_struct_type(ctx, NULL, type);

		case IDLN_TYPE_ARRAY: {
			IDL_tree mt = get_array_type(type);
			return LLVMArrayType(llvm_rigid_type(ctx, mt),
				ARRAY_TYPE_LENGTH(type));
		}

		case IDLN_TYPE_UNION:
			/* TODO */

		default:
			NOTDEFINED(type);
	}
}


/* FIXME: this is not actually a typing-related function. move it elsewhere,
 * say, message.c once that one is merged in.
 */
void build_read_ipc_parameter_ixval(
	struct llvm_ctx *ctx,
	LLVMValueRef *dst,
	IDL_tree ctyp,
	LLVMValueRef first_mr)
{
	if(IS_LONGLONG_TYPE(ctyp)) {
		/* unpack a two-word parameter. */
		LLVMValueRef low = build_utcb_load(ctx, first_mr, "longparm.lo.mr"),
			high = build_utcb_load(ctx,
				LLVMBuildAdd(ctx->builder, first_mr, CONST_INT(1),
					"longparm.hi.mr.ix"),
				"longparm.hi.mr");
		/* TODO: stash this in the context */
		LLVMTypeRef i64t = LLVMInt64TypeInContext(ctx->ctx);
		low = LLVMBuildBitCast(ctx->builder, low, i64t,
			"longparm.lo.cast");
		high = LLVMBuildBitCast(ctx->builder, high, i64t,
			"longparm.hi.cast");
		dst[0] = LLVMBuildOr(ctx->builder, low,
			LLVMBuildShl(ctx->builder, high, CONST_INT(32),
				"longparm.hi.shift"),
			"longparm.value");
	} else if(IS_LONGDOUBLE_TYPE(ctyp)) {
		fprintf(stderr, "%s: not defined for long double (yet)\n",
			__func__);
		abort();
	} else if(is_value_type(ctyp)) {
		/* appropriate for all value types. */
		dst[0] = LLVMBuildTruncOrBitCast(ctx->builder,
			build_utcb_load(ctx, first_mr, "shortparm.mr"),
			llvm_value_type(ctx, ctyp), "shortparm");
	} else if(IDL_NODE_TYPE(ctyp) == IDLN_TYPE_STRUCT) {
		const char *s_name = IDL_IDENT(IDL_TYPE_STRUCT(ctyp).ident).str;
		const struct packed_format *fmt = packed_format_of(ctyp);
		if(fmt == NULL) {
			/* FIXME: ensure this doesn't happen. */
			fprintf(stderr, "%s: struct `%s' not packable\n",
				__func__, s_name);
			abort();
		}
		char **names = NULL;
		T s_type = llvm_struct_type(ctx, &names, ctyp);
		/* TODO: fold this allocation with struct fields from other decoders,
		 * if invoked from a dispatcher. somehow. (v2?)
		 */
		dst[0] = build_local_storage(ctx, s_type, NULL, s_name);
		int names_len = 0;
		while(names[names_len] != NULL) names_len++;
		assert(names_len == fmt->num_items);
		assert(LLVMCountStructElementTypes(s_type) == names_len);
		T types[names_len];
		LLVMGetStructElementTypes(s_type, types);
		int cur_word = 0;
		V wordval = NULL;
		for(int i=0; i<fmt->num_items; i++) {
			const struct packed_item *pi = fmt->items[i];
			/* FIXME */
			if(pi->len > BITS_PER_WORD) continue;
			int field_ix = 0;
			while(names[field_ix] != NULL
				&& strcmp(names[field_ix], pi->name) != 0)
			{
				field_ix++;
			}
			if(names[field_ix] == NULL) {
				fprintf(stderr, "%s: not the way to go.\n", __func__);
				abort();
			}
			/* NOTE: this only works for non-compound members. struct members
			 * should get their own loop, or something.
			 */
			if(cur_word != pi->word || wordval == NULL) {
				cur_word = pi->word;
				wordval = build_utcb_load(ctx,
					LLVMBuildAdd(ctx->builder, first_mr,
						CONST_INT(pi->word),
						tmp_f(ctx->pr, "st.word%d.mr", pi->word)),
					tmp_f(ctx->pr, "st.word%d", pi->word));
			}
			V shifted = LLVMBuildLShr(ctx->builder, wordval,
				CONST_INT(pi->bit),
				tmp_f(ctx->pr, "st.word%d.shr%d", pi->word, pi->bit));
			V masked = LLVMBuildAnd(ctx->builder, shifted,
				CONST_WORD((1 << pi->len) - 1),
				tmp_f(ctx->pr, "st.word%d.s%d.m%d", pi->word, pi->bit,
					pi->len));
			LLVMBuildStore(ctx->builder,
				LLVMBuildTruncOrBitCast(ctx->builder, masked,
					types[field_ix], "st.val.cast"),
				LLVMBuildStructGEP(ctx->builder, dst[0], field_ix,
					"st.ptr"));
		}
		g_strfreev(names);
	} else if(IDL_NODE_TYPE(ctyp) == IDLN_TYPE_UNION) {
		NOTDEFINED(ctyp);
	} else if(IDL_NODE_TYPE(ctyp) == IDLN_TYPE_ARRAY) {
		/* FIXME: FAAAAAKE */
		dst[0] = ctx->zero;
	} else {
		/* genuinely not defined */
		NOTDEFINED(ctyp);
	}
}
