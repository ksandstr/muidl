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
#include <stdbool.h>
#include <llvm-c/Core.h>
#include <libIDL/IDL.h>

#include "muidl.h"
#include "llvmutil.h"
#include "l4x2.h"


bool is_integral_type(IDL_tree typ)
{
	switch(IDL_NODE_TYPE(typ)) {
		case IDLN_TYPE_INTEGER:
		case IDLN_TYPE_OCTET:
		case IDLN_TYPE_CHAR:
		case IDLN_TYPE_WIDE_CHAR:
		case IDLN_TYPE_BOOLEAN:
			return true;

		default:
			return IS_WORD_TYPE(typ);
	}
}


#if 0
bool is_signed(IDL_tree typ)
{
	assert(is_integral_type(typ));
	return IDL_NODE_TYPE(typ) == IDLN_TYPE_CHAR
		|| IDL_NODE_TYPE(typ) == IDLN_TYPE_WIDE_CHAR
		|| (IDL_NODE_TYPE(typ) == IDLN_TYPE_INTEGER
			&& IDL_TYPE_INTEGER(typ).f_signed);
}
#endif


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
				abort();
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

	T ret = LLVMStructTypeInContext(ctx->ctx, &g_array_index(types, T, 0),
		types->len, is_packed(type) ? 1 : 0);
	g_array_free(types, TRUE);
	return ret;
}


LLVMTypeRef llvm_rigid_type(struct llvm_ctx *ctx, IDL_tree type)
{
	if(type == NULL) return LLVMVoidTypeInContext(ctx->ctx);
	switch(IDL_NODE_TYPE(type)) {
		case IDLN_TYPE_STRUCT:
			return llvm_struct_type(ctx, NULL, type);

		case IDLN_TYPE_ARRAY: {
			IDL_tree mt = get_array_type(type);
			return LLVMArrayType(llvm_rigid_type(ctx, mt),
				ARRAY_TYPE_LENGTH(type));
		}

#if 0
		case IDLN_TYPE_UNION:
			/* TODO */
#endif

		case IDLN_NATIVE:
			if(IS_MAPGRANT_TYPE(type)) return ctx->mapgrant;
			/* FALL THRU */
		default:
			if(is_value_type(type)) return llvm_value_type(ctx, type);
			else {
				NOTDEFINED(type);
			}
	}
}


/* FIXME: this should really be in message.c . */
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
		low = LLVMBuildZExtOrBitCast(ctx->builder, low, i64t,
			"longparm.lo.cast");
		high = LLVMBuildZExtOrBitCast(ctx->builder, high, i64t,
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
			build_utcb_load(ctx, first_mr, "shortparm.mr"),
			llvm_value_type(ctx, ctyp), "shortparm");
	} else if(IDL_NODE_TYPE(ctyp) == IDLN_TYPE_STRUCT) {
		decode_packed_struct(ctx, &dst[0], ctyp, first_mr, CONST_INT(0));
	} else if(IDL_NODE_TYPE(ctyp) == IDLN_TYPE_UNION) {
		NOTDEFINED(ctyp);
	} else if(IDL_NODE_TYPE(ctyp) == IDLN_TYPE_ARRAY) {
		/* FIXME: FAAAAAKE */
		dst[0] = ctx->zero;
	} else if(IS_MAPGRANT_TYPE(ctyp)) {
		dst[0] = build_local_storage(ctx, ctx->mapgrant, NULL,
			"mapgrant.mem");
		LLVMBuildStore(ctx->builder,
			build_utcb_load(ctx, first_mr, "mapgrant.info"),
			LLVMBuildStructGEP(ctx->builder, dst[0], 0, "mgitem.info.ptr"));
		LLVMBuildStore(ctx->builder,
			build_utcb_load(ctx, LLVMBuildAdd(ctx->builder, first_mr,
					CONST_INT(1), "off.bump"),
				"mapgrant.fpage"),
			LLVMBuildStructGEP(ctx->builder, dst[0], 1, "mgitem.fpage.ptr"));
	} else {
		/* genuinely not defined */
		NOTDEFINED(ctyp);
	}
}
