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
#include "llvmutil.h"


/* NOTE: offset is in _words_, since that's the unit the UTCB is addressed
 * with.
 */
static LLVMValueRef build_utcb_address(
	struct llvm_ctx *ctx,
	int offset,
	const char *name)
{
	LLVMValueRef off = LLVMConstInt(ctx->wordt, abs(offset), 0);
	if(offset < 0) off = LLVMBuildNeg(ctx->builder, off, "utcboffset");
	return LLVMBuildGEP(ctx->builder, ctx->utcb, &off, 1, name);
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


static LLVMValueRef build_ipc_input_val(struct llvm_ctx *ctx, int mr)
{
	if(mr == 0) return ctx->tag;
	else if(mr == 1) return ctx->mr1;
	else if(mr == 2) return ctx->mr2;
	else {
		return LLVMBuildLoad(ctx->builder,
			build_utcb_address(ctx, mr, "mr.addr"),
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
	} else if(is_value_type(ctyp)) {
		/* appropriate for all value types. */
		dst[0] = LLVMBuildTruncOrBitCast(ctx->builder,
			build_ipc_input_val(ctx, first_mr),
			llvm_value_type(ctx, ctyp), "shortparm");
	} else if(is_rigid_type(ctx->ns, ctyp)) {
		build_read_ipc_parameter_ixval(ctx, dst, ctyp,
			CONST_INT(first_mr));
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


#if 0
static bool is_signed(IDL_tree typ)
{
	assert(is_integral_type(typ));
	return IDL_NODE_TYPE(typ) == IDLN_TYPE_CHAR
		|| IDL_NODE_TYPE(typ) == IDLN_TYPE_WIDE_CHAR
		|| (IDL_NODE_TYPE(typ) == IDLN_TYPE_INTEGER
			&& IDL_TYPE_INTEGER(typ).f_signed);
}
#endif


/* returns # of MRs used.
 *
 * when is_value_type(ctyp), @val[0] is a C representation of @ctyp.
 * when is_rigid_type(..., ctyp) || IS_MAPGRANT_TYPE(ctyp), @val[0] is a
 * pointer to the same.
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
	} else if(IS_MAPGRANT_TYPE(ctyp)) {
		for(int i=0; i<2; i++) {
			LLVMBuildStore(ctx->builder,
				LLVMBuildLoad(ctx->builder,
					LLVMBuildStructGEP(ctx->builder, val[0], i,
						tmp_f(ctx->pr, "mg.field%d.ptr", i)),
					tmp_f(ctx->pr, "mg.field%d.val", i)),
				build_utcb_address(ctx, first_mr + i, "mg.store.addr"));
		}
		return 2;
	}

	/* single-word types */
	LLVMValueRef reg;
	if(is_integral_type(ctyp)) {
		reg = WORD(val[0]);
	} else {
		/* TODO! */
		NOTDEFINED(ctyp);
	}
	LLVMBuildStore(ctx->builder, reg,
		build_utcb_address(ctx, first_mr, "store.mr.addr"));
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
	if(is_value_type(type)) {
		dst[(*pos_p)++] = llvm_value_type(ctx, type);
	} else if(is_rigid_type(ctx->ns, type)) {
		dst[(*pos_p)++] = LLVMPointerType(llvm_rigid_type(ctx, type), 0);
	} else if(IDL_NODE_TYPE(type) == IDLN_TYPE_STRING) {
		dst[(*pos_p)++] = LLVMInt8TypeInContext(ctx->ctx);
	} else if(IDL_NODE_TYPE(type) == IDLN_TYPE_WIDE_STRING) {
		/* TODO: get wchar_t size from ABI! */
		dst[(*pos_p)++] = ctx->i32t;
	} else if(IDL_NODE_TYPE(type) == IDLN_TYPE_SEQUENCE) {
		IDL_tree seqtype = get_type_spec(
			IDL_TYPE_SEQUENCE(type).simple_type_spec);
		dst[(*pos_p)++] = LLVMPointerType(llvm_value_type(ctx, seqtype), 0);
		dst[(*pos_p)++] = ctx->i32t;
	} else {
		/* should not be reached, either. something fell out. */
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
	} else if(IS_MAPGRANT_TYPE(type)) {
		dst[(*pos_p)++] = LLVMPointerType(ctx->mapgrant, 0);
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


/* NOTE: this function must be called before referencing ctx->fncall_phi! */
static LLVMBasicBlockRef get_msgerr_bb(struct llvm_ctx *ctx)
{
	if(ctx->msgerr_bb == NULL) {
		ctx->msgerr_bb = add_sibling_block(ctx, "msgerr");

		BB prior = LLVMGetInsertBlock(ctx->builder);
		LLVMPositionBuilderAtEnd(ctx->builder, ctx->msgerr_bb);
		ctx->fncall_phi = LLVMBuildPhi(ctx->builder, ctx->i32t, "fncall.phi");
		LLVMValueRef msgerr_tag = LLVMBuildOr(ctx->builder,
			CONST_WORD(1), CONST_WORD(1 << 16), "msgerr.tag");
		LLVMBuildStore(ctx->builder,
			LLVMBuildNeg(ctx->builder, ctx->fncall_phi, "rcneg.val"),
			build_utcb_address(ctx, 1, "mr1.addr"));
		branch_set_phi(ctx, ctx->reply_tag, msgerr_tag);
		LLVMBuildBr(ctx->builder, ctx->reply_bb);

		LLVMPositionBuilderAtEnd(ctx->builder, prior);
	}

	return ctx->msgerr_bb;
}


static struct msg_param *find_pdecl(GList *list, IDL_tree pdecl)
{
	GLIST_FOREACH(cur, list) {
		struct msg_param *p = cur->data;
		if(p->param_dcl == pdecl) return p;
	}
	return NULL;
}


static void emit_in_param(
	struct llvm_ctx *ctx,
	LLVMValueRef *args,
	int *arg_pos_p,
	const struct method_info *inf,
	const struct stritem_info *stritems,
	IDL_tree pdecl)
{
	const struct message_info *req = inf->request;

	struct msg_param *u = find_pdecl(req->untyped, pdecl);
	if(u != NULL) {
		build_read_ipc_parameter(ctx, &args[(*arg_pos_p)++],
			u->X.untyped.type, u->X.untyped.first_reg);
		return;
	}

	/* inline sequence? */
	struct msg_param *seq = find_pdecl(req->seq, pdecl);
	if(seq != NULL) {
		/* this only works if inline sequences appear as parameters to this
		 * function in the same order as they were in IDL. if not,
		 * inline_seq_pos will be wrong.
		 */
		LLVMBasicBlockRef err_bb = get_msgerr_bb(ctx);
		LLVMValueRef new_upos = build_decode_inline_sequence(ctx,
			args, arg_pos_p, seq, ctx->inline_seq_pos,
			seq == g_list_last(req->seq)->data,
			ctx->fncall_phi, err_bb);
		ctx->inline_seq_pos = new_upos;
		return;
	}

	/* long parameter? */
	struct msg_param *lp = NULL;
	int lp_offset = 0;
	GLIST_FOREACH(cur, req->_long) {
		struct msg_param *p = cur->data;
		if(p->param_dcl == pdecl) {
			lp = p;
			break;
		}
		lp_offset++;
	}
	if(lp != NULL) {
		assert(stritems != NULL);
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
		branch_set_phi(ctx, ctx->fncall_phi, einval);
		LLVMBuildCondBr(ctx->builder, tpos_pc, pre_ok_bb, msgerr_bb);

		/* call lenfn, branch off to msgerr if retval indicates failure */
		LLVMPositionBuilderAtEnd(ctx->builder, pre_ok_bb);
		LLVMValueRef item_len_bytes = NULL;
		ctx->tpos = build_recv_stritem_len(ctx, &item_len_bytes, ctx->tpos);
		LLVMValueRef fail_cond = LLVMBuildICmp(ctx->builder, LLVMIntUGT,
			ctx->tpos, tmax_plus_one, "stritem.fail.cond");
		LLVMAddIncoming(ctx->fncall_phi, &einval, &pre_ok_bb, 1);
		LLVMBuildCondBr(ctx->builder, fail_cond, msgerr_bb, cont_bb);

		/* the actual sequence decode. */
		LLVMPositionBuilderAtEnd(ctx->builder, cont_bb);
		switch(IDL_NODE_TYPE(type)) {
			case IDLN_TYPE_STRING: {
				/* <i8 *> is exactly what we need. */
				args[(*arg_pos_p)++] = stritems[lp_offset].memptr;
				/* null-terminate it, though. */
				LLVMBuildStore(ctx->builder,
					LLVMConstInt(LLVMInt8TypeInContext(ctx->ctx), 0, 0),
					LLVMBuildGEP(ctx->builder, stritems[lp_offset].memptr,
						&item_len_bytes, 1, "str.nullpo"));	/* ガ！ */
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
				args[(*arg_pos_p)++] = ptr;
				args[(*arg_pos_p)++] = len;
				break;
			}

			case IDLN_TYPE_WIDE_STRING:
			case IDLN_TYPE_STRUCT:
			case IDLN_TYPE_UNION:
			case IDLN_TYPE_ARRAY:
				/* TODO */

			default:
				NOTDEFINED(type);
		}
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
	} else if(IS_MAPGRANT_TYPE(ptyp)) {
		/* this is just a pointer to a struct of 2 words. */
		args[(*arg_pos_p)++] = build_local_storage(ctx,
			ctx->mapgrant, NULL, "outparam.mapgrant.mem");
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
	const struct method_info *inf,
	const struct stritem_info *stritems)
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

	LLVMPositionBuilderAtEnd(ctx->builder, bb);

	/* collection of arguments according to op decl. */
	const struct message_info *req = inf->request;
	bool rv_actual = false;
	LLVMTypeRef rv_type = vtable_return_type(ctx, inf->node, &rv_actual);

	ctx->inline_seq_pos = LLVMConstInt(ctx->i32t,
		inf->request->tag_u + 1, 0);
	LLVMValueRef tag_u_val = build_u_from_tag(ctx, ctx->tag);
	ctx->tmax = LLVMBuildAdd(ctx->builder,
		tag_u_val, build_t_from_tag(ctx, ctx->tag), "tmax");
	ctx->tpos = LLVMBuildAdd(ctx->builder, LLVMConstInt(ctx->i32t, 1, 0),
		tag_u_val, "tpos.initial");
	const int num_args_max = g_list_length(req->untyped) + 1
		+ 2 * g_list_length(req->seq) + 2 * g_list_length(req->_long);
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
			emit_in_param(ctx, args, &arg_pos, inf, stritems, p);
		} else if(attr == IDL_PARAM_OUT) {
			emit_out_param(ctx, args, &arg_pos,
				get_type_spec(IDL_PARAM_DCL(p).param_type_spec));
		} else /* inout */ {
			/* hax! */
			IDL_tree typ = get_type_spec(IDL_PARAM_DCL(p).param_type_spec);
			int start = arg_pos;
			emit_out_param(ctx, args, &arg_pos, typ);
			LLVMValueRef in_args[2];
			int in_arg_pos = 0;
			emit_in_param(ctx, in_args, &in_arg_pos, inf, stritems, p);
			assert(in_arg_pos == arg_pos - start);
			/* insert tab A in slot B */
			for(int i=0; i<in_arg_pos; i++) {
				if(is_value_type(typ)) {
					/* just a value */
					LLVMBuildStore(ctx->builder, in_args[i], args[start + i]);
				} else if(IDL_NODE_TYPE(typ) == IDLN_TYPE_SEQUENCE) {
					/* a pointer and a length value. */
					assert(i + 1 < in_arg_pos);
					args[start + i] = in_args[i];
					LLVMBuildStore(ctx->builder, in_args[i + 1],
						args[start + i + 1]);
					i++;
				} else {
					/* single pointer arguments encode structs (whether rigid
					 * or not), mapgrant items, arrays, strings, and wide
					 * strings. convenient, huh?
					 */
					args[start + i] = in_args[i];
				}
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
		LLVMBasicBlockRef current_bb = LLVMGetInsertBlock(ctx->builder),
			msgerr_bb = get_msgerr_bb(ctx);
		LLVMAddIncoming(ctx->fncall_phi, &fncall, &current_bb, 1);
		LLVMBuildCondBr(ctx->builder, ok_cond, ex_chain_bb, msgerr_bb);
	}

	/* pack results from the non-exceptional return. */
	LLVMPositionBuilderAtEnd(ctx->builder, pr_bb);
	assert(inf->num_reply_msgs > 0);
	assert(!IS_EXN_MSG(inf->replies[0]));
	const struct message_info *reply = inf->replies[0];
	int mr_pos = 1;
	/* return value */
	if(inf->return_type != NULL) {
		LLVMValueRef val[2];
		char *rv_name = tmp_f(pr, "%s.retval", opname);
		if(is_value_type(inf->return_type)) {
			if(rv_actual) {
				val[0] = LLVMBuildTruncOrBitCast(ctx->builder, fncall,
					llvm_value_type(ctx, inf->return_type), rv_name);
			} else {
				assert(retvalptr != NULL);
				val[0] = LLVMBuildLoad(ctx->builder, retvalptr, rv_name);
			}
		} else if(IS_MAPGRANT_TYPE(inf->return_type)
			|| is_rigid_type(ctx->ns, inf->return_type))
		{
			val[0] = retvalptr;
		} else {
			/* TODO: add the other types */
			NOTDEFINED(inf->return_type);
		}
		mr_pos += build_write_ipc_parameter(ctx, val,
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
		} else if(IS_MAPGRANT_TYPE(typ) || is_rigid_type(ctx->ns, typ)) {
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
	ctx->inline_seq_pos = LLVMConstInt(ctx->i32t, reply->tag_u + 1, 0);
	GLIST_FOREACH(cur, reply->seq) {
		const struct msg_param *seq = cur->data;
		int first_arg = first_arg_index(reply->node, seq->param_dcl);
		assert(first_arg >= 0);
		ctx->inline_seq_pos = build_encode_inline_sequence(ctx,
			args[first_arg], args[first_arg + 1], seq,
			ctx->inline_seq_pos, g_list_next(cur) == NULL);
	}

	/* TODO: encode typed words (long & complex sequences, strings, wide
	 * strings)
	 */

	/* epilogue */
	/* flags = 0, u = inline_seq_pos - 1, label = 0, t = 0 */
	LLVMValueRef u_val = LLVMBuildSub(ctx->builder, ctx->inline_seq_pos,
			LLVMConstInt(ctx->i32t, 1, 0),
			tmp_f(ctx->pr, "%s.reply.u", opname)),
		tag = WORD(u_val);	/* simple. */
	branch_set_phi(ctx, ctx->reply_tag, tag);
	LLVMBuildBr(ctx->builder, ctx->reply_bb);

	return bb;
}


/* (this actually allocates memory that is pointed to by the string items in
 * the buffer registers.)
 */
static void build_alloc_stritems(
	struct llvm_ctx *ctx,
	struct stritem_info *stritems)
{
	if(stritems == NULL) return;
	for(int i=0; stritems[i].length >= 0; i++) {
		/* TODO: the "4" is sizeof(wchar_t); get this from the ABI spec */
		int len = stritems[i].length + (stritems[i].stringlike ? 4 : 0);
		/* TODO: allocate small blocks (by some reasonable criteria... a single
		 * cache line's size, or something) with build_local_storage()
		 */
		stritems[i].memptr = build_malloc_storage(ctx,
			LLVMInt8TypeInContext(ctx->ctx),
			LLVMConstInt(ctx->i32t, len, 0),
			tmp_f(ctx->pr, "stritem%d.ptr", i));
	}
}


static void build_store_br(
	struct llvm_ctx *ctx,
	LLVMValueRef value,
	int br)
{
	assert(br <= 32 && br >= 0);
	LLVMValueRef br_addr = build_utcb_address(ctx,
		br * -4 - 64, tmp_f(ctx->pr, "br%d.ptr", br));
	LLVMBuildStore(ctx->builder, value, br_addr);
}


static void build_set_strbufs(
	struct llvm_ctx *ctx,
	const struct stritem_info *stritems)
{
	assert(stritems != NULL && stritems[0].length >= 0);
	for(int i=0; stritems[i].length >= 0; i++) {
		LLVMValueRef si[2];
		build_simple_string_item(ctx, si, stritems[i].memptr,
			LLVMConstInt(ctx->wordt, stritems[i].length, 0),
			NULL);
		build_store_br(ctx, si[0], i * 2 + 1);
		if(stritems[i+1].length >= 0) {
			/* non-last string buffers have low bit, "C", set. */
			si[1] = LLVMBuildOr(ctx->builder, si[1],
				LLVMConstInt(ctx->wordt, 1, 0), "strbuf.info.cont");
		}
		build_store_br(ctx, si[1], i * 2 + 2);
	}
}


static gint method_by_tagmask_cmp(gconstpointer ap, gconstpointer bp)
{
	const struct method_info *a = ap, *b = bp;
	if(a->request->tagmask < b->request->tagmask) return -1;
	else if(a->request->tagmask > b->request->tagmask) return 1;
	else return 0;
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

	/* the entry block. */
	LLVMPositionBuilderAtEnd(ctx->builder, bb);
	ctx->utcb = build_utcb_get(ctx);
	ctx->alloc_bb = bb;
	ctx->malloc_ptrs = NULL;
	/* store xfer timeouts at point of entry, i.e. as they are given by the
	 * caller.
	 */
	LLVMValueRef xfer_timeouts_addr = build_utcb_address(ctx, -8, "xferto.addr"),
		stored_timeouts = LLVMBuildLoad(ctx->builder, xfer_timeouts_addr,
			"stored_timeouts");
	/* string buffers */
	struct stritem_info *stritems = dispatcher_stritems(methods);
	build_alloc_stritems(ctx, stritems);
	const bool have_stringbufs = stritems != NULL && stritems[0].length >= 0;
	/* acceptor word (TODO: map/grant items) */
	LLVMValueRef acceptor = LLVMConstInt(ctx->wordt,
		have_stringbufs ? 1 : 0, 0);
	/* (this block will be closed as ctx->alloc_bb down near function end.) */

	/* non-reply IPC wait block. */
	LLVMPositionBuilderAtEnd(ctx->builder, ctx->wait_bb);
	LLVMBuildStore(ctx->builder, stored_timeouts, xfer_timeouts_addr);
	build_store_br(ctx, acceptor, 0);
	if(have_stringbufs) build_set_strbufs(ctx, stritems);
	LLVMValueRef ipc_from, ipc_mr1, ipc_mr2,
		ipc_tag = build_l4_ipc_call(ctx,
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
	LLVMBuildStore(ctx->builder, stored_timeouts, xfer_timeouts_addr);
	build_store_br(ctx, acceptor, 0);
	if(have_stringbufs) build_set_strbufs(ctx, stritems);
	ipc_tag = build_l4_ipc_call(ctx,
		ctx->from, LLVMConstNot(ctx->zero), LLVMConstNot(ctx->zero),
		ctx->reply_tag, &ipc_from, &ipc_mr1, &ipc_mr2);
	LLVMAddIncoming(ctx->from, &ipc_from, &ctx->reply_bb, 1);
	LLVMAddIncoming(ctx->mr1, &ipc_mr1, &ctx->reply_bb, 1);
	LLVMAddIncoming(ctx->mr2, &ipc_mr2, &ctx->reply_bb, 1);
	LLVMAddIncoming(ctx->tag, &ipc_tag, &ctx->reply_bb, 1);
	LLVMBuildBr(ctx->builder, loop_bb);

	/* exit_bb's interface */
	LLVMPositionBuilderAtEnd(ctx->builder, exit_bb);
	LLVMValueRef retval = LLVMBuildPhi(ctx->builder, ctx->wordt, "retval");

	/* return L4_ErrorCode(); */
	LLVMPositionBuilderAtEnd(ctx->builder, ret_ec_bb);
	LLVMValueRef errorcode = LLVMBuildLoad(ctx->builder,
		build_utcb_address(ctx, -9, "errcode.addr"), "errcode");
	LLVMAddIncoming(retval, &errorcode, &ret_ec_bb, 1);
	LLVMBuildBr(ctx->builder, exit_bb);

	/* recognize interfaces that've got tag-mask dispatching. */
	GList *tm_list = NULL;
	for(GList *cur = g_list_first(methods);
		cur != NULL;
		cur = g_list_next(cur))
	{
		struct method_info *inf = cur->data;
		if(inf->request->tagmask != NO_TAGMASK) {
			assert(inf->request->sublabel == NO_SUBLABEL);
			tm_list = g_list_prepend(tm_list, inf);
		}
	}
	tm_list = g_list_reverse(tm_list);
	LLVMBasicBlockRef tm_dispatch_bb = NULL;
	if(tm_list != NULL) {
		tm_dispatch_bb = LLVMAppendBasicBlockInContext(ctx->ctx,
			fn, "tagmask.dispatch");
	}

	/* dispatch according to ctx->tag. */
	LLVMPositionBuilderAtEnd(ctx->builder, dispatch_bb);
	/* TODO: get the correct value */
	LLVMValueRef labelswitch = LLVMBuildSwitch(ctx->builder,
		build_label_from_tag(ctx, ctx->tag),
		tm_list != NULL ? tm_dispatch_bb : exit_bb, 2);

	/* tag-mask dispatching. */
	if(tm_list != NULL) {
		LLVMPositionBuilderAtEnd(ctx->builder, tm_dispatch_bb);
		tm_list = g_list_sort(tm_list, &method_by_tagmask_cmp);
		uint32_t prior = NO_TAGMASK;
		LLVMValueRef mask_switch = NULL;
		for(GList *cur = g_list_first(tm_list);
			cur != NULL;
			cur = g_list_next(cur))
		{
			const struct method_info *inf = cur->data;
			if(prior != inf->request->tagmask) {
				/* chain start. */
				prior = inf->request->tagmask;
				tm_dispatch_bb = LLVMAppendBasicBlockInContext(ctx->ctx, fn,
					tmp_f(ctx->pr, "tagmask.%x.next", prior));
				/* TODO: compute the right number of branches? */
				LLVMValueRef masked_label = LLVMBuildLShr(ctx->builder,
					LLVMBuildAnd(ctx->builder, ctx->tag,
						LLVMConstInt(ctx->wordt, prior, 0),
						"tag.masked"),
					LLVMConstInt(ctx->i32t, 16, 0),
					"label.masked");
				mask_switch = LLVMBuildSwitch(ctx->builder,
					masked_label, tm_dispatch_bb, 2);
				LLVMPositionBuilderAtEnd(ctx->builder, tm_dispatch_bb);
			}

			/* chain member. */
			LLVMAddCase(mask_switch,
				LLVMConstInt(ctx->wordt, inf->request->label, 0),
				build_op_decode(ctx, fn, inf, stritems));
		}
		g_list_free(tm_list);
	}

	/* return 42666 (a µIDL special value) on unrecognized label. */
	if(tm_dispatch_bb != NULL) {
		LLVMPositionBuilderAtEnd(ctx->builder, tm_dispatch_bb);
	}
	LLVMBasicBlockRef current = LLVMGetInsertBlock(ctx->builder);
	LLVMValueRef unknownlabel = LLVMConstInt(ctx->wordt, 42666, 0);
	LLVMAddIncoming(retval, &unknownlabel, &current, 1);
	if(tm_dispatch_bb != NULL) {
		/* must branch explicitly. the non-bb path exits from the switch
		 * instruction.
		 */
		LLVMBuildBr(ctx->builder, exit_bb);
	}

	int top_label = -1;
	LLVMValueRef sublabelswitch = NULL;
	for(GList *cur = g_list_first(methods);
		cur != NULL;
		cur = g_list_next(cur))
	{
		struct method_info *inf = cur->data;
		/* tag-mask dispatching is handled above */
		if(inf->request->tagmask != NO_TAGMASK) continue;
		if(inf->request->sublabel == NO_SUBLABEL) {
			LLVMBasicBlockRef decode_bb = build_op_decode(ctx, fn, inf,
				stritems);
			LLVMAddCase(labelswitch,
				LLVMConstInt(ctx->wordt, inf->request->label, 0),
				decode_bb);
		} else {
			if(top_label != -1 && top_label != inf->request->label) {
				/* restore pre-first state */
				top_label = -1;
				sublabelswitch = NULL;
			}
			if(top_label == -1) {
				/* begin a new sub-switch. */
				assert(sublabelswitch == NULL);
				top_label = inf->request->label;
				LLVMBasicBlockRef sub_bb = LLVMAppendBasicBlockInContext(
					ctx->ctx, fn, tmp_f(ctx->pr, "sub%04x.dispatch",
						(unsigned)top_label));
				LLVMPositionBuilderAtEnd(ctx->builder, sub_bb);
				/* TODO: get the correct value for "2" */
				sublabelswitch = LLVMBuildSwitch(ctx->builder,
					ctx->mr1, exit_bb, 2);
				LLVMAddIncoming(retval, &unknownlabel, &sub_bb, 1);
				LLVMAddCase(labelswitch,
					LLVMConstInt(ctx->wordt, top_label, 0), sub_bb);
			}
			assert(sublabelswitch != NULL);
			assert(top_label == inf->request->label);
			LLVMAddCase(sublabelswitch,
				LLVMConstInt(ctx->wordt, inf->request->sublabel, 0),
				build_op_decode(ctx, fn, inf, stritems));
		}
	}

	/* close off the alloc (entry) block. */
	LLVMPositionBuilderAtEnd(ctx->builder, ctx->alloc_bb);
	LLVMBuildBr(ctx->builder, ctx->wait_bb);

	/* and emit free insns for all blocks in the malloc list */
	LLVMPositionBuilderAtEnd(ctx->builder, exit_bb);
	build_free_mallocs(ctx);
	LLVMBuildRet(ctx->builder, retval);

	LLVMDisposeBuilder(ctx->builder);
	ctx->builder = NULL;
	g_list_foreach(methods, (GFunc)free_method_info, NULL);
	g_list_free(methods);
	g_list_free(tagmask_list);
	g_free(stritems);
	return fn;
}
