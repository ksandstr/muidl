/*
 * except.c -- encoding and decoding of exceptions
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

#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <glib.h>
#include <libIDL/IDL.h>
#include <llvm-c/Core.h>

#include "muidl.h"
#include "llvmutil.h"
#include "l4x2.h"


uint32_t exn_hash(IDL_tree exn)
{
	GString *str = g_string_sized_new(256);
	int words_total = 0;

	bool first = true;
	IDL_LIST_FOREACH(m_cur, IDL_EXCEPT_DCL(exn).members) {
		IDL_tree member = IDL_LIST(m_cur).data,
			mtype = get_type_spec(IDL_MEMBER(member).type_spec);

		int mtype_bits, mtype_words;
		if(is_rigid_type(mtype)) {
			mtype_bits = size_in_bits(mtype);
			mtype_words = size_in_words(mtype);
		} else {
			mtype_bits = max_size(mtype) * 8;
			mtype_words = (mtype_bits + BITS_PER_WORD - 1) / BITS_PER_WORD;
		}

		IDL_LIST_FOREACH(d_cur, IDL_MEMBER(member).dcls) {
			IDL_tree dcl = IDL_LIST(d_cur).data;
			int count = -1;
			const char *name = "********";
			if(IDL_NODE_TYPE(dcl) == IDLN_IDENT) {
				count = 1;
				name = IDL_IDENT(dcl).str;
			} else {
				assert(IDL_NODE_TYPE(dcl) == IDLN_TYPE_ARRAY);
				count = 1;
				name = IDL_IDENT(IDL_TYPE_ARRAY(dcl).ident).str;
				IDL_LIST_FOREACH(size_cur, IDL_TYPE_ARRAY(dcl).size_list) {
					count *= IDL_INTEGER(IDL_LIST(size_cur).data).value;
				}
			}
			if(first) first = false; else g_string_append_c(str, ' ');
			g_string_append_printf(str, "%s:%d", name, count * mtype_bits);
			words_total += count * mtype_words;
		}
	}

	g_string_append_printf(str, "%s%s %d",
		str->len > 0 ? " " : "",
		IDL_IDENT_REPO_ID(IDL_EXCEPT_DCL(exn).ident), words_total);
	uint32_t ret = djb2_hash(str->str);
	g_string_free(str, TRUE);
	return ret;
}


char *exn_raise_fn_name(IDL_tree exn)
{
	const char *repo_id = IDL_IDENT(IDL_EXCEPT_DCL(exn).ident).repo_id;
	char **colon_parts = g_strsplit(repo_id, ":", 0);
	assert(colon_parts != NULL);
	assert(g_strv_length(colon_parts) >= 3);
	char **scope_parts = g_strsplit(colon_parts[1], "/", 0);
	assert(scope_parts != NULL);
	int scope_len = g_strv_length(scope_parts);
	assert(scope_len > 0);
	GString *raiser_name = g_string_new("");
	for(int i=0; i<scope_len; i++) {
		bool last = i == scope_len - 1;
		if(last) g_string_append(raiser_name, "raise_");
		char *m = mangle_repo_id(scope_parts[i]);
		g_string_append(raiser_name, m);
		g_free(m);
		if(!last) g_string_append_c(raiser_name, '_');
	}
	g_strfreev(scope_parts);
	g_strfreev(colon_parts);
	return g_string_free(raiser_name, FALSE);
}


static LLVMTypeRef exn_raise_fn_type(struct llvm_ctx *ctx, IDL_tree node)
{
	struct member_item *members = expand_member_list(
		IDL_EXCEPT_DCL(node).members);
	GPtrArray *types = g_ptr_array_new();
	for(int i=0; members[i].type != NULL; i++) {
		const struct member_item *m = &members[i];
		const bool isvt = is_value_type(m->type);
		T typ;
		if(isvt && m->dim > 0) {
			typ = LLVMPointerType(llvm_value_type(ctx, m->type), 0);
		} else if(!isvt) {
			/* nonvalue types are passed by pointer; an array of those is
			 * either disallowed (seqs, strings) or just a pointer to more
			 * than one.
			 */
			T mt;
			if(is_rigid_type(m->type)) {
				mt = llvm_rigid_type(ctx, m->type);
			} else if(IDL_NODE_TYPE(m->type) == IDLN_TYPE_STRING) {
				mt = LLVMInt8TypeInContext(ctx->ctx);
			} else if(IDL_NODE_TYPE(m->type) == IDLN_TYPE_WIDE_STRING) {
				mt = ctx->i32t;
			} else if(IDL_NODE_TYPE(m->type) == IDLN_TYPE_SEQUENCE) {
				mt = NULL;
				T subtype = llvm_rigid_type(ctx, get_type_spec(
						IDL_TYPE_SEQUENCE(m->type).simple_type_spec));
				g_ptr_array_add(types, LLVMPointerType(subtype, 0));
				g_ptr_array_add(types, ctx->i32t);	/* length parameter */
			} else {
				NOTDEFINED(m->type);
			}

			if(mt == NULL) typ = NULL; else typ = LLVMPointerType(mt, 0);
		} else {
			assert(m->dim == 0);
			typ = llvm_value_type(ctx, m->type);
		}
		if(typ != NULL) g_ptr_array_add(types, typ);
	}
	g_free(members);

	T ret = LLVMFunctionType(LLVMVoidTypeInContext(ctx->ctx),
		(LLVMTypeRef *)types->pdata, types->len, 0);
	g_ptr_array_free(types, TRUE);
	return ret;
}


static LLVMValueRef build_exn_raise_fn(
	struct llvm_ctx *ctx,
	const char *fn_name,
	const struct message_info *exn)
{
	T fntype = exn_raise_fn_type(ctx, exn->node);
	V fn = LLVMAddFunction(ctx->module, fn_name, fntype);
	begin_function(ctx, fn);
	BB start_bb = add_sibling_block(ctx, "start"),
		exit_bb = add_sibling_block(ctx, "exit");

	LLVMPositionBuilderAtEnd(ctx->builder, start_bb);
	V supp_ptr = build_fetch_supp_ctx(ctx);

	V tag;
	if(is_noreply_exn(exn->node)) {
		/* special thing. */
		tag = CONST_WORD(~0ull);
	} else {
		tag = LLVMBuildShl(ctx->builder, CONST_WORD(2), CONST_WORD(16),
			"label.shifted");
		unsigned num_args = LLVMCountParams(fn);
		V *args = g_new(V, num_args);
		LLVMGetParams(fn, args);
		tag = build_msg_encoder(ctx, exn, NULL, args, false);
		g_free(args);
	}

	/* store the tag to indicate a raised exception. */
	/* FIXME: "2" is external knowledge; it's defined as SUPP_EXN_TAG_IX in
	 * dispatch.c, and should be moved into muidl.h .
	 */
	LLVMBuildStore(ctx->builder, tag,
		LLVMBuildStructGEP(ctx->builder, supp_ptr, 2,
			"supp.exntag.ptr"));

	LLVMBuildBr(ctx->builder, exit_bb);

	LLVMPositionBuilderAtEnd(ctx->builder, exit_bb);
	end_function(ctx, start_bb);
	LLVMBuildRetVoid(ctx->builder);

	return fn;
}


LLVMValueRef build_exception_raise_fns_for_iface(
	struct llvm_ctx *ctx,
	const struct iface_info *iface)
{
	GHashTable *seen_exn_hash = g_hash_table_new(&g_str_hash, &g_str_equal);
	GLIST_FOREACH(o_cur, iface->ops) {
		const struct method_info *op = o_cur->data;
		for(int i=1; i < op->num_reply_msgs; i++) {
			const struct message_info *msg = op->replies[i];
			const char *key = IDL_IDENT_REPO_ID(
				IDL_EXCEPT_DCL(msg->node).ident);
			if(g_hash_table_lookup(seen_exn_hash, key) != NULL
				|| is_negs_exn(msg->node))
			{
				/* already emitted for this module, or one of those exceptions
				 * that doesn't get a raise function
				 */
				continue;
			}
			g_hash_table_insert(seen_exn_hash, (char *)key, (void *)msg);

			char *name = exn_raise_fn_name(msg->node);
			V fn = build_exn_raise_fn(ctx, name, msg);
			g_free(name);

			/* set collapsible linkage. */
			LLVMSetLinkage(fn, LLVMLinkOnceAnyLinkage);
		}
	}

	g_hash_table_destroy(seen_exn_hash);
	return NULL;
}


/* TODO: memoize the typeref on the iface name under a hash in ctx */
LLVMTypeRef context_type_of_iface(struct llvm_ctx *ctx, IDL_tree iface)
{
	assert(IDL_NODE_TYPE(iface) == IDLN_INTERFACE);

	GList *exn_list = iface_exns_sorted(ctx->ns, iface);
	if(exn_list == NULL) return NULL;

	GPtrArray *u_types = g_ptr_array_new(),
		*e_types = g_ptr_array_new();
	/* the tag is the exception hash. */
	g_ptr_array_add(u_types, ctx->i32t);

	GLIST_FOREACH(e_cur, exn_list) {
		IDL_tree exn = e_cur->data;

		g_ptr_array_set_size(e_types, 0);
		g_ptr_array_add(e_types, ctx->i32t);
		struct member_item *members = expand_member_list(
			IDL_EXCEPT_DCL(exn).members);
		for(int i=0; members[i].type != NULL; i++) {
			struct member_item *mi = &members[i];
			T m;
			if(mi->dim > 0) {
				assert(is_rigid_type(mi->type));
				m = LLVMArrayType(llvm_rigid_type(ctx, mi->type), mi->dim);
			} else if(is_rigid_type(mi->type)) {
				m = llvm_rigid_type(ctx, mi->type);
			} else if(IDL_NODE_TYPE(mi->type) == IDLN_TYPE_STRING) {
				int len = IDL_INTEGER(
					IDL_TYPE_STRING(mi->type).positive_int_const).value;
				m = LLVMArrayType(LLVMInt8TypeInContext(ctx->ctx), len + 1);
				/* (TODO: wide strings) */
			} else {
				assert(IDL_NODE_TYPE(mi->type) == IDLN_TYPE_SEQUENCE);
				int len = IDL_INTEGER(
					IDL_TYPE_SEQUENCE(mi->type).positive_int_const).value;
				T subtype = llvm_rigid_type(ctx, get_type_spec(
						IDL_TYPE_SEQUENCE(mi->type).simple_type_spec));
				g_ptr_array_add(e_types, LLVMArrayType(subtype, len));
				g_ptr_array_add(e_types, ctx->i32t);
				m = NULL;
			}

			if(m != NULL) g_ptr_array_add(e_types, m);
		}
		g_free(members);
		T st = LLVMStructTypeInContext(ctx->ctx, (T *)e_types->pdata,
			e_types->len, 0);
		g_ptr_array_add(u_types, st);
	}

	T ctx_type = LLVMUnionTypeInContext(ctx->ctx, (T *)u_types->pdata,
		u_types->len);

	g_ptr_array_free(e_types, TRUE);
	g_ptr_array_free(u_types, TRUE);
	g_list_free(exn_list);

	return ctx_type;
}


void build_decode_exception(
	struct llvm_ctx *ctx,
	LLVMValueRef ex_ptr,
	const struct message_info *msg,
	int msg_index,
	const struct stritem_info *stritems)
{
	assert(msg->ret_type == NULL);

	GList *params = g_list_concat(
		g_list_concat(g_list_copy(msg->untyped), g_list_copy(msg->seq)),
		g_list_concat(g_list_copy(msg->mapped), g_list_copy(msg->string)));

	int max_arg = 0;
	GLIST_FOREACH(p_cur, params) {
		const struct msg_param *p = p_cur->data;
		max_arg = MAX(max_arg, p->arg_ix);
	}

	/* set up the spots where out-parameters get stored. */
	V args[max_arg + 1];	/* (+1 in case the last one is a sequence) */
	for(int i=0; i < max_arg + 1; i++) args[i] = NULL;
	GLIST_FOREACH(p_cur, params) {
		const struct msg_param *p = p_cur->data;
		if(p->kind == P_STRING) {
			V indexes[2] = { CONST_INT(0), CONST_INT(0) };
			args[p->arg_ix + 0] = LLVMBuildGEP(ctx->builder,
				LLVMBuildStructGEP(ctx->builder, ex_ptr,
					p->arg_ix + 1, "ex.member.seq.data.addr"),
				indexes, 2, "ex.member.seq.data.1stptr");
			if(IDL_NODE_TYPE(p->type) == IDLN_TYPE_SEQUENCE) {
				args[p->arg_ix + 1] = LLVMBuildStructGEP(ctx->builder, ex_ptr,
					p->arg_ix + 2, "ex.member.seq.len.ptr");
			}
		} else {
			args[p->arg_ix] = LLVMBuildStructGEP(ctx->builder, ex_ptr,
				p->arg_ix + 1, "ex.member.ptr");
		}
	}

	/* decode the message. */
	build_msg_decoder(ctx, NULL, args, msg, msg_index, stritems, true);

	g_list_free(params);
}
