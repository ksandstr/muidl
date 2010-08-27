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
#include <glib.h>
#include <libIDL/IDL.h>
#include <llvm-c/Core.h>

#include "muidl.h"
#include "llvmutil.h"
#include "l4x2.h"


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


/* collect distinct exception repo IDs and resolve them into IDL_EXCEPT_DCL
 * nodes. inserts the result into seen_hash as dup(repo_id) -> except_dcl.
 * returns the number of new exceptions seen.
 */
int collect_exceptions(IDL_ns ns, GHashTable *ex_repo_ids, IDL_tree iface)
{
	int ret = 0;
	GList *methods = all_methods_of_iface(ns, iface);
	GLIST_FOREACH(cur, methods) {
		IDL_tree op = cur->data;
		for(IDL_tree r_cur = IDL_OP_DCL(op).raises_expr;
			r_cur != NULL;
			r_cur = IDL_LIST(r_cur).next)
		{
			IDL_tree exn_id = IDL_LIST(r_cur).data;
			const char *rid = IDL_IDENT(exn_id).repo_id;
			if(g_hash_table_lookup(ex_repo_ids, rid) == NULL) {
				/* exn_id actually refers to the IDL_EXCEPT_DCL's ident node.
				 * the actual exception is an immediate parent.
				 */
				IDL_tree exn = IDL_get_parent_node(exn_id, IDLN_EXCEPT_DCL,
					NULL);
				assert(exn != NULL);
				assert(strcmp(rid, IDL_IDENT(IDL_EXCEPT_DCL(exn).ident).repo_id) == 0);
				g_hash_table_insert(ex_repo_ids, g_strdup(rid), exn);
				ret++;
			}
		}
	}
	g_list_free(methods);

	return ret;
}


static LLVMTypeRef exn_raise_fn_type(struct llvm_ctx *ctx, IDL_tree exn)
{
	GPtrArray *types = g_ptr_array_new();
	IDL_LIST_FOREACH(m_cur, IDL_EXCEPT_DCL(exn).members) {
		IDL_tree m = IDL_LIST(m_cur).data,
			mtype = get_type_spec(IDL_MEMBER(m).type_spec);
		IDL_LIST_FOREACH(d_cur, IDL_MEMBER(m).dcls) {
			IDL_tree dcl = IDL_LIST(d_cur).data;
			T typ;
			if(IDL_NODE_TYPE(dcl) == IDLN_TYPE_ARRAY
				&& is_value_type(mtype))
			{
				typ = LLVMPointerType(llvm_value_type(ctx, mtype), 0);
			} else if(is_rigid_type(ctx->ns, mtype)) {
				/* rigid types are passed by pointer; an array of those is
				 * just a pointer to more than one.
				 */
				typ = LLVMPointerType(llvm_rigid_type(ctx, mtype), 0);
			} else {
				assert(IDL_NODE_TYPE(dcl) == IDLN_IDENT);
				typ = llvm_value_type(ctx, mtype);
			}
			g_ptr_array_add(types, typ);
		}
	}

	T ret = LLVMFunctionType(LLVMVoidTypeInContext(ctx->ctx),
		(LLVMTypeRef *)types->pdata, types->len, 0);
	g_ptr_array_free(types, TRUE);
	return ret;
}


static LLVMValueRef build_exn_raise_fn(
	struct llvm_ctx *ctx,
	const char *fn_name,
	IDL_tree exn)
{
	T fntype = exn_raise_fn_type(ctx, exn);
	V fn = LLVMAddFunction(ctx->module, fn_name, fntype);
	begin_function(ctx, fn);

	BB start_bb = add_sibling_block(ctx, "start"),
		exit_bb = add_sibling_block(ctx, "exit");
	LLVMPositionBuilderAtEnd(ctx->builder, start_bb);
	/* TODO: ... */
	LLVMBuildBr(ctx->builder, exit_bb);

	LLVMPositionBuilderAtEnd(ctx->builder, exit_bb);
	end_function(ctx, start_bb);
	LLVMBuildRetVoid(ctx->builder);

	return fn;
}


static void build_exn_raise_fns_for_iface(
	struct llvm_ctx *ctx,
	GHashTable *seen_hash,
	IDL_tree iface)
{
	GHashTable *exn_hash = g_hash_table_new(&g_str_hash, &g_str_equal);
	collect_exceptions(ctx->ns, exn_hash, iface);
	GHashTableIter iter;
	g_hash_table_iter_init(&iter, exn_hash);
	gpointer key = NULL, value = NULL;
	while(g_hash_table_iter_next(&iter, &key, &value)) {
		IDL_tree exn = value;
		if(g_hash_table_lookup(seen_hash, key) != NULL
			|| is_negs_exn(exn))
		{
			/* already emitted for this module, or one of those exceptions that
			 * doesn't get a raise function
			 */
			g_free(key);
			continue;
		}
		g_hash_table_insert(seen_hash, key, value);

		char *name = exn_raise_fn_name(exn);
		V fn = build_exn_raise_fn(ctx, name, exn);
		g_free(name);

		/* set collapsible linkage. */
		LLVMSetLinkage(fn, LLVMLinkOnceAnyLinkage);
	}
}


gboolean iter_build_exception_raise_fns(
	IDL_tree_func_data *tf,
	void *ud)
{
	struct llvm_ctx *ctx = ud;
	bool made = false;
	if(ctx->seen_exn_hash == NULL) {
		ctx->seen_exn_hash = g_hash_table_new_full(&g_str_hash, &g_str_equal,
			&g_free, NULL);
		made = true;
	}

	gboolean rv;
	switch(IDL_NODE_TYPE(tf->tree)) {
		case IDLN_LIST:
		case IDLN_MODULE:
		case IDLN_SRCFILE:
			rv = TRUE;
			break;

		default: rv = FALSE; break;

		case IDLN_INTERFACE:
			build_exn_raise_fns_for_iface(ctx, ctx->seen_exn_hash, tf->tree);
			rv = FALSE;
			break;
	}
	if(made) {
		g_hash_table_destroy(ctx->seen_exn_hash);
		ctx->seen_exn_hash = NULL;
	}
	return rv;
}
