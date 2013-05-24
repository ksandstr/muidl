/*
 * util.c -- utilities for µIDL
 * Copyright 2009, 2010  Kalle A. Sandström <ksandstr@iki.fi>
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

#include <glib.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <pthread.h>
#include <ctype.h>
#include <llvm-c/Core.h>

#include "defs.h"
#include "llvmutil.h"


static GHashTable *warn_once_hash = NULL;
static pthread_key_t llvm_ctx_key;
static bool keys_done = false;
static pthread_once_t key_once = PTHREAD_ONCE_INIT;


void list_dispose(GList *list)
{
	g_list_foreach(list, (GFunc)&g_free, NULL);
	g_list_free(list);
}


void free_message_info(struct message_info *inf)
{
	if(inf == NULL) return;

	g_list_free(inf->params);
	list_dispose(inf->untyped);
	list_dispose(inf->seq);
	list_dispose(inf->string);
	list_dispose(inf->mapped);
	g_free(inf);
}


void free_method_info(struct method_info *inf)
{
	if(inf == NULL) return;
	free_message_info(inf->request);
	for(int i=0; i<inf->num_reply_msgs; i++) {
		free_message_info(inf->replies[i]);
	}
	g_free(inf);
}


void free_iface_info(struct iface_info *inf)
{
	if(inf == NULL) return;

	GLIST_FOREACH(cur, inf->ops) {
		free_method_info(cur->data);
	}
	g_list_free(inf->ops);
	g_list_free(inf->tagmask_ops);
	g_free(inf);
}


bool warn_once(const char *fmt, ...)
{
	if(warn_once_hash == NULL) {
		warn_once_hash = g_hash_table_new_full(&g_str_hash,
			&g_str_equal, &g_free, NULL);
	}

	va_list al;
	va_start(al, fmt);
	char *str = g_strdup_vprintf(fmt, al);
	va_end(al);
	int len = strlen(str);
	while(len > 0 && str[len - 1] == '\n') len--;
	str[MAX(0, len - 1)] = '\0';
	if(g_hash_table_lookup(warn_once_hash, str) == NULL) {
		g_hash_table_insert(warn_once_hash, str, GINT_TO_POINTER(1));
		fprintf(stderr, "warning: %s\n", str);
		return true;
	} else {
		g_free(str);
		return false;
	}
}


void reset_warn_once(void)
{
	if(warn_once_hash != NULL) {
		g_hash_table_remove_all(warn_once_hash);
	}
}


struct msg_param *find_pdecl(GList *list, IDL_tree pdecl)
{
	GLIST_FOREACH(cur, list) {
		struct msg_param *p = cur->data;
		if(p->param_dcl == pdecl) return p;
	}
	return NULL;
}


char *tmp_vf(struct print_ctx *pr, const char *fmt, va_list al)
{
	/* leave the heap out of this, if reasonably possible */
	char buf[256];
	va_list copy;
	va_copy(copy, al);
	int n = vsnprintf(buf, sizeof(buf), fmt, copy);
	va_end(copy);
	char *result;
	if(n+1 > sizeof(buf)) {
		/* ok, have to involve the heap. */
#ifndef NDEBUG
		fprintf(stderr, "%s: output would be %d bytes long\n",
			__FUNCTION__, n);
#endif
		char *feh = g_strdup_vprintf(fmt, al);
		result = g_string_chunk_insert_len(pr->tmpstrchunk, feh, n);
		g_free(feh);
	} else {
		result = g_string_chunk_insert_len(pr->tmpstrchunk, buf, n);
	}
	return result;
}


char *tmp_f(struct print_ctx *pr, const char *fmt, ...)
{
	va_list al;
	va_start(al, fmt);
	char *result = tmp_vf(pr, fmt, al);
	va_end(al);
	return result;
}


void print_headers(struct print_ctx *pr, const char * const *strs, int len)
{
	for(int i=0; i<len; i++) {
		code_f(pr, "#include <%s>", strs[i]);
	}
	code_f(pr, "\n");
}


void print_file_heading(struct print_ctx *pr)
{
	code_f(pr,
		"/* THIS FILE WAS GENERATED WITH µidl!\n"
		" *\n"
		" * Do not modify it, modify the source IDL file `%s' instead.\n"
		" */\n", pr->idlfilename);
}


/* TODO: callsites to this here thing should recycle compatible chunks of
 * memory, rather than allocating their own for each. this is a low-hanging
 * space optimization.
 */
LLVMValueRef build_local_storage(
	struct llvm_ctx *ctx,
	LLVMTypeRef type,
	LLVMValueRef count,
	const char *name)
{
	assert(ctx->alloc_bb != NULL);
	LLVMBuilderRef b = LLVMCreateBuilderInContext(ctx->ctx);
	LLVMPositionBuilderAtEnd(b, ctx->alloc_bb);
	LLVMValueRef ptr;
	if(count == NULL) ptr = LLVMBuildAlloca(b, type, name);
	else ptr = LLVMBuildArrayAlloca(b, type, count, name);
	LLVMDisposeBuilder(b);
	return ptr;
}


/* free(2)'d at end of function. */
LLVMValueRef build_malloc_storage(
	struct llvm_ctx *ctx,
	LLVMTypeRef type,
	LLVMValueRef count,
	const char *name)
{
	assert(ctx->alloc_bb != NULL);
	LLVMBuilderRef b = LLVMCreateBuilderInContext(ctx->ctx);
	LLVMPositionBuilderAtEnd(b, ctx->alloc_bb);
	LLVMValueRef ptr;
	if(count == NULL) ptr = LLVMBuildMalloc(b, type, name);
	else ptr = LLVMBuildArrayMalloc(b, type, count, name);
	LLVMDisposeBuilder(b);
	ctx->malloc_ptrs = g_list_prepend(ctx->malloc_ptrs, ptr);
	return ptr;
}


LLVMValueRef build_seq_param_storage(
	struct llvm_ctx *ctx,
	IDL_tree ptyp,
	const char *name)
{
	IDL_tree subtype = SEQ_SUBTYPE(ptyp);
	uint64_t max_size = SEQ_BOUND_VAL(ptyp);

	/* use stack allocation only for buffers smaller than 256 bytes */
	uint64_t max_bytes = max_size * (size_in_bits(subtype) / 8);
	T typ = llvm_rigid_type(ctx, subtype);
	V sz = CONST_INT(max_size);
	if(max_bytes >= 256) {
		return build_malloc_storage(ctx, typ, sz,
			tmp_f(ctx->pr, "%s.heap", name));
	} else {
		return build_local_storage(ctx, typ, sz,
			tmp_f(ctx->pr, "%s.stk", name));
	}
}


char *mangle_repo_id(const char *repo_id)
{
	int len = strlen(repo_id);
	GString *str = g_string_sized_new(len + 16);
	bool lower = false;
	for(int i=0; i<len; i++) {
		char c = repo_id[i];
		assert(c != '\0');
		if(!isalnum(c) && c != '_') {
			c = '_';
			lower = false;
		} else if(isupper(c)) {
			if(lower) g_string_append_c(str, '_');
			c = tolower(c);
			lower = false;
		} else {
			lower = true;
		}
		g_string_append_c(str, c);
	}
	return g_string_free(str, FALSE);
}


struct member_item *expand_member_list(IDL_tree list)
{
	GArray *items = g_array_new(FALSE, FALSE, sizeof(struct member_item));
	IDL_LIST_FOREACH(m_cur, list) {
		IDL_tree member = IDL_LIST(m_cur).data,
			mtype = get_type_spec(IDL_MEMBER(member).type_spec);
		IDL_LIST_FOREACH(d_cur, IDL_MEMBER(member).dcls) {
			IDL_tree dcl = IDL_LIST(d_cur).data;
			struct member_item mi = { .type = mtype };
			IDL_tree id;
			if(IDL_NODE_TYPE(dcl) == IDLN_IDENT) {
				mi.dim = 0;
				id = dcl;
			} else if(IDL_NODE_TYPE(dcl) == IDLN_TYPE_ARRAY) {
				mi.dim = IDL_INTEGER(IDL_LIST(
					IDL_TYPE_ARRAY(dcl).size_list).data).value;
				id = IDL_TYPE_ARRAY(dcl).ident;
			} else {
				g_assert_not_reached();
			}
			mi.name = IDL_IDENT(id).str;
			g_array_append_val(items, mi);
		}
	}
	struct member_item term = { .type = NULL };
	g_array_append_val(items, term);
	return (struct member_item *)g_array_free(items, FALSE);
}


uint32_t djb2_hash(const char *key)
{
	/* djb2 (k=33).
	 * from http://www.cse.yorku.ca/~oz/hash.html
	 */
	uint32_t hash = 5381;
	const uint8_t *s = (const uint8_t *)key;
	int len = strlen(key);
	for(int i=0; i<len; i++) {
		hash = (hash << 5) + hash + s[i];
	}
	return hash;
}


static void make_keys(void)
{
	pthread_key_create(&llvm_ctx_key, NULL);
	keys_done = true;
}


struct llvm_ctx *get_llvm_ctx(void) {
	return keys_done ? pthread_getspecific(llvm_ctx_key) : NULL;
}


struct llvm_ctx *replace_llvm_ctx(struct llvm_ctx *ctx)
{
	pthread_once(&key_once, &make_keys);
	struct llvm_ctx *old = pthread_getspecific(llvm_ctx_key);
	pthread_setspecific(llvm_ctx_key, ctx);
	return old;
}
