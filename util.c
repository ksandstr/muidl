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
#include <ctype.h>
#include <llvm-c/Core.h>

#include "muidl.h"


static GHashTable *warn_once_hash = NULL;


void list_dispose(GList *list)
{
	g_list_foreach(list, (GFunc)&g_free, NULL);
	g_list_free(list);
}


void free_message_info(struct message_info *inf)
{
	if(inf == NULL) return;

	list_dispose(inf->params);
	g_list_free(inf->untyped);
	g_list_free(inf->seq);
	g_list_free(inf->_long);
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


const char *seq_len_lvalue(
	struct print_ctx *pr,
	IDL_tree param,
	const char *var_prefix,
	const char *name,
	bool for_dispatcher)
{
	enum IDL_param_attr attr = IDL_PARAM_DCL(param).attr;
	bool ptr = attr != IDL_PARAM_IN && !for_dispatcher;
	return tmp_f(pr, "%s%s%s_len%s", ptr ? "*" : "", var_prefix, name,
		ptr ? "_ptr" : "");
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
