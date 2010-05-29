/*
 * util.c -- utilities for µidl
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
#include <string.h>

#include "muidl.h"


void list_dispose(GList *list)
{
	g_list_foreach(list, (GFunc)&g_free, NULL);
	g_list_free(list);
}


void free_message_info(struct message_info *inf)
{
	if(inf == NULL) return;
	for(int i=0; i<inf->num_untyped; i++) {
		g_free(inf->untyped[i]);
	}
	g_free(inf->untyped);

	for(int i=0; i<inf->num_inline_seq; i++) g_free(inf->seq[i]);
	g_free(inf->seq);

	for(int i=0; i<inf->num_long; i++) g_free(inf->long_params[i]);
	/* long_params is allocated at end-of-struct so no freeing for that. */

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
