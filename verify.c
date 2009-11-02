/*
 * verify.c -- verification of IDL tree inputs
 * Copyright 2009  Kalle A. Sandström <ksandstr@iki.fi>
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

#define _GNU_SOURCE

#include <glib.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>
#include <libIDL/IDL.h>

#include "muidl.h"


struct ver_ctx
{
	IDL_ns ns;
	IDL_tree tree;
	bool failed;
};


static void fail(struct ver_ctx *v, const char *fmt, ...)
	__attribute__((format(printf, 2, 3)));


static void failv(struct ver_ctx *v, const char *fmt, va_list al)
{
	char *tmp = g_strdup_vprintf(fmt, al);
	fprintf(stderr, "%s\n", tmp);
	g_free(tmp);
	v->failed = true;
}


static void fail(struct ver_ctx *v, const char *fmt, ...)
{
	va_list al;
	va_start(al, fmt);
	failv(v, fmt, al);
	va_end(al);
}


static gboolean no_reserved_idents(IDL_tree_func_data *tf, gpointer udptr)
{
	struct ver_ctx *v = udptr;

	/* a huge green fierce snake bars your way! */
	switch(IDL_NODE_TYPE(tf->tree)) {

		case IDLN_IDENT: break;

		default: return TRUE;	/* leculsion prease. */
	}

	if(is_reserved_word(IDL_IDENT(tf->tree).str)) {
		IDL_tree up = IDL_NODE_UP(tf->tree);
		switch(IDL_NODE_TYPE(up)) {
			case IDLN_TYPE_STRUCT:
			case IDLN_TYPE_UNION:
			case IDLN_TYPE_ENUM:
			case IDLN_MODULE:
			case IDLN_INTERFACE:
				/* reserved words in these structures' names are handled and
				 * prefixed by long_name().
				 */
				break;

			default:
				fail(v, "`%s' (repoid `%s') is a reserved word in C",
					IDL_IDENT(tf->tree).str, IDL_IDENT_REPO_ID(tf->tree));
		}
	}

	/* you may pass. */
	return FALSE;
}


/* true when everything's OK */
bool verify_idl_input(IDL_ns ns, IDL_tree tree)
{
	struct ver_ctx v = { .ns = ns, .tree = tree, .failed = false };

	/* can't have identifiers which, when changed to lower case, are reserved
	 * words in C.
	 */
	IDL_tree_walk_in_order(tree, &no_reserved_idents, &v);
	if(v.failed) return false;

	/* succeed */
	return true;
}
