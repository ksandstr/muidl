/*
 * verify.c -- verification of IDL tree inputs
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
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>
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
	int len = strlen(tmp);
	while(isblank(tmp[len - 1])) {
		tmp[len - 1] = '\0';
		len--;
	}
	assert(strlen(tmp) == len);
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


static gboolean supported_types_only(IDL_tree_func_data *tf, gpointer udptr)
{
	struct ver_ctx *v = udptr;

	IDL_tree node = tf->tree;
	while(node != NULL && IDL_NODE_TYPE(node) == IDLN_IDENT) {
		node = IDL_get_parent_node(node, IDLN_ANY, NULL);
	}

	bool in_const = IDL_get_parent_node(node, IDLN_CONST_DCL, NULL) != NULL;
	switch(IDL_NODE_TYPE(node)) {
		case IDLN_INTERFACE: {
			/* what do badgers eat?
			 * where cannot interface types appear?
			 */
			static const IDL_tree_type nogo[] = {
				IDLN_PARAM_DCL,		/* not in parameters. */
				IDLN_TYPE_STRUCT,	/* and not in structs. */
				IDLN_OP_DCL,		/* not in return types... */
				IDLN_TYPE_SEQUENCE,	/* or sequences */
				IDLN_TYPE_ARRAY,	/* let alone arrays */
				IDLN_TYPE_DCL,		/* or even typedefs. */
				/* so sad. */
			};
			if(tf->up == NULL) break;
			for(int i=0; i<G_N_ELEMENTS(nogo); i++) {
				IDL_tree parent = IDL_get_parent_node(tf->up->tree, nogo[i], NULL);
				if(parent != NULL) {
					fail(v, "interface types are not supported inside <%s>",
						IDL_NODE_TYPE_NAME(parent));
					return FALSE;
				}
			}
			break;
		}

		case IDLN_TYPE_OBJECT:
		case IDLN_TYPE_ANY:
		case IDLN_TYPE_TYPECODE:
		case IDLN_TYPE_FLOAT:
		case IDLN_TYPE_UNION:
			fail(v, "type <%s> not supported", IDL_NODE_TYPE_NAME(node));
			return FALSE;

		case IDLN_TYPE_SEQUENCE:
			if(IDL_TYPE_SEQUENCE(node).positive_int_const == NULL
				&& !in_const)
			{
				fail(v, "sequences must be bounded");
			}
			break;

		case IDLN_TYPE_STRING:
			if(IDL_TYPE_STRING(node).positive_int_const == NULL
				&& !in_const)
			{
				fail(v, "string types must be bounded");
			}
			return FALSE;	/* no point */

		case IDLN_TYPE_WIDE_STRING:
			if(IDL_TYPE_WIDE_STRING(node).positive_int_const == NULL
				&& !in_const)
			{
				fail(v, "wide strings must be bounded");
			}
			return FALSE;

		case IDLN_TYPE_ARRAY:
			if(IDL_list_length(IDL_TYPE_ARRAY(node).size_list) > 1) {
				fail(v, "arrays must have a single dimension");
			}
			break;

		default:
			break;
	}

	/* everything else, and sequence & array component types */
	return TRUE;
}


static gboolean no_recv_timeout_in_oneway(
	IDL_tree_func_data *tf,
	gpointer udptr)
{
	struct ver_ctx *v = udptr;

	switch(IDL_NODE_TYPE(tf->tree)) {
		case IDLN_MODULE:
		case IDLN_INTERFACE:
			return TRUE;

		case IDLN_OP_DCL:
			if(IDL_OP_DCL(tf->tree).f_oneway
				&& (op_timeout_kind(tf->tree) & TIMEOUT_RECV))
			{
				fail(v, "`%s': an oneway operation can't have a receive timeout",
					IDL_IDENT(IDL_OP_DCL(tf->tree).ident).repo_id);
			}
			return FALSE;

		default:
			return FALSE;
	}
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

	/* fail on invalid types (unbounded sequences, unbounded strings,
	 * multidimensional arrays, unions, any types, and references)
	 */
	IDL_tree_walk_in_order(tree, &supported_types_only, &v);
	if(v.failed) return false;

	/* fail when an oneway opdcl has the StubRecvTimeout or StubTimeouts
	 * attribute.
	 */
	IDL_tree_walk_in_order(tree, &no_recv_timeout_in_oneway, &v);
	if(v.failed) return false;

	/* succeed */
	return true;
}
