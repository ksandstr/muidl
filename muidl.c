/*
 * muidl.c -- IDL compiler for µiX
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <glib.h>
#include <libIDL/IDL.h>


static int msg_callback(
	int level,
	int num,
	int line,
	const char *filename,
	const char *message)
{
	fprintf(stderr, "%s:%d: %s\n", filename, line, message);
	return 0;
}


static gboolean tree_func(
	IDL_tree_func_data *tf,
	gpointer userdata)
{
	IDL_ns ns = userdata;
	IDL_tree t = tf->tree;
	switch(IDL_NODE_TYPE(t)) {
		case IDLN_INTERFACE: {
			IDL_tree ident = t->u.idl_interface.ident;
			printf("interface: `%s'\n", ident->u.idl_ident.str);
			for(IDL_tree inh = IDL_INTERFACE(t).inheritance_spec;
				inh != NULL;
				inh = IDL_LIST(inh).next)
			{
				IDL_tree data = IDL_LIST(inh).data;
				printf("  inherits `%s'\n", IDL_IDENT(data).str);
				gboolean conflict = FALSE;
				IDL_tree real = IDL_ns_lookup_cur_scope(ns, data, &conflict);
				if(real != NULL) {
					printf("  resolved has type %d%s\n", IDL_NODE_TYPE(real),
						conflict ? " (conflict)" : " (no conflict)");
				}
			}

			return TRUE;
		}

		case IDLN_OP_DCL: {
			IDL_tree ident = IDL_OP_DCL(t).ident;
			printf("operation: `%s' %s\n", IDL_IDENT(ident).str,
				IDL_OP_DCL(t).f_oneway ? "(oneway)" : "(two-way)");
			const char *tagmask = IDL_tree_property_get(ident, "TagMask"),
				*label = IDL_tree_property_get(ident, "Label");
			printf("  tagmask `%s', label `%s'\n", tagmask, label);
			return FALSE;
		}

		case IDLN_TYPE_STRUCT: {
			IDL_tree ident = t->u.idl_type_struct.ident;
			printf("struct: `%s'\n", ident->u.idl_ident.str);
			return FALSE;
		}

		default:
			return TRUE;
	}
}


bool parse_idl_file(const char *filename)
{
	IDL_tree tree = NULL;
	IDL_ns ns = NULL;
	int n = IDL_parse_filename(filename, "", &msg_callback,
		&tree, &ns, IDLF_PROPERTIES | IDLF_XPIDL, IDL_WARNING1);
	if(n == IDL_ERROR) {
		fprintf(stderr, "IDL_parse_filename() failed.\n");
		return false;
	} else if(n < 0) {
		perror(filename);
		return false;
	}

	printf("IDL_parse_filename() returned %d (tree %p, ns %p)\n",
		n, tree, ns);
	IDL_tree_walk_in_order(tree, &tree_func, ns);

	IDL_ns_free(ns);
	IDL_tree_free(tree);

	return true;
}


int main(int argc, char *argv[])
{
	if(argc < 2) {
		fprintf(stderr, "Usage: %s [IDLFILE]\n", argv[0]);
		return EXIT_FAILURE;
	}

	IDL_check_cast_enable(TRUE);
	bool ok = true;
	for(int i=1; i<argc; i++) {
		ok = ok && parse_idl_file(argv[i]);
	}
	return ok ? EXIT_SUCCESS : EXIT_FAILURE;
}
