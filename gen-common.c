/*
 * gen-common.c -- generic common header file generation for the L4 C API
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

#include <glib.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <libIDL/IDL.h>

#include "muidl.h"


static void print_out_param(
	FILE *of,
	IDL_ns ns,
	IDL_tree type,
	const char *name)
{
	char *b = NULL;
	if(IDL_NODE_TYPE(type) == IDLN_TYPE_SEQUENCE) {
		IDL_tree elemtyp = get_type_spec(
			IDL_TYPE_SEQUENCE(type).simple_type_spec);
		b = in_param_type(ns, elemtyp);
		fprintf(of, "%s **%s_ptr, unsigned *%s_len", b, name, name);
	} else if(is_value_type(type)) {
		b = value_type(ns, type);
		fprintf(of, "%s *%s_ptr", b, name);
	} else if(IS_MAPGRANT_TYPE(type)) {
		fprintf(of, "L4_MapGrantItem_t *%s_ptr", name);
	} else {
		b = rigid_type(ns, type);
		fprintf(of, "%s *%s_ptr", b, name);
	}
	g_free(b);
}


static void print_vtable(
	FILE *of,
	IDL_tree file_tree,
	IDL_ns ns,
	IDL_tree iface)
{
	char *vtpfx = vtable_prefix(ns, iface);
	fprintf(of, "struct %s_vtable\n{\n", vtpfx);
	g_free(vtpfx);

	GList *methods = all_methods_of_iface(ns, iface);
	for(GList *cur = g_list_first(methods);
		cur != NULL;
		cur = g_list_next(cur))
	{
		IDL_tree op = cur->data;

		char *rettypstr = return_type(ns, op, NULL),
			*name = decapsify(METHOD_NAME(op));
		fprintf(of, "\t%s%s(*%s%s)(", rettypstr, type_space(rettypstr),
			is_reserved_word(name) ? "_" : "", name);
		g_free(rettypstr);
		g_free(name);

		bool first_param = true;
		IDL_tree rettyp = get_type_spec(IDL_OP_DCL(op).op_type_spec);
		if(rettyp != NULL && !is_value_type(rettyp)) {
			/* compound type return values are declared as the first parameter,
			 * because they're on the "left side".
			 */
			first_param = false;
			print_out_param(of, ns, rettyp, "_result");
		}

		IDL_tree params = IDL_OP_DCL(op).parameter_dcls;
		if(first_param && params == NULL) fprintf(of, "void");
		for(IDL_tree iter = params;
			iter != NULL;
			iter = IDL_LIST(iter).next)
		{
			if(first_param) first_param = false; else fprintf(of, ", ");
			IDL_tree p = IDL_LIST(iter).data,
				decl = IDL_PARAM_DCL(p).simple_declarator,
				type = get_type_spec(IDL_PARAM_DCL(p).param_type_spec);
			const char *name = IDL_IDENT(decl).str;
			switch(IDL_PARAM_DCL(p).attr) {
				case IDL_PARAM_IN: {
					if(IDL_NODE_TYPE(p) == IDLN_TYPE_SEQUENCE) {
						NOTDEFINED(p);
					}
					char *typestr = in_param_type(ns, type);
					fprintf(of, "%s%s%s%s", typestr, type_space(typestr),
						is_reserved_word(name) ? "_" : "", name);
					g_free(typestr);
					break;
				}

				case IDL_PARAM_OUT:
					print_out_param(of, ns, type, name);
					break;

				case IDL_PARAM_INOUT:
					fprintf(of, "in-out parameters not done\n");
					break;
			}
		}

		fprintf(of, "),\n");
	}
	fprintf(of, "};\n");
	g_list_free(methods);
}


static gboolean print_struct_decls(IDL_tree_func_data *tf, gpointer userdata)
{
	struct print_ctx *print = userdata;
	FILE *of = print->of;

	switch(IDL_NODE_TYPE(tf->tree)) {
		default: return FALSE;

		case IDLN_LIST:
		case IDLN_MODULE:
		case IDLN_SRCFILE:
		case IDLN_INTERFACE:
			return TRUE;

		case IDLN_TYPE_STRUCT:
			break;
	}

	char *l = long_name(print->ns, tf->tree);
	fprintf(of, "struct %s", l);
	g_free(l);

	IDL_tree prop_node = IDL_TYPE_STRUCT(tf->tree).ident;
	const bool packed = IDL_tree_property_get(prop_node, "NoPacked") == NULL;
	fprintf(of, "\n{\n");
	for(IDL_tree cur = IDL_TYPE_STRUCT(tf->tree).member_list;
		cur != NULL;
		cur = IDL_LIST(cur).next)
	{
		IDL_tree member = IDL_LIST(cur).data,
			type = get_type_spec(IDL_MEMBER(member).type_spec);

		char *typestr = NULL, *suffix = NULL;
		unsigned long max_size = 0;
		if(is_value_type(type)) {
			typestr = value_type(print->ns, type);
		} else if(is_rigid_type(print->ns, type)) {
			typestr = rigid_type(print->ns, type);

			/* and an array bound suffix. */
			IDL_tree bound;
			unsigned long extra;
			switch(IDL_NODE_TYPE(type)) {
				case IDLN_TYPE_STRING:
					bound = IDL_TYPE_STRING(type).positive_int_const;
					extra = 1;	/* '\0' terminator */
					break;

				case IDLN_TYPE_WIDE_STRING:
					bound = IDL_TYPE_WIDE_STRING(type).positive_int_const;
					extra = 1;
					break;

				case IDLN_TYPE_SEQUENCE:
					bound = IDL_TYPE_SEQUENCE(type).positive_int_const;
					extra = 0;
					break;

				default:
					NOTDEFINED(type);
			}
			assert(bound != NULL);
			max_size = (unsigned long)IDL_INTEGER(bound).value;
			suffix = g_strdup_printf("[%lu]", max_size + extra);
		} else {
			/* only constant maximum length types are permitted in
			 * µidl structs.
			 */
			NOTDEFINED(type);
		}

		for(IDL_tree dcl_node = IDL_MEMBER(member).dcls;
			dcl_node != NULL;
			dcl_node = IDL_LIST(dcl_node).next)
		{
			IDL_tree data = IDL_LIST(dcl_node).data;
			if(IDL_NODE_TYPE(data) == IDLN_IDENT) {
				const char *name = IDL_IDENT(data).str;
				if(is_value_type(type)) {
					fprintf(of, "\t%s %s;\n", typestr, name);
				} else if(is_rigid_type(print->ns, type)) {
					if(IDL_NODE_TYPE(type) == IDLN_TYPE_SEQUENCE) {
						const char *t;
						if(max_size <= 255) t = "uint8_t";
					else if(max_size <= 65535) t = "uint16_t";
						else t = "unsigned";
						fprintf(of, "\t%s %s_len;\n", t, name);
					}
				fprintf(of, "\t%s %s%s%s;\n", typestr, name,
					is_reserved_word(name) ? "_" : "",
					suffix != NULL ? suffix : "");
				} else {
					/* TODO */
					NOTDEFINED(type);
				}
			} else if(IDL_NODE_TYPE(data) == IDLN_TYPE_ARRAY) {
				IDL_tree ident = IDL_TYPE_ARRAY(data).ident,
					size_list = IDL_TYPE_ARRAY(data).size_list;
				const char *name = IDL_IDENT(ident).str;
				if(IDL_LIST(size_list).next != NULL) {
					NOTSUPPORTED("multi-dimensional arrays");
				}
				/* we'll disallow sequences, strings and wide strings. */
				int nt = IDL_NODE_TYPE(type);
				if(nt == IDLN_TYPE_SEQUENCE || nt == IDLN_TYPE_STRING
					|| nt == IDLN_TYPE_WIDE_STRING)
				{
					NOTSUPPORTED("arrays of sequences, strings or wide strings");
				}
				fprintf(of, "\t%s %s%s[%ld];\n", typestr, name,
					is_reserved_word(name) ? "_" : "",
					(long)IDL_INTEGER(IDL_LIST(size_list).data).value);
			} else {
				NOTDEFINED(data);
			}
		}

		g_free(typestr);
		g_free(suffix);
	}
	fprintf(of, "}%s;\n", packed ? " __attribute__((__packed__))" : "");

	return FALSE;	/* let's not go into structures. they're silly. */
}


void print_common_header(struct print_ctx *pr)
{
	code_f(pr,
		"/* THIS FILE WAS GENERATED WITH µidl!\n"
		" *\n"
		" * Do not modify it, modify the source IDL file `%s' instead.\n"
		" */\n", pr->idlfilename);

	/* include guard */
	char *upper = g_utf8_strup(pr->common_header_name, -1);
	/* (assume it's valid utf8 after strup.) */
	for(char *p = upper; *p != '\0'; p = g_utf8_next_char(p)) {
		if(g_ascii_ispunct(*p)) *p = '_';
	}
	code_f(pr, "#ifndef _MUIDL_%s\n#define _MUIDL_%s\n", upper, upper);

	/* struct, union & enum declarations as they appear in the IDL source. this
	 * is appropriate because IDL doesn't permit forward declaration of
	 * structs, unions, enums, or typedefs.
	 */
	IDL_tree_walk_in_order(pr->tree, &print_struct_decls, pr);
	code_f(pr, " ");

	/* interface vtables, but only for service implementations (so as to avoid
	 * polluting the namespace).
	 */
	if(g_hash_table_size(pr->ifaces) > 0) {
		code_f(pr, "#ifdef MUIDL_IMPL_SOURCE");
		GHashTableIter iter;
		g_hash_table_iter_init(&iter, pr->ifaces);
		gpointer key, value;
		while(g_hash_table_iter_next(&iter, &key, &value)) {
			code_f(pr, "/* vtable for `%s': */", (const char *)key);
			print_vtable(pr->of, pr->tree, pr->ns, (IDL_tree)value);
		}
		code_f(pr, "#endif\n");
	}

	code_f(pr, "#endif");
	g_free(upper);
}