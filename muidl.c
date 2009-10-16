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
#include <assert.h>
#include <ctype.h>
#include <glib.h>
#include <libIDL/IDL.h>


#define IFACE_REPO_ID(t) IDL_IDENT_REPO_ID(IDL_INTERFACE((t)).ident)
#define IFACE_NAME(t) (IDL_IDENT(IDL_INTERFACE((t)).ident).str)
#define METHOD_NAME(t) (IDL_IDENT(IDL_OP_DCL((t)).ident).str)
#define NATIVE_NAME(t) (IDL_IDENT(IDL_NATIVE((t)).ident).str)

#define IS_MAPGRANT_TYPE(t) (IDL_NODE_TYPE((t)) == IDLN_NATIVE \
	&& strcmp(NATIVE_NAME((t)), "l4_mapgrantitem_t") == 0)
#define IS_WORD_TYPE(t) (IDL_NODE_TYPE((t)) == IDLN_NATIVE \
	&& strcmp(NATIVE_NAME((t)), "l4_word_t") == 0)

#define NODETYPESTR(n) ({ \
		IDL_tree __n = (n); \
		__n == NULL ? "(nil)" : IDL_tree_type_names[IDL_NODE_TYPE(__n)]; \
	})



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


static bool collect_methods(
	GList **methods_p,
	GHashTable *ifaces_seen,
	IDL_ns ns,
	IDL_tree iface)
{
	/* depth-first recursion so that operation overlap errors etc. are caught
	 * as close to the inheritance graph's root (i.e. the interfaces we're
	 * going to use) as possible.
	 *
	 * to contrast: interface inheritance is idempotent, so multiple
	 * overlapping inheritance of interfaces is A-OK.
	 */
	for(IDL_tree cur = IDL_INTERFACE(iface).inheritance_spec;
		cur != NULL;
		cur = IDL_LIST(cur).next)
	{
		IDL_tree inh_id = IDL_LIST(cur).data;
		char *inh_repoid = IDL_IDENT_REPO_ID(inh_id);
		if(g_hash_table_lookup(ifaces_seen, inh_repoid) == NULL) {
			IDL_tree inh_iface_id = IDL_ns_resolve_this_scope_ident(ns,
				IDL_IDENT_TO_NS(inh_id), inh_id);
			if(inh_iface_id == NULL) {
				fprintf(stderr, "can't find inherited interface `%s'\n",
					inh_repoid);
				return false;
			}

			assert(IDL_NODE_TYPE(inh_iface_id) == IDLN_GENTREE);
			inh_iface_id = IDL_GENTREE(inh_iface_id).data;
			assert(IDL_NODE_TYPE(inh_iface_id) == IDLN_IDENT);
#if 0
			printf("%s: recursing to `%s'\n", __FUNCTION__,
				IDL_IDENT_REPO_ID(inh_iface_id));
#endif
			IDL_tree inh_iface = IDL_NODE_UP(inh_iface_id);
			assert(IDL_NODE_TYPE(inh_iface) == IDLN_INTERFACE);

			g_hash_table_insert(ifaces_seen, inh_repoid, inh_iface);
			if(!collect_methods(methods_p, ifaces_seen, ns, inh_iface)) {
				return false;
			}
		}
	}

	/* then the actual methods. */
	for(IDL_tree cur = IDL_INTERFACE(iface).body;
		cur != NULL;
		cur = IDL_LIST(cur).next)
	{
		IDL_tree op = IDL_LIST(cur).data;
		if(IDL_NODE_TYPE(op) != IDLN_OP_DCL) continue;
		*methods_p = g_list_prepend(*methods_p, op);
	}

	return true;
}


static char *decapsify(const char *name)
{
	const int len = strlen(name);
	int ncaps = 0;
	for(int i=0; i < len; i++) if(isupper(name[i])) ncaps++;
	char *out = g_malloc(sizeof(char) * (len + ncaps + 1));
	int o = 0;
	for(int i=0; i < len; i++) {
		if(isupper(name[i])) {
			if(i > 0) out[o++] = '_';
			out[o++] = tolower(name[i]);
		} else {
			out[o++] = name[i];
		}
	}
	out[o] = '\0';
	return out;
}


static char *iface_prefix(IDL_ns ns, IDL_tree iface)
{
	char *ifname = decapsify(IFACE_NAME(iface));
	return ifname;
}


/* does the operation given have a NegativeReturn exception? */
static bool has_negs_exns(IDL_ns ns, IDL_tree op)
{
	bool has_negs = false;
	for(IDL_tree cur = IDL_OP_DCL(op).raises_expr;
		cur != NULL;
		cur = IDL_LIST(cur).next)
	{
		IDL_tree r = IDL_NODE_UP(IDL_LIST(cur).data);
		/* FIXME: check properties instead */
		const char *exrid = IDL_IDENT_REPO_ID(IDL_EXCEPT_DCL(r).ident);
		bool add = false;
		if(strcmp(exrid, "IDL:Posix/Errno:1.0") == 0) add = true;
		if(add && has_negs) {
			/* FIXME: blow up cleanly */
			fprintf(stderr, "two exceptions specify NegativeReturn for `%s'\n",
				IDL_IDENT(IDL_OP_DCL(op).ident).str);
			exit(EXIT_FAILURE);
		}
		has_negs = has_negs || add;
	}
	return has_negs;
}


/* chase typedefs down to the non-typedef type. */
static IDL_tree get_type_spec(IDL_tree node)
{
	if(node == NULL) return NULL;

	switch(IDL_NODE_TYPE(node)) {
		case IDLN_TYPE_DCL:
			return get_type_spec(IDL_TYPE_DCL(node).type_spec);

		case IDLN_PARAM_DCL:
			return get_type_spec(IDL_PARAM_DCL(node).param_type_spec);

		case IDLN_MEMBER:
			return get_type_spec(IDL_MEMBER(node).type_spec);

		case IDLN_LIST:
		case IDLN_IDENT:
			return get_type_spec(IDL_get_parent_node(node, IDLN_ANY, NULL));

		default:
			return node;
	}
}


/* returns "true" for non-primitive types that shouldn't be passed by
 * value.
 */
static bool is_compound_type(IDL_tree type)
{
	if(type == NULL) return false;	/* void is not a compound type. */
	switch(IDL_NODE_TYPE(type)) {
		/* unsupported things */
		case IDLN_TYPE_ANY:
		case IDLN_TYPE_OBJECT:
		case IDLN_TYPE_TYPECODE:
		case IDLN_TYPE_FIXED:

		/* actual compound types we'll see */
		case IDLN_TYPE_ARRAY:
		case IDLN_TYPE_SEQUENCE:
		case IDLN_TYPE_STRUCT:
		case IDLN_TYPE_UNION:
			return true;

		case IDLN_NATIVE:
			if(IS_WORD_TYPE(type)) return false;
			else if(IS_MAPGRANT_TYPE(type)) return true;
			else {
				/* FIXME: don't exit */
				fprintf(stderr, "%s: unknown native type `%s'\n",
					__FUNCTION__, NATIVE_NAME(type));
				exit(EXIT_FAILURE);
			}

		default:
			return false;
	}
}


static const char *basic_type(IDL_ns ns, IDL_tree op_type)
{
	if(op_type == NULL) return "void";
	else {
		switch(IDL_NODE_TYPE(op_type)) {
			case IDLN_TYPE_INTEGER: {
				static const char *ityps[] = {
					[IDL_INTEGER_TYPE_SHORT] = "uint16_t",
					[IDL_INTEGER_TYPE_LONG] = "uint32_t",
					[IDL_INTEGER_TYPE_LONGLONG] = "uint64_t",
				};
				int t = IDL_TYPE_INTEGER(op_type).f_type;
				assert(t < G_N_ELEMENTS(ityps));
				return ityps[t] +
					(IDL_TYPE_INTEGER(op_type).f_signed ? 1 : 0);
			}

			case IDLN_IDENT: {
				assert(IDL_NODE_TYPE(op_type->up) == IDLN_LIST);
				assert(IDL_NODE_TYPE(op_type->up->up) == IDLN_TYPE_DCL);
				IDL_tree type = get_type_spec(op_type->up->up);
				return basic_type(ns, type);
			}

			case IDLN_NATIVE: {
				if(IS_WORD_TYPE(op_type)) {
					return "L4_Word_t";
				} else if(IS_MAPGRANT_TYPE(op_type)) {
					fprintf(stderr, "%s: caller must handle native l4_mapgrantitem_t\n",
						__FUNCTION__);
					abort();
				} else {
					fprintf(stderr, "%s: native type `%s' not supported\n",
						__FUNCTION__, NATIVE_NAME(op_type));
					exit(EXIT_FAILURE);
				}
				break;
			}

			case IDLN_TYPE_FLOAT:
			case IDLN_TYPE_FIXED:
			case IDLN_TYPE_CHAR:
			case IDLN_TYPE_WIDE_CHAR:
			case IDLN_TYPE_STRING:
			case IDLN_TYPE_WIDE_STRING:
			case IDLN_TYPE_BOOLEAN:
			case IDLN_TYPE_OCTET:
			case IDLN_TYPE_ANY:
			case IDLN_TYPE_OBJECT:
			case IDLN_TYPE_TYPECODE:
			case IDLN_TYPE_ENUM:
			case IDLN_TYPE_SEQUENCE:
			case IDLN_TYPE_ARRAY:
			case IDLN_TYPE_STRUCT:
			case IDLN_TYPE_UNION:

			default:
				if(is_compound_type(op_type)) {
					fprintf(stderr, "%s: <%s> is a compound type\n",
						__FUNCTION__, NODETYPESTR(op_type));
				} else {
					fprintf(stderr, "%s: <%s> not implemented\n",
						__FUNCTION__, NODETYPESTR(op_type));
				}
				exit(EXIT_FAILURE);
		}
	}
}


/* returns the unabbreviated C name of a struct, union, interfacedcl, opdcl (in
 * which case the stub's name is returned), enum, or const.
 *
 * the return value must be g_free()'d.
 */
static char *long_name(IDL_ns ns, IDL_tree node)
{
	/* FIXME: compute a prefix properly */
	const char *prefix = "", *name;
	switch(IDL_NODE_TYPE(node)) {
		case IDLN_TYPE_STRUCT:
			name = IDL_IDENT(IDL_TYPE_STRUCT(node).ident).str;
			break;

		case IDLN_TYPE_UNION:
			name = IDL_IDENT(IDL_TYPE_UNION(node).ident).str;
			break;

		case IDLN_INTERFACE:
			name = IDL_IDENT(IDL_INTERFACE(node).ident).str;
			break;

		case IDLN_OP_DCL:
			/* FIXME: handle interface StubPrefix property! */
			name = IDL_IDENT(IDL_OP_DCL(node).ident).str;
			break;

		case IDLN_TYPE_ENUM:
			name = IDL_IDENT(IDL_TYPE_ENUM(node).ident).str;
			break;

		default:
			fprintf(stderr, "%s: not defined for <%s>\n", __FUNCTION__,
				NODETYPESTR(node));
			abort();
	}
	return g_strconcat(prefix, name, NULL);
}


/* handles the "rigid" compound types, i.e. structs, unions, and arrays.
 * return value is allocated and must be g_free()'d.
 */
static char *rigid_type(IDL_ns ns, IDL_tree type)
{
	switch(IDL_NODE_TYPE(type)) {
		case IDLN_TYPE_STRUCT: {
			char *l = long_name(ns, type),
				*ret = g_strdup_printf("struct %s", l);
			g_free(l);
			return ret;
		}

		case IDLN_TYPE_UNION:
		case IDLN_TYPE_ARRAY:

		default:
			return g_strdup(basic_type(ns, type));
	}
}


static const char *param_type(IDL_ns ns, IDL_tree type)
{
	if(IS_MAPGRANT_TYPE(type)) return "L4_MapGrantItem_t *";

	return basic_type(ns, type);
}


/* FIXME: fail gracefully */
static const char *return_type(IDL_ns ns, IDL_tree op)
{
	IDL_tree op_type = get_type_spec(IDL_OP_DCL(op).op_type_spec);
	if(IDL_OP_DCL(op).f_oneway) {
		if(op_type != NULL) {
			fprintf(stderr, "can't have non-void return type for oneway operation `%s'\n",
				METHOD_NAME(op));
			exit(EXIT_FAILURE);
		}
		if(IDL_OP_DCL(op).raises_expr != NULL) {
			fprintf(stderr, "can't have exceptions for a oneway operation `%s'\n",
				METHOD_NAME(op));
			exit(EXIT_FAILURE);
		}
	} else if(has_negs_exns(ns, op)) {
		if(op_type == NULL || is_compound_type(op_type)) return "int";
		else if(IDL_NODE_TYPE(op_type) != IDLN_TYPE_INTEGER
				|| !IDL_TYPE_INTEGER(op_type).f_signed)
		{
			fprintf(stderr,
				"return type for a NegativeReturn raising operation must be\n"
				"void, a signed short, long, long long, or a compound type.\n");
			exit(EXIT_FAILURE);
		}
	}

	return is_compound_type(op_type) ? "void" : basic_type(ns, op_type);
}


/* returns what should go in between this "type-section" and the variable name
 * to satisfy arbitrary aesthetic criteria.
 *
 * this doesn't play well with declaring multiple variables on a single line,
 * but fuck it -- i'm not carrying "type section" and "variable section"
 * separately.
 */
static const char *type_space(const char *ctype)
{
	int len = strlen(ctype);
	return ctype[len-1] == '*' ? "" : " ";
}


static void print_vtable(
	FILE *of,
	IDL_tree file_tree,
	IDL_ns ns,
	IDL_tree iface)
{
	GList *methods = NULL;
	GHashTable *ifaces_seen = g_hash_table_new(&g_str_hash, &g_str_equal);
	g_hash_table_insert(ifaces_seen, IFACE_REPO_ID(iface), iface);
	collect_methods(&methods, ifaces_seen, ns, iface);
	g_hash_table_destroy(ifaces_seen);
	methods = g_list_reverse(methods);

	char *prefix = iface_prefix(ns, iface);
	fprintf(of, "struct %s_vtable\n{\n", prefix);

	for(GList *cur = g_list_first(methods);
		cur != NULL;
		cur = g_list_next(cur))
	{
		IDL_tree op = cur->data;

		const char *rettypstr = return_type(ns, op);
		char *name = decapsify(METHOD_NAME(op));
		fprintf(of, "\t%s%s(*%s)(", rettypstr, type_space(rettypstr),
			name);
		g_free(name);

		bool first_param = true;
		IDL_tree rettyp = get_type_spec(IDL_OP_DCL(op).op_type_spec);
		if(is_compound_type(rettyp)) {
			/* compound type return values are declared as the first parameter,
			 * because they're on the "left side".
			 */
			first_param = false;
			if(IS_MAPGRANT_TYPE(rettyp)) {
				fprintf(of, "L4_MapGrantItem_t *_result");
			} else if(IDL_NODE_TYPE(rettyp) == IDLN_TYPE_SEQUENCE) {
				IDL_tree elemtyp = get_type_spec(
					IDL_TYPE_SEQUENCE(rettyp).simple_type_spec);
				char *b = rigid_type(ns, elemtyp);
				fprintf(of, "%s **_result_ptr, unsigned *_result_len", b);
				g_free(b);
			} else {
				fprintf(stderr, "%s: compound type <%s> not supported as return type\n",
					__FUNCTION__, NODETYPESTR(rettyp));
				exit(EXIT_FAILURE);
			}
		}

		IDL_tree params = IDL_OP_DCL(op).parameter_dcls;
		for(IDL_tree iter = params;
			iter != NULL;
			iter = IDL_LIST(iter).next)
		{
			IDL_tree p = IDL_LIST(iter).data,
				decl = IDL_PARAM_DCL(p).simple_declarator,
				type = get_type_spec(IDL_PARAM_DCL(p).param_type_spec);
			const char *typestr = param_type(ns, type),
				*name = IDL_IDENT(decl).str;
			if(first_param) first_param = false; else fprintf(of, ", ");
			switch(IDL_PARAM_DCL(p).attr) {
				case IDL_PARAM_IN:
					fprintf(of, "%s%s%s%s",
						is_compound_type(type) ? "const " : "",
						typestr, type_space(typestr), name);
					break;

				case IDL_PARAM_OUT:
				case IDL_PARAM_INOUT:
					fprintf(of, "out or in-out parameters not done\n");
					break;
			}
		}

		fprintf(of, "),\n");
	}
	fprintf(of, "};\n");
	g_list_free(methods);
	g_free(prefix);
}


static gboolean pick_ifaces(IDL_tree_func_data *tf, gpointer userdata)
{
	GHashTable *ifaces = userdata;
	switch(IDL_NODE_TYPE(tf->tree)) {
		default: return FALSE;

		case IDLN_SRCFILE:
		case IDLN_MODULE:
		case IDLN_LIST:
			return TRUE;

		case IDLN_INTERFACE: {
			char *name = IDL_IDENT(IDL_INTERFACE(tf->tree).ident).str;
			g_hash_table_insert(ifaces, name, tf->tree);
			return FALSE;
		}
	}
}


/* result is <char *> -> <IDL_tree [nodetype IDLN_INTERFACE]>, where the key is
 * allocated within the given tree and is the unqualified name of the interface
 * in question.
 */
static GHashTable *collect_ifaces(IDL_tree tree, IDL_ns ns)
{
	GHashTable *ifaces = g_hash_table_new(&g_str_hash, &g_str_equal);
	IDL_tree_walk_in_order(tree, &pick_ifaces, ifaces);
	return ifaces;
}


bool do_idl_file(const char *filename)
{
	IDL_tree tree = NULL;
	IDL_ns ns = NULL;
	int n = IDL_parse_filename(filename, "-I idl", &msg_callback,
		&tree, &ns, IDLF_PROPERTIES | IDLF_XPIDL | IDLF_SHOW_CPP_ERRORS
			| IDLF_COMBINE_REOPENED_MODULES | IDLF_INHIBIT_INCLUDES,
		IDL_WARNING1);
	if(n == IDL_ERROR) {
		fprintf(stderr, "IDL_parse_filename() failed.\n");
		return false;
	} else if(n < 0) {
		perror(filename);
		return false;
	}

	GHashTable *ifaces = collect_ifaces(tree, ns);
	GHashTableIter iter;
	g_hash_table_iter_init(&iter, ifaces);
	gpointer key, value;
	while(g_hash_table_iter_next(&iter, &key, &value)) {
		fprintf(stdout, "/* vtable for `%s': */\n", (const char *)key);
		print_vtable(stdout, tree, ns, (IDL_tree)value);
	}

	g_hash_table_destroy(ifaces);


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
		bool status = do_idl_file(argv[i]);
		ok = ok && status;
	}
	return ok ? EXIT_SUCCESS : EXIT_FAILURE;
}
