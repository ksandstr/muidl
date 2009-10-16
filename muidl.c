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


static char *basic_return_type(IDL_ns ns, IDL_tree op_type)
{
	if(op_type == NULL) return g_strdup("void");
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
				return g_strdup(ityps[t] +
					(IDL_TYPE_INTEGER(op_type).f_signed ? 1 : 0));
			}

			case IDLN_IDENT: {
				assert(IDL_NODE_TYPE(op_type->up) == IDLN_LIST);
				assert(IDL_NODE_TYPE(op_type->up->up) == IDLN_TYPE_DCL);
				IDL_tree type = get_type_spec(op_type->up->up);
				return basic_return_type(ns, type);
			}

			case IDLN_NATIVE: {
				const char *nname = IDL_IDENT(IDL_NATIVE(op_type).ident).str;
				if(strcmp(nname, "l4_word_t") == 0) {
					return g_strdup("L4_Word_t");
				} else if(strcmp(nname, "l4_mapgrantitem_t") == 0) {
					/* FIXME: this should be handled as a structure
					 * out-parameter!
					 */
					return g_strdup("L4_MapGrantItem_t");
				} else {
					fprintf(stderr, "%s: native type `%s' not supported\n",
						__FUNCTION__, nname);
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
				fprintf(stderr, "%s: nodetype <%s> not implemented\n",
					__FUNCTION__, NODETYPESTR(op_type));
				exit(EXIT_FAILURE);
		}
	}
}


/* FIXME: fail gracefully */
static char *return_type(IDL_ns ns, IDL_tree op)
{
	IDL_tree op_type = IDL_OP_DCL(op).op_type_spec;
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
		IDL_tree rettyp = get_type_spec(op_type);
		if(rettyp == NULL) return g_strdup("int");
		else if(IDL_NODE_TYPE(rettyp) != IDLN_TYPE_INTEGER
				|| !IDL_TYPE_INTEGER(rettyp).f_signed)
		{
			fprintf(stderr,
				"return type for a NegativeReturn raising operation must be\n"
				"void or a signed short, long or long long.\n");
			exit(EXIT_FAILURE);
		}
	}

	return basic_return_type(ns, op_type);
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
		IDL_tree type_spec = IDL_OP_DCL(op).op_type_spec,
			params = IDL_OP_DCL(op).parameter_dcls,
			raises = IDL_OP_DCL(op).raises_expr,
			context = IDL_OP_DCL(op).context_expr;
		fprintf(of, "\t/* entry for `%s' */\n", METHOD_NAME(op));
#if 0
		printf("op_type_spec = <%s>\n", NODETYPESTR(type_spec));
		printf("params = <%s>\n", NODETYPESTR(params));
		printf("raises = <%s>\n", NODETYPESTR(raises));
		printf("context = <%s>\n", NODETYPESTR(context));
#endif

		char *rettyp = return_type(ns, op),
			*name = decapsify(METHOD_NAME(op));
		fprintf(of, "\t%s (*%s)(void),\n", rettyp, name);
		g_free(rettyp);
		g_free(name);
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
