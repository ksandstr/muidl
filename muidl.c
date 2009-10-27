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
#include <stdint.h>
#include <string.h>
#include <stdbool.h>
#include <setjmp.h>
#include <assert.h>
#include <alloca.h>
#include <errno.h>
#include <ctype.h>
#include <glib.h>
#include <libIDL/IDL.h>


#define IFACE_REPO_ID(t) IDL_IDENT_REPO_ID(IDL_INTERFACE((t)).ident)
#define IFACE_NAME(t) (IDL_IDENT(IDL_INTERFACE((t)).ident).str)
#define METHOD_NAME(t) (IDL_IDENT(IDL_OP_DCL((t)).ident).str)
#define NATIVE_NAME(t) (IDL_IDENT(IDL_NATIVE((t)).ident).str)

#define IS_MAPGRANT_TYPE(t) (IDL_NODE_TYPE((t)) == IDLN_NATIVE \
	&& strcmp(NATIVE_NAME((t)), "l4_mapgrantitem_t") == 0)
#define IS_FPAGE_TYPE(t) (IDL_NODE_TYPE((t)) == IDLN_NATIVE \
	&& strcmp(NATIVE_NAME((t)), "l4_fpage_t") == 0)
#define IS_WORD_TYPE(t) (IDL_NODE_TYPE((t)) == IDLN_NATIVE \
	&& strcmp(NATIVE_NAME((t)), "l4_word_t") == 0)

#define NODETYPESTR(n) ({ \
		IDL_tree __n = (n); \
		__n == NULL ? "(nil)" : IDL_tree_type_names[IDL_NODE_TYPE(__n)]; \
	})

#define NOTDEFINED(t) do { \
		fprintf(stderr, "%s: not defined for <%s>\n", __FUNCTION__, \
			NODETYPESTR((t))); \
		assert(0); \
		abort(); \
	} while(0)

#define NOTSUPPORTED(what) do { \
		fprintf(stderr, "%s: µidl does not support %s\n", \
			__FUNCTION__, (what)); \
		exit(EXIT_FAILURE); \
	} while(0)


struct print_ctx
{
	FILE *of;
	IDL_ns ns;
	IDL_tree tree;
	const char *idlfilename;
	const char *common_header_name;
	GHashTable *ifaces;
	int indent_level;

	/* error handling bits */
	jmp_buf fail_to;
};


struct param_info
{
	const char *name;
	IDL_tree type, param_dcl;
	uint8_t first_reg, last_reg;
};


struct method_info
{
	IDL_tree node;
	uint16_t label;
	uint32_t tagmask;
	int num_params;
	struct param_info params[];
};


static char *long_name(IDL_ns ns, IDL_tree node);
static bool is_rigid_type(IDL_ns ns, IDL_tree type);


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


static bool is_reserved_word(const char *str)
{
	/* TODO: this list needs to be expanded quite a bit. */
	static const char *reserved[] = {
		"register",
	};
	if(str == NULL || str[0] == '\0') return false;
	for(int i=0; i<G_N_ELEMENTS(reserved); i++) {
		if(strcmp(str, reserved[i]) == 0) return true;
	}
	return false;
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


static GList *all_methods_of_iface(IDL_ns ns, IDL_tree iface)
{
	GList *methods = NULL;
	GHashTable *ifaces_seen = g_hash_table_new(&g_str_hash, &g_str_equal);
	g_hash_table_insert(ifaces_seen, IFACE_REPO_ID(iface), iface);
	collect_methods(&methods, ifaces_seen, ns, iface);
	g_hash_table_destroy(ifaces_seen);
	return g_list_reverse(methods);
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
			if(i > 0 && islower(name[i-1])) out[o++] = '_';
			out[o++] = tolower(name[i]);
		} else {
			out[o++] = name[i];
		}
	}
	out[o] = '\0';
	return out;
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


/* returns "true" for types that are passed by value.
 *
 * the type bestiary is divided into three categories:
 *   - value types (shorts, longs, words, etc), which are passed and returned
 *     by value, and are simple members of structs and unions;
 *   - rigid types (unions, structs, arrays), which are passed and returned by
 *     reference, and are simple members of structs and unions;
 *   - long types (sequences, "any", fixed, etc), which have special handling
 *     for each, and are separately allocated members of structs and unions.
 */
static bool is_value_type(IDL_tree type)
{
	if(type == NULL) return false;	/* void is not a value type. */
	switch(IDL_NODE_TYPE(type)) {
		/* rigid & long types */
		case IDLN_TYPE_ARRAY:
		case IDLN_TYPE_SEQUENCE:
		case IDLN_TYPE_STRUCT:
		case IDLN_TYPE_UNION:
		case IDLN_TYPE_ANY:
		case IDLN_TYPE_FIXED:
		case IDLN_TYPE_STRING:
		case IDLN_TYPE_WIDE_STRING:
			return false;

		case IDLN_NATIVE:
			if(IS_WORD_TYPE(type) || IS_FPAGE_TYPE(type)) return true;
			else if(IS_MAPGRANT_TYPE(type)) return false;	/* rigid */
			else {
				/* FIXME: don't exit */
				fprintf(stderr, "%s: unknown native type `%s'\n",
					__FUNCTION__, NATIVE_NAME(type));
				exit(EXIT_FAILURE);
			}

		/* value types. */
		case IDLN_TYPE_INTEGER:
		case IDLN_TYPE_FLOAT:
		case IDLN_TYPE_CHAR:
		case IDLN_TYPE_WIDE_CHAR:
		case IDLN_TYPE_BOOLEAN:
		case IDLN_TYPE_OCTET:
		case IDLN_TYPE_ENUM:
			return true;

		default:
			fprintf(stderr, "%s: unsupported type <%s>\n",
				__FUNCTION__, NODETYPESTR(type));
			exit(EXIT_FAILURE);
	}
}


/* returns the C type of value types and void. */
static char *value_type(IDL_ns ns, IDL_tree type)
{
	if(type == NULL) return g_strdup("void");
	else if(!is_value_type(type)) {
		fprintf(stderr, "%s: <%s> is not a value type\n", __FUNCTION__,
			NODETYPESTR(type));
		abort();
	} else {
		switch(IDL_NODE_TYPE(type)) {
			case IDLN_TYPE_INTEGER: {
				static const char *ityps[] = {
					[IDL_INTEGER_TYPE_SHORT] = "uint16_t",
					[IDL_INTEGER_TYPE_LONG] = "uint32_t",
					[IDL_INTEGER_TYPE_LONGLONG] = "uint64_t",
				};
				int t = IDL_TYPE_INTEGER(type).f_type;
				assert(t < G_N_ELEMENTS(ityps));
				return g_strdup(ityps[t] +
						(IDL_TYPE_INTEGER(type).f_signed ? 1 : 0));
			}

			case IDLN_NATIVE: {
				if(IS_WORD_TYPE(type)) return g_strdup("L4_Word_t");
				else if(IS_FPAGE_TYPE(type)) return g_strdup("L4_Fpage_t");
				else {
					fprintf(stderr, "%s: native type `%s' not supported\n",
						__FUNCTION__, NATIVE_NAME(type));
					exit(EXIT_FAILURE);
				}
				break;
			}

			case IDLN_TYPE_FLOAT: {
				static const char *ftyps[] = {
					[IDL_FLOAT_TYPE_FLOAT] = "float",
					[IDL_FLOAT_TYPE_DOUBLE] = "double",
					[IDL_FLOAT_TYPE_LONGDOUBLE] = "long double",
				};
				int t = IDL_TYPE_FLOAT(type).f_type;
				assert(t < G_N_ELEMENTS(ftyps));
				return g_strdup(ftyps[t]);
			}

			case IDLN_TYPE_CHAR: return g_strdup("char");
			case IDLN_TYPE_WIDE_CHAR: return g_strdup("wchar_t");
			case IDLN_TYPE_BOOLEAN: return g_strdup("bool");
			case IDLN_TYPE_OCTET: return g_strdup("uint8_t");

			case IDLN_TYPE_ENUM: {
				char *s = long_name(ns, type),
					*ret = g_strdup_printf("enum %s", s);
				g_free(s);
				return ret;
			}

			default:
				NOTDEFINED(type);
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
	IDL_tree ident;
	switch(IDL_NODE_TYPE(node)) {
		case IDLN_TYPE_STRUCT: ident = IDL_TYPE_STRUCT(node).ident; break;
		case IDLN_TYPE_UNION: ident = IDL_TYPE_UNION(node).ident; break;
		case IDLN_INTERFACE: ident = IDL_INTERFACE(node).ident; break;
		case IDLN_TYPE_ENUM: ident = IDL_TYPE_ENUM(node).ident; break;

		case IDLN_OP_DCL:
			/* FIXME: handle interface StubPrefix property! */
			ident = IDL_OP_DCL(node).ident;
			break;

		default:
			NOTDEFINED(node);
	}

	char *prefix;
	IDL_tree mod = IDL_get_parent_node(node, IDLN_MODULE, NULL),
		iface = IDL_get_parent_node(node, IDLN_INTERFACE, NULL);
	if(mod == node) mod = NULL;
	if(iface == node) iface = NULL;
	char *modname = NULL, *ifacename = NULL;
	if(mod != NULL) {
		modname = decapsify(IDL_IDENT(IDL_MODULE(mod).ident).str);
	}
	if(iface != NULL) {
		ifacename = decapsify(IDL_IDENT(IDL_INTERFACE(iface).ident).str);
	}
	if(mod != NULL && iface != NULL) {
		prefix = g_strdup_printf("%s_%s_", modname, ifacename);
	} else if(mod != NULL) prefix = g_strdup_printf("%s_", modname);
	else if(iface != NULL) prefix = g_strdup_printf("%s_", ifacename);
	else prefix = g_strdup("");
	g_free(modname);
	g_free(ifacename);

	const char *name = IDL_IDENT(ident).str;
	char *ret = g_strconcat(prefix, name, NULL);
	g_free(prefix);
	if(is_reserved_word(ret)) {
		char *r2 = g_strconcat("_", ret, NULL);
		g_free(ret);
		ret = r2;
	}
	return ret;
}


static bool is_struct_rigid(IDL_ns ns, IDL_tree type)
{
	for(IDL_tree cur = IDL_TYPE_STRUCT(type).member_list;
		cur != NULL;
		cur = IDL_LIST(cur).next)
	{
		IDL_tree member = IDL_LIST(cur).data,
			type = get_type_spec(IDL_MEMBER(member).type_spec);
		if(!is_rigid_type(ns, type)) return false;
	}
	return true;
}


static bool is_rigid_type(IDL_ns ns, IDL_tree type)
{
	if(type == NULL) return false;	/* void is not a rigid type either. */
	switch(IDL_NODE_TYPE(type)) {
		default: return is_value_type(type);
		case IDLN_TYPE_STRUCT: return is_struct_rigid(ns, type);

		case IDLN_TYPE_STRING:
			/* only bounded strings are rigid. */
			return IDL_TYPE_STRING(type).positive_int_const != NULL;

		case IDLN_TYPE_WIDE_STRING:
			/* same for wide strings. */
			return IDL_TYPE_WIDE_STRING(type).positive_int_const != NULL;

		case IDLN_TYPE_UNION:
		case IDLN_TYPE_ARRAY:
			/* TODO: is rigid if the element type is, but... */
			NOTDEFINED(type);
	}
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

		case IDLN_TYPE_STRING: return g_strdup("char");
		case IDLN_TYPE_WIDE_STRING: return g_strdup("wchar_t");

		case IDLN_TYPE_UNION:
		case IDLN_TYPE_ARRAY:

		default:
			NOTDEFINED(type);
	}
}


#define IS_USHORT_TYPE(op_type) \
	(IDL_NODE_TYPE((op_type)) == IDLN_TYPE_INTEGER \
	&& !IDL_TYPE_INTEGER((op_type)).f_signed \
	&& IDL_TYPE_INTEGER((op_type)).f_type == IDL_INTEGER_TYPE_SHORT)

/* FIXME: fail gracefully */
static char *return_type(IDL_ns ns, IDL_tree op, bool *real_p)
{
	bool spare_bool;
	if(real_p == NULL) real_p = &spare_bool;
	IDL_tree op_type = get_type_spec(IDL_OP_DCL(op).op_type_spec);

	/* oneway restrictions. */
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
	}

	/* NegativeReturn restrictions. */
	if(has_negs_exns(ns, op)) {
		if(op_type == NULL || !is_value_type(op_type)
			|| IS_USHORT_TYPE(op_type))
		{
			*real_p = false;
			return g_strdup("int");
		} else {
			fprintf(stderr,
				"return type for a NegativeReturn raising operation must be\n"
				"void, an unsigned short, or a non-value type.\n");
			exit(EXIT_FAILURE);
		}
	}

	if(op_type == NULL || !is_value_type(op_type)) {
		*real_p = false;
		return g_strdup("void");
	} else {
		*real_p = true;
		return value_type(ns, op_type);
	}
}


/* returns the type as the type of an in-parameter. these are the same for
 * server skeletons and caller stub prototypes. defined only for value and
 * rigid types; long types are handled differently in each case.
 */
static char *in_param_type(IDL_ns ns, IDL_tree tree)
{
	if(is_value_type(tree)) return value_type(ns, tree);
	switch(IDL_NODE_TYPE(tree)) {
		case IDLN_TYPE_STRUCT: {
			char *sname = long_name(ns, tree),
				*ret = g_strdup_printf("struct %s *", sname);
			g_free(sname);
			return ret;
		}

		case IDLN_TYPE_STRING: return g_strdup("const char *");
		case IDLN_TYPE_WIDE_STRING: return g_strdup("const wchar_t *");

		case IDLN_TYPE_ARRAY:
		case IDLN_TYPE_UNION:
			/* TODO */

		default:
			NOTDEFINED(tree);
	}
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


static char *vtable_prefix(IDL_ns ns, IDL_tree iface)
{
	char *l = long_name(ns, iface), *ret = decapsify(l);
	g_free(l);
	return ret;
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


static void indent(struct print_ctx *pr, int change)
{
	if(pr->indent_level + change < 0) {
		fprintf(stderr, "warning: %s: changing %d by %d results in %d; clamped\n",
			__FUNCTION__, pr->indent_level, change, pr->indent_level + change);
		pr->indent_level = 0;
	} else {
		pr->indent_level += change;
	}
}


/* NOTE: code_f() and code_vf() add a newline to the output. */
static int code_vf(struct print_ctx *pr, const char *fmt, va_list args)
{
	char *text = g_strdup_vprintf(fmt, args),
		**lines = g_strsplit(text, "\n", 0),
		*prefix = g_strnfill(pr->indent_level, '\t');
	g_free(text);
	int total = 0;
	for(int i=0; lines[i] != NULL; i++) {
		total += fprintf(pr->of, "%s%s\n", lines[i][0] != '\0' ? prefix : "",
			lines[i]);
	}
	g_strfreev(lines);
	g_free(prefix);
	return total;
}


static int code_f(struct print_ctx *pr, const char *fmt, ...)
{
	va_list al;
	va_start(al, fmt);
	int n = code_vf(pr, fmt, al);
	va_end(al);
	return n;
}


static void close_brace(struct print_ctx *pr)
{
	indent(pr, -1);
	code_f(pr, "}");
}


static void print_common_header(struct print_ctx *pr)
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
	code_f(pr, "");

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


static void print_op_decode(struct print_ctx *pr, struct method_info *inf)
{
	code_f(pr, "/* would decode operation `%s' here */",
		METHOD_NAME(inf->node));
}


/* returns true on success, including property not found (in which case
 * *value_p is not modified).
 */
static bool get_ul_property(
	unsigned long *value_p,
	IDL_tree ident,
	const char *name)
{
	const char *p = IDL_tree_property_get(ident, name);
	if(p == NULL) return true;
	char *endptr = NULL;
	unsigned long ret = strtoul(p, &endptr, 0);
	if(ret == 0 && endptr == p) {
		fprintf(stderr, "error: invalid %s property: `%s'\n", name, p);
		return false;
	} else {
		*value_p = ret;
		return true;
	}
}


/* TODO: get the number and max dimensions of the string buffers we'll
 * send/receive once long types are implemented.
 */
static struct method_info *analyse_op_dcl(
	struct print_ctx *pr,
	IDL_tree method)
{
	IDL_tree param_list = IDL_OP_DCL(method).parameter_dcls;
	const int num_params = IDL_list_length(param_list);
	struct method_info *inf = g_malloc(sizeof(struct method_info)
		+ sizeof(struct param_info) * num_params);
	IDL_tree prop_node = IDL_OP_DCL(method).ident;
	inf->node = method;

	unsigned long tagmask = ~0ul;
	if(!get_ul_property(&tagmask, prop_node, "TagMask")) goto fail;
	inf->tagmask = tagmask;

	unsigned long labelval = 0;
	if(!get_ul_property(&labelval, prop_node, "Label")) goto fail;
	inf->label = labelval;

	/* first pass: get the highest assigned register number and fill in the
	 * parameter info bits so we don't have to crawl the list again.
	 */
	int num_regs = 0, max_fixreg = 0, ppos = 0;
	for(IDL_tree cur = param_list; cur != NULL; cur = IDL_LIST(cur).next) {
		IDL_tree parm = IDL_LIST(cur).data,
			type = get_type_spec(IDL_PARAM_DCL(parm).param_type_spec),
			ident = IDL_PARAM_DCL(parm).simple_declarator;
		struct param_info *pinf = &inf->params[ppos++];
		pinf->name = IDL_IDENT(ident).str;
		pinf->type = type;
		pinf->param_dcl = parm;

		if(is_value_type(type)) num_regs++;
		else {
			/* TODO: count and assign MRs for rigid and long types */
			NOTDEFINED(type);
		}

		unsigned long mr_n = 0;
		if(!get_ul_property(&mr_n, ident, "MR")) goto fail;
		else if(mr_n > 0) {
			if(mr_n > 63) {
				fprintf(stderr, "MR(%lu) too large\n", mr_n);
				goto fail;
			}
			pinf->first_reg = mr_n;
			pinf->last_reg = mr_n;
			max_fixreg = MAX(max_fixreg, mr_n);
		} else {
			pinf->first_reg = 0;
			pinf->last_reg = 0;
		}
	}
	inf->num_params = ppos;

	/* second pass: verify that fixed registers are referenced at most once,
	 * and assign other registers around them.
	 */
	int taken_len = MAX(num_regs, max_fixreg);
	/* (one more to permit one-origin addressing.) */
	bool *taken = alloca(sizeof(bool) * (taken_len + 1));
	for(int i=1; i <= taken_len; i++) taken[i] = false;
	int next_reg = 1, r_used = 0;
	for(int i=0; i < inf->num_params; i++) {
		struct param_info *pinf = &inf->params[i];
		if(pinf->first_reg > 0) {
			assert(pinf->last_reg > 0);
			assert(pinf->last_reg >= pinf->first_reg);
			for(int r = pinf->first_reg; r <= pinf->last_reg; r++) {
				if(taken[r]) {
					fprintf(stderr, "%s: MR%d fixed multiple times\n",
						__FUNCTION__, r);
					goto fail;
				}
				taken[r] = true;
				r_used++;
			}
		} else if(is_value_type(pinf->type)) {
			/* single-word types. */
			int r;
			do {
				r = next_reg++;
				assert(r <= taken_len);
			} while(taken[r]);
			taken[r] = true;
			r_used++;
			pinf->first_reg = r;
			pinf->last_reg = r;
		} else {
			NOTDEFINED(pinf->type);
		}
	}

	return inf;

fail:
	g_free(inf);
	return NULL;
}


/* this outputs a very generic dispatcher. ones written in customized assembly
 * code are optimizations, which we won't touch until µidl is reasonably
 * feature-complete. (i.e. need-to basis, and optimizations don't usually need
 * to.)
 */
static void print_dispatcher_for_iface(IDL_tree iface, struct print_ctx *pr)
{
	code_f(pr, "/* dispatcher for `%s' */\n",
		IDL_IDENT(IDL_INTERFACE(iface).ident).repo_id);

	char *vtprefix = vtable_prefix(pr->ns, iface);
	code_f(pr, "L4_Word_t _muidl_%s_dispatch(", vtprefix);
	indent(pr, 1);
	code_f(pr, "struct %s_vtable *vtable)", vtprefix);
	indent(pr, -1);
	code_f(pr, "{");
	indent(pr, 1);

	GList *methods = all_methods_of_iface(pr->ns, iface);

	GList *tagmask_list = NULL;
	for(GList *cur = g_list_first(methods);
		cur != NULL;
		cur = g_list_next(cur))
	{
		IDL_tree method = cur->data;
		struct method_info *inf = analyse_op_dcl(pr, method);
		if(inf == NULL) {
			/* FIXME: fail properly */
			abort();
		}

		if(inf->label == 0) {
			/* FIXME: assign method labels at some point */
			fprintf(stderr, "error: can't assign automatic label to `%s'\n",
				METHOD_NAME(method));
			abort();
		}

		cur->data = inf;
		if(inf->tagmask != ~0ul) {
			tagmask_list = g_list_prepend(tagmask_list, inf);
		}
	}
	tagmask_list = g_list_reverse(tagmask_list);
	const bool have_switch = g_list_length(tagmask_list) < g_list_length(methods),
		have_tagmask = g_list_length(tagmask_list) > 0;

	code_f(pr,
		"L4_Acceptor_t acc = L4_UntypedWordsAcceptor;\n"
		"for(;;) {");
	indent(pr, 1);
	code_f(pr,
			"L4_Accept(acc);\n"
			"L4_ThreadId_t sender_tid;\n"
			"L4_MsgTag_t tag = L4_Wait(&sender_tid);\n"
			"for(;;) {");
	indent(pr, 1);
	code_f(pr,	"if(L4_IpcFailed(tag)) return L4_ErrorCode();\n"
				"L4_Msg_t msg;\n"
				"L4_MsgStore(tag, &msg);");

	/* tag-mask dispatched things, in IDL order. */
	for(GList *cur = g_list_first(tagmask_list);
		cur != NULL;
		cur = g_list_next(cur))
	{
		struct method_info *inf = cur->data;
		code_f(pr, "%sif((tag.raw & 0x%lx) == 0x%lx) {",
			cur == g_list_first(tagmask_list) ? "" : "} else ",
			(unsigned long)inf->tagmask, (unsigned long)inf->label << 16);
		indent(pr, 1);

		print_op_decode(pr, inf);
		if(IDL_OP_DCL(inf->node).f_oneway) code_f(pr, "break;");

		if(g_list_next(cur) != NULL) indent(pr, -1);
		else {
			/* last element, falls out into either label switch or
			 * unknown-handler.
			 */
			indent(pr, -1);
			code_f(pr, "} else {");
			indent(pr, 1);
		}
	}

	if(have_switch) {
		code_f(pr, "bool reply = true;\nswitch(L4_Label(tag)) {");
		indent(pr, 1);
		for(GList *cur = g_list_first(methods);
			cur != NULL;
			cur = g_list_next(cur))
		{
			struct method_info *inf = cur->data;
			if(g_list_find(tagmask_list, inf) != NULL) continue; /* been done */

			code_f(pr, "case 0x%lx: {", inf->label);
			indent(pr, 1);
			print_op_decode(pr, inf);
			if(IDL_OP_DCL(inf->node).f_oneway) code_f(pr, "reply = false;");
			code_f(pr, "break;");
			close_brace(pr);
			code_f(pr, "");		/* aesthetics, man! */
		}

		code_f(pr, "default: {");
		indent(pr, 1);
	}

	/* handler for unrecognized messages */
	code_f(pr, "/* FIXME: pop an error or something? */");

	if(have_switch) {
		/* finish the unrecognized-messages handler */
		code_f(pr, "break;");
		close_brace(pr);
		/* and the switch stmt */
		close_brace(pr);
		/* prevent msgload, replywait for oneway operations */
		code_f(pr, "if(!reply) break;");
	}

	if(have_tagmask) close_brace(pr);

	code_f(pr, "L4_MsgLoad(&msg);\n"
		"L4_Accept(acc);\n"
		"tag = L4_ReplyWait(sender_tid, &sender_tid);");

	close_brace(pr);	/* inner for loop */
	close_brace(pr);	/* outer for loop */
	close_brace(pr);	/* function */

	g_list_foreach(methods, (GFunc)g_free, NULL);
	g_list_free(methods);
	g_list_free(tagmask_list);
	g_free(vtprefix);
}


static gboolean iter_print_dispatchers(IDL_tree_func_data *tf, void *ud)
{
	switch(IDL_NODE_TYPE(tf->tree)) {
		case IDLN_LIST:
		case IDLN_MODULE:
		case IDLN_SRCFILE:
			return TRUE;

		default: return FALSE;

		case IDLN_INTERFACE:
			print_dispatcher_for_iface(tf->tree, (struct print_ctx *)ud);
			return FALSE;
	}
}


static void print_dispatcher(struct print_ctx *pr)
{
	fprintf(pr->of, "#include <kernel/types.h>\n"
		"#include <kernel/message.h>\n"
		"\n"
		"#include <l4/types.h>\n"
		"#include <l4/message.h>\n"
		"#include <l4/ipc.h>\n"
		"\n");
	fprintf(pr->of, "#include \"%s\"\n\n", pr->common_header_name);

	IDL_tree_walk_in_order(pr->tree, &iter_print_dispatchers, pr);
}


/* FIXME: return errors somehow */
static void print_into(
	const char *filename,
	void (*prtfn)(struct print_ctx *),
	volatile struct print_ctx *pr)
{
	FILE *f = fopen(filename, "wb");
	if(f == NULL) {
		fprintf(stderr, "can't open `%s' for writing: %s\n", filename,
			strerror(errno));
		exit(EXIT_FAILURE);
	}

	FILE *oldof = pr->of;
	pr->of = f;
	if(setjmp(((struct print_ctx *)pr)->fail_to)) {
		fprintf(stderr, "%s: fail handler invoked\n", __FUNCTION__);
		fclose(f);
		abort();
	}

	(*prtfn)((struct print_ctx *)pr);
	pr->of = oldof;
	fclose(f);
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

	const char *filepart = strrchr(filename, '/');
	if(filepart == NULL) filepart = filename; else filepart++;
	char basename[strlen(filepart) + 1];
	memcpy(basename, filepart, strlen(filepart) + 1);
	char *dot = strrchr(basename, '.');
	if(dot != NULL) *dot = '\0';
	char *commonname = g_strdup_printf("%s-defs.h", basename);

	struct print_ctx print_ctx = {
		.of = stdout,
		.ns = ns, .tree = tree, .ifaces = ifaces,
		.idlfilename = filename,
		.common_header_name = commonname,
	};
	print_into(commonname, &print_common_header, &print_ctx);
	char *dispname = g_strdup_printf("%s-dispatch.c", basename);
	print_into(dispname, &print_dispatcher, &print_ctx);

	g_hash_table_destroy(ifaces);
	g_free(commonname);
	g_free(dispname);

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
