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
#include <locale.h>
#include <errno.h>
#include <ctype.h>
#include <glib.h>
#include <libIDL/IDL.h>

#include "muidl.h"


/* command-line arguments */
gboolean arg_verbose = FALSE;
static gboolean arg_version = FALSE;
static gchar **arg_defines = NULL, **arg_idl_files = NULL,
	**arg_include_paths = NULL;


static void add_str_f(GList **listptr, const char *fmt, ...)
	__attribute__((format(printf, 2, 3)));

static int strvlen(char **strv) __attribute__((pure));


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


static int compare_str_ptr(const void *a, const void *b)
{
	const char *aa = *(char **)a, *bb = *(char **)b;
	return strcmp(aa, bb);
}


static int strvlen(char **strv) {
	return strv == NULL ? 0 : g_strv_length(strv);
}


bool is_reserved_word(const char *str)
{
	/* TODO: this list is not confirmed complete. */
	static const char *reserved[] = {
		"asm", "auto",
		"bool", "break",
		"case", "char", "const", "continue",
		"default", "do", "double",
		"else", "enum", "extern",
		"false", "float", "for",
		"goto",
		"if", "inline", "int",
		"long",
		"register", "return",
		"short", "signed", "sizeof", "static", "struct", "switch",
		"true", "typedef",
		"union", "unsigned",
		"void", "volatile",
		"while",
	};
	if(str == NULL || str[0] == '\0') return false;
	void *ptr = bsearch(&str, reserved, G_N_ELEMENTS(reserved),
		sizeof(const char *), compare_str_ptr);
	if(ptr != NULL) return true;
	for(int i=1; i<G_N_ELEMENTS(reserved); i++) {
		assert(strcmp(reserved[i-1], reserved[i]) < 0);
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


GList *all_methods_of_iface(IDL_ns ns, IDL_tree iface)
{
	GList *methods = NULL;
	GHashTable *ifaces_seen = g_hash_table_new(&g_str_hash, &g_str_equal);
	g_hash_table_insert(ifaces_seen, IFACE_REPO_ID(iface), iface);
	collect_methods(&methods, ifaces_seen, ns, iface);
	g_hash_table_destroy(ifaces_seen);
	return g_list_reverse(methods);
}


char *decapsify(const char *name)
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


bool is_negs_exn(IDL_tree exn)
{
	/* FIXME: once libIDL supports properties on exceptions, use
	 * those to recognize this sort of thing.
	 */
	const char *rid = IDL_IDENT_REPO_ID(IDL_EXCEPT_DCL(exn).ident);
	return strcmp(rid, "IDL:Posix/Errno:1.0") == 0;
}


static IDL_tree find_neg_exn(IDL_tree op)
{
	for(IDL_tree cur = IDL_OP_DCL(op).raises_expr;
		cur != NULL;
		cur = IDL_LIST(cur).next)
	{
		IDL_tree e = IDL_get_parent_node(IDL_LIST(cur).data,
			IDLN_EXCEPT_DCL, NULL);
		if(is_negs_exn(e)) return e;
	}
	return NULL;
}


IDL_tree get_type_spec(IDL_tree node)
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
bool is_value_type(IDL_tree type)
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


char *value_type(IDL_ns ns, IDL_tree type)
{
	if(type == NULL) return g_strdup("void");
	else if(!is_value_type(type)) {
		fprintf(stderr, "%s: <%s> is not a value type\n", __FUNCTION__,
			NODETYPESTR(type));
		exit(EXIT_FAILURE);
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
char *long_name(IDL_ns ns, IDL_tree node)
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

		case IDLN_EXCEPT_DCL:
			/* FIXME: prefixes etc? */
			ident = IDL_EXCEPT_DCL(node).ident;
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

	char *name = decapsify(IDL_IDENT(ident).str),
		*ret = g_strconcat(prefix, name, NULL);
	g_free(name);
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


bool is_rigid_type(IDL_ns ns, IDL_tree type)
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


char *rigid_type(IDL_ns ns, IDL_tree type)
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


char *return_type(IDL_ns ns, IDL_tree op, bool *real_p)
{
	bool spare_bool;
	if(real_p == NULL) real_p = &spare_bool;

	IDL_tree op_type = get_type_spec(IDL_OP_DCL(op).op_type_spec);
	if(find_neg_exn(op) != NULL) {
		*real_p = false;
		return g_strdup("int");
	} else if(op_type == NULL || !is_value_type(op_type)) {
		*real_p = false;
		return g_strdup("void");
	} else {
		*real_p = true;
		return value_type(ns, op_type);
	}
}


/* returns the type as the type of an in-parameter. these are the same for
 * server skeletons, caller stub prototypes, and exception encoder prototypes.
 * defined only for value and rigid types; long types are handled differently
 * in each case.
 */
char *in_param_type(IDL_ns ns, IDL_tree tree)
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


const char *type_space(const char *ctype)
{
	int len = strlen(ctype);
	return ctype[len-1] == '*' ? "" : " ";
}


char *vtable_prefix(IDL_ns ns, IDL_tree iface)
{
	char *l = long_name(ns, iface), *ret = decapsify(l);
	g_free(l);
	return ret;
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


static char *skipwhites(const char *s)
{
	while(isblank(*s)) s++;
	return (char *)s;
}


int code_vf(struct print_ctx *pr, const char *fmt, va_list args)
{
	char *text = g_strdup_vprintf(fmt, args),
		**lines = g_strsplit(skipwhites(text), "\n", 0),
		prefix[pr->indent_level + 1];

	for(int i=0; i<pr->indent_level; i++) prefix[i] = '\t';
	prefix[pr->indent_level] = '\0';
	int total = 0;
	if(lines[0] == NULL) total = fprintf(pr->of, "\n");
	for(int i=0; lines[i] != NULL; i++) {
		total += fprintf(pr->of, "%s%s\n", lines[i][0] != '\0' ? prefix : "",
			lines[i]);
	}
	g_strfreev(lines);
	g_free(text);
	return total;
}


int code_f(struct print_ctx *pr, const char *fmt, ...)
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


/* NOTE: these actually add items to the _front_, since it is a O(1) operation
 * (disregarding allocator overhead etc). reverse before use, or iterate back
 * to front.
 */
static void add_str_vf(GList **listptr, const char *fmt, va_list al)
{
	*listptr = g_list_prepend(*listptr, g_strdup_vprintf(fmt, al));
}


static void add_str_f(GList **listptr, const char *fmt, ...)
{
	va_list al;
	va_start(al, fmt);
	add_str_vf(listptr, fmt, al);
	va_end(al);
}


static unsigned short_mask(IDL_tree fldtype)
{
	if(IS_USHORT_TYPE(fldtype)) return 0xffff;
	else if(IDL_NODE_TYPE(fldtype) == IDLN_TYPE_OCTET) return 0xff;
	else NOTDEFINED(fldtype);
}


static void print_op_decode(struct print_ctx *pr, struct method_info *inf)
{
	code_f(pr, "/* decoding of `%s' */", METHOD_NAME(inf->node));

	/* helper variables. */
	for(int i=0; i<inf->num_params; i++) {
		const struct param_info *pinf = &inf->params[i];

		if(is_value_type(pinf->type)
			&& IDL_PARAM_DCL(pinf->param_dcl).attr == IDL_PARAM_IN)
		{
			/* automatic valuetype in-parameters get implicit conversion at
			 * the function call site, so we don't decode them into
			 * variables here.
			 */
			continue;
		}

		char *initializer = NULL;
		if(IDL_PARAM_DCL(pinf->param_dcl).attr != IDL_PARAM_OUT) {
			if(IS_MAPGRANT_TYPE(pinf->type)) {
				assert(pinf->first_reg == pinf->last_reg - 1);
				initializer = g_strdup_printf(" = {\n"
					"\t.raw = { L4_MsgWord(&msg, %d), L4_MsgWord(&msg, %d) }\n"
					"}", (int)pinf->first_reg - 1,
					(int)pinf->last_reg - 1);
			} else if(IS_FPAGE_TYPE(pinf->type)) {
				initializer = g_strdup_printf(" = { .raw = L4_MsgWord(&msg, %d) }",
					(int)pinf->first_reg - 1);
			} else if(is_value_type(pinf->type)) {
				initializer = g_strdup_printf(" = L4_MsgWord(&msg, %d)",
					(int)pinf->first_reg - 1);
			} else {
				/* TODO: add rigid, long types */
				NOTDEFINED(pinf->type);
			}
		}

		char *tstr;
		if(is_value_type(pinf->type)) tstr = value_type(pr->ns, pinf->type);
		else if(is_rigid_type(pr->ns, pinf->type)) {
			tstr = rigid_type(pr->ns, pinf->type);
		} else {
			NOTDEFINED(pinf->type);
		}
		code_f(pr, "%s p_%s%s;", tstr, pinf->name,
			initializer != NULL ? initializer : "");
		g_free(tstr);
		g_free(initializer);
	}

	IDL_tree rettyp = get_type_spec(IDL_OP_DCL(inf->node).op_type_spec);
	bool ret_is_real;
	char *rtstr = return_type(pr->ns, inf->node, &ret_is_real),
		*name = decapsify(METHOD_NAME(inf->node));
	const bool is_voidfn = (rettyp == NULL || strcmp(rtstr, "void") == 0);

	/* build the actual parameter list. */
	GList *parm_list = NULL;	/* <char *>, g_free()'d at end */

	/* the result out-parameter, if non-valuetype or both hidden by error
	 * status and not unsigned short or octet
	 */
	if(rettyp != NULL && !ret_is_real && !IS_USHORT_TYPE(rettyp)
		&& IDL_NODE_TYPE(rettyp) != IDLN_TYPE_OCTET)
	{
		char *typ;
		if(is_value_type(rettyp)) typ = value_type(pr->ns, rettyp);
		else if(IS_MAPGRANT_TYPE(rettyp)) typ = g_strdup("L4_MapItem_t");
		else typ = rigid_type(pr->ns, rettyp);
		code_f(pr, "%s result;", typ);
		add_str_f(&parm_list, "&result");
		g_free(typ);
	}

	for(int i=0; i<inf->num_params; i++) {
		struct param_info *pinf = &inf->params[i];
		switch(IDL_PARAM_DCL(pinf->param_dcl).attr) {
			case IDL_PARAM_IN:
				if(IS_FPAGE_TYPE(pinf->type)) {
					add_str_f(&parm_list, "p_%s", pinf->name);
				} else if(is_value_type(pinf->type)) {
					for(int r=pinf->first_reg; r <= pinf->last_reg; r++) {
						add_str_f(&parm_list, "L4_MsgWord(&msg, %d)", r - 1);
					}
				} else {
					NOTDEFINED(pinf->type);
				}
				break;

			case IDL_PARAM_INOUT:
			case IDL_PARAM_OUT:
				add_str_f(&parm_list, "&p_%s", pinf->name);
				break;
		}
	}

	/* the vtable call. */
	const char *ret1, *ret2;
	if(is_voidfn) {
		ret1 = "";
		ret2 = "";
	} else {
		ret1 = rtstr;
		ret2 = " retval = ";
	}
	if(g_list_first(parm_list) == NULL) {
		/* parameterless operations that don't even have a result
		 * parameter.
		 */
		code_f(pr, "%s%s(*vtable->%s)();", ret1, ret2, name);
	} else {
		parm_list = g_list_reverse(parm_list);
		for(GList *cur = g_list_first(parm_list);
			cur != NULL;
			cur = g_list_next(cur))
		{
			const char *str = cur->data,
				*suffix = g_list_next(cur) != NULL ? "," : ");";
			if(cur == g_list_first(parm_list)) {
				code_f(pr, "%s%s(*vtable->%s)(%s%s", ret1, ret2, name,
					(const char *)g_list_first(parm_list)->data, suffix);
				indent(pr, 1);
			} else {
				code_f(pr, "%s%s", str, suffix);
			}
		}
		indent(pr, -1);
	}

	if(!IDL_OP_DCL(inf->node).f_oneway) {
		code_f(pr, "L4_MsgClear(&msg);");
		IDL_tree n_ex = find_neg_exn(inf->node);
		bool first_if = true;
		if(n_ex != NULL) {
			assert(strcmp(rtstr, "int") == 0);
			code_f(pr, "if(retval < 0) {");
			indent(pr, 1);
			/* FIXME: grab exception label from somewhere! */
			code_f(pr, "L4_Set_MsgLabel(&msg, MSG_ERROR);");
			IDL_tree memb = IDL_LIST(IDL_EXCEPT_DCL(n_ex).members).data,
				fldtype = get_type_spec(IDL_MEMBER(memb).type_spec);
			assert(IDL_list_length(IDL_MEMBER(memb).dcls) == 1);
			code_f(pr, "L4_MsgAppendWord(&msg, %#x & ((L4_Word_t)-retval));",
				short_mask(fldtype));
			close_brace(pr);
			first_if = false;
		}

		/* TODO: if the operation has any exceptions, check whether ctx->exmsg
		 * label is 0; if not, forward the exception.
		 */

		if(inf->num_out_params > 0
			|| IDL_OP_DCL(inf->node).op_type_spec != NULL)
		{
			if(!first_if) {
				code_f(pr, "else {");
				indent(pr, 1);
			}
			int next_r = 1;
			if(n_ex != NULL && rettyp != NULL) {
				/* strictly positive integral return types that're 31 bits or
				 * shorter: unsigned short and octet. (whee!)
				 */
				code_f(pr, "L4_MsgAppendWord(&msg, retval & %#x);",
					short_mask(rettyp));
				next_r++;
			} else if(rettyp != NULL) {
				code_f(pr, "/* TODO: would encode non-n_ex return type here */");
			}
			for(int i=0; i<inf->num_params; i++) {
				struct param_info *pi = &inf->params[i];
				if(IDL_PARAM_DCL(pi->param_dcl).attr == IDL_PARAM_IN) {
					continue;
				}

				for(int r = pi->first_reg; r <= pi->last_reg; r++) {
					if(r != next_r) {
						fprintf(stderr, "warning: next reg# is %d, but r is %d (opdcl `%s')\n",
							next_r, r, pi->name);
					}
					if(IS_FPAGE_TYPE(pi->type)) {
						code_f(pr, "L4_MsgAppendWord(&msg, p_%s.raw);", pi->name);
						next_r++;
					} else if(is_value_type(pi->type)) {
						code_f(pr, "L4_MsgAppendWord(&msg, p_%s);", pi->name);
						next_r++;
					} else {
						/* TODO: handle mapitems, rigid types, and long types
						 * as out-values too
						 */
						NOTDEFINED(pi->type);
					}
				}
			}
			if(!first_if) close_brace(pr);
		}
	}

	g_free(rtstr);
	g_free(name);
	g_list_foreach(parm_list, (GFunc)g_free, NULL);
}


/* FIXME: this is really a _skeleton_ utility function; i.e. the skeleton calls
 * a mod_iface_exnname_raise(member1, member2, member3, etc) function and
 * returns zero or void. the exception message is then picked up and propagated
 * by the dispatcher.
 *
 * so rewrite how this stuff is emitted for real and declare the functions
 * extern with prototypes in $idlfile-defs.h, conditional to a skelimpl #define
 * or something.
 */
static void print_exception_encoder(struct print_ctx *pr, IDL_tree exn)
{
	code_f(pr, "/* would emit encoder for exception `%s' */",
		IDL_IDENT_REPO_ID(IDL_EXCEPT_DCL(exn).ident));
#if 0
	char *exn_ln = long_name(pr->ns, exn);
	code_f(pr, "static void %s_encode(", exn_ln);
	indent(pr, 1);
	code_f(pr, "L4_Msg_t *msgp,");

	/* encoder parameters */
	for(IDL_tree cur = IDL_EXCEPT_DCL(exn).members;
		cur != NULL;
		cur = IDL_LIST(cur).next)
	{
		IDL_tree member = IDL_LIST(cur).data,
			type = get_type_spec(IDL_MEMBER(member).type_spec);
		/* TODO: handle and/or forbid long types */
		char *typestr = in_param_type(pr->ns, type);
		const bool last = IDL_LIST(cur).next == NULL;
		for(IDL_tree m_iter = IDL_MEMBER(member).dcls;
			m_iter != NULL;
			m_iter = IDL_LIST(m_iter).next)
		{
			IDL_tree ident = IDL_LIST(m_iter).data;
			code_f(pr, "%s%s%s%s", typestr, type_space(typestr),
				IDL_IDENT(ident).str, last ? ")" : ",");
		}
		g_free(typestr);
	}
	indent(pr, -1);

	code_f(pr, "{");
	indent(pr, 1);

	code_f(pr, "/* insert encoder body here */");

	close_brace(pr);
	code_f(pr, " ");
	g_free(exn_ln);
#endif
}


static gint exn_repoid_compare(gconstpointer a, gconstpointer b)
{
	IDL_tree n = (IDL_tree)a, m = (IDL_tree)b;
	return strcmp(IDL_IDENT_REPO_ID(IDL_EXCEPT_DCL(n).ident),
		IDL_IDENT_REPO_ID(IDL_EXCEPT_DCL(m).ident));
}


/* generate local (static) encoders for all exceptions referenced by this
 * interface's operations.
 */
static void print_exceptions_for_iface(struct print_ctx *pr, IDL_tree iface)
{
	/* collect the exceptions. */
	GHashTable *exn_hash = g_hash_table_new(&g_str_hash, &g_str_equal);
	for(IDL_tree if_iter = IDL_INTERFACE(iface).body;
		if_iter != NULL;
		if_iter = IDL_LIST(if_iter).next)
	{
		IDL_tree elem = IDL_LIST(if_iter).data;
		if(IDL_NODE_TYPE(elem) != IDLN_OP_DCL) continue;

		for(IDL_tree r_iter = IDL_OP_DCL(elem).raises_expr;
			r_iter != NULL;
			r_iter = IDL_LIST(r_iter).next)
		{
			IDL_tree ident = IDL_LIST(r_iter).data;
			IDL_tree ex = IDL_get_parent_node(ident, IDLN_EXCEPT_DCL, NULL);
			if(ex == NULL) {
				/* FIXME: gracefully plz */
				fprintf(stderr, "%s: unknown exception `%s'\n",
					__FUNCTION__, IDL_IDENT_REPO_ID(ident));
				exit(EXIT_FAILURE);
			}

			char *repoid = IDL_IDENT_REPO_ID(IDL_EXCEPT_DCL(ex).ident);
			/* pointers to long-lived objects == just overwrite where
			 * necessary, it's all good
			 */
			g_hash_table_insert(exn_hash, repoid, ex);
		}
	}

	GList *exn_list = g_hash_table_get_values(exn_hash);
	/* sort them by repoid for consistency. */
	exn_list = g_list_sort(exn_list, &exn_repoid_compare);

	for(GList *cur = g_list_first(exn_list);
		cur != NULL;
		cur = g_list_next(cur))
	{
		IDL_tree exn = cur->data;
		print_exception_encoder(pr, exn);
	}

	g_list_free(exn_list);
	g_hash_table_destroy(exn_hash);
}


/* this outputs a very generic dispatcher. ones written in customized assembly
 * code are optimizations, which we won't touch until µidl is reasonably
 * feature-complete. (i.e. need-to basis, and optimizations don't usually need
 * to.)
 */
static void print_dispatcher_for_iface(struct print_ctx *pr, IDL_tree iface)
{
	code_f(pr, "/* dispatcher for interface `%s' */",
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
			fprintf(stderr, "error: analyse_op_dcl() failed\n");
			exit(EXIT_FAILURE);
		}

		if(inf->label == 0) {
			/* FIXME: assign method labels at some point */
			fprintf(stderr, "error: can't assign automatic label to `%s'\n",
				METHOD_NAME(method));
			exit(EXIT_FAILURE);
		}

		cur->data = inf;
		if(inf->tagmask != NO_TAGMASK) {
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
		assert(inf->tagmask != NO_TAGMASK);
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

			code_f(pr, "case 0x%lx: {", (unsigned long)inf->label);
			indent(pr, 1);
			print_op_decode(pr, inf);
			if(IDL_OP_DCL(inf->node).f_oneway) code_f(pr, "reply = false;");
			code_f(pr, "break;");
			close_brace(pr);
			code_f(pr, " ");		/* aesthetics, man! */
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
	code_f(pr, " ");

	g_list_foreach(methods, (GFunc)g_free, NULL);
	g_list_free(methods);
	g_list_free(tagmask_list);
	g_free(vtprefix);
}


static gboolean iter_print_dispatchers(IDL_tree_func_data *tf, void *ud)
{
	struct print_ctx *pr = ud;
	switch(IDL_NODE_TYPE(tf->tree)) {
		case IDLN_LIST:
		case IDLN_MODULE:
		case IDLN_SRCFILE:
			return TRUE;

		default: return FALSE;

		case IDLN_INTERFACE:
			/* TODO: have an option to not emit inhibited exceptions, i.e.
			 * those that weren't declared in the IDL file at hand. for now
			 * we'll regard dispatcher source as self-contained.
			 */
			print_exceptions_for_iface(pr, tf->tree);
			print_dispatcher_for_iface(pr, tf->tree);
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
		exit(EXIT_FAILURE);
	}

	(*prtfn)((struct print_ctx *)pr);
	pr->of = oldof;
	fclose(f);
}


bool do_idl_file(const char *cppopts, const char *filename)
{
	IDL_tree tree = NULL;
	IDL_ns ns = NULL;
	int n = IDL_parse_filename(filename, cppopts, &msg_callback,
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

	if(!verify_idl_input(ns, tree)) {
		fprintf(stderr, "verification of `%s' failed.\n", filename);
		IDL_ns_free(ns);
		IDL_tree_free(tree);
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


static void parse_cmdline(int argc, char *argv[])
{
	static const GOptionEntry entries[] = {
		{ "cpp-define", 'D', 0, G_OPTION_ARG_STRING_ARRAY, &arg_defines,
		  "command-line definitions for cpp(1)", "NAME[=VALUE]" },
		{ "include-path", 'I', 0, G_OPTION_ARG_STRING_ARRAY,
		  &arg_include_paths, "add PATH to preprocessor search list", "PATH" },
		{ "verbose", 'v', 0, G_OPTION_ARG_NONE, &arg_verbose,
		  "enable verbose output", NULL },
		{ "version", 'V', 0, G_OPTION_ARG_NONE, &arg_version,
		  "print muidl version and exit", NULL },
		{ G_OPTION_REMAINING, 0, 0, G_OPTION_ARG_STRING_ARRAY, &arg_idl_files,
		  "IDL files", "[IDLFILE...]" },
		{ NULL }
	};
	GError *error = NULL;
	GOptionContext *oc = g_option_context_new("- µiX IDL compiler");
	g_option_context_add_main_entries(oc, entries, NULL);
	if(!g_option_context_parse(oc, &argc, &argv, &error)) {
		fprintf(stderr, "option parsing error: %s\n", error->message);
		g_error_free(error);
		exit(EXIT_FAILURE);
	}
	g_option_context_free(oc);
}


static char *join_cpp_opts(const char *parm, char **strv)
{
	int num = strvlen(strv);
	if(num == 0) {
		/* fuck you then. */
		return g_strdup("");
	}
	GString *buf = g_string_sized_new(num * (16 + strlen(parm)));
	for(int i=0; i<num; i++) {
		g_string_append_printf(buf, "%s '%s'", parm, strv[i]);
	}
	return g_string_free(buf, FALSE);
}


int main(int argc, char *argv[])
{
	setlocale(LC_CTYPE, "");
	parse_cmdline(argc, argv);

	if(arg_version) {
		printf("muidl (µidl) version 0.1\n");
		return EXIT_SUCCESS;
	}

	if(strvlen(arg_idl_files) == 0) {
		printf("no input files\n");
		return EXIT_SUCCESS;
	}

	IDL_check_cast_enable(TRUE);

	/* prepare options for cpp. */
	char *defs = join_cpp_opts("-D", arg_defines),
		*incs = join_cpp_opts("-I", arg_include_paths),
		*opts = g_strdup_printf("%s %s", defs, incs);
	g_free(defs);
	g_free(incs);

	bool ok = true;
	for(int i=0; i < strvlen(arg_idl_files); i++) {
		bool status = do_idl_file(opts, arg_idl_files[i]);
		ok = ok && status;
	}

	g_free(opts);
	g_strfreev(arg_defines);
	g_strfreev(arg_idl_files);

	return ok ? EXIT_SUCCESS : EXIT_FAILURE;
}
