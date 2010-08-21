/*
 * muidl.c -- IDL compiler for µiX
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
#include <fcntl.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <glib.h>
#include <libIDL/IDL.h>
#include <llvm-c/Analysis.h>
#include <llvm-c/BitWriter.h>

#include "muidl.h"
#include "llvmutil.h"


/* command-line arguments */
gboolean arg_verbose = FALSE;
static gboolean arg_version = FALSE, arg_verbose_idl = FALSE,
	arg_defs_only = FALSE, arg_client_only = FALSE, arg_service_only = FALSE,
	arg_dump_llvm = FALSE;
static gchar **arg_defines = NULL, **arg_idl_files = NULL,
	**arg_include_paths = NULL, *arg_dest_path = NULL;


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


IDL_tree find_neg_exn(IDL_tree op)
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


bool is_real_nre_return_type(IDL_tree typ)
{
	if(typ == NULL) return false;	/* "void" is not considered "real". */
	return IS_USHORT_TYPE(typ)
		|| IDL_NODE_TYPE(typ) == IDLN_TYPE_OCTET;
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
			if(IS_WORD_TYPE(type) || IS_FPAGE_TYPE(type)
				|| IS_TIME_TYPE(type))
			{
				return true;
			} else if(IS_MAPGRANT_TYPE(type)) {
				return false;	/* rigid, but not single-word */
			} else {
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
				__FUNCTION__, IDL_NODE_TYPE_NAME(type));
			abort();
	}
}


/* TODO: make value_type() return a tmp string; this requires updates to all
 * callsites however, since the pointer must not be passed to g_free()
 * afterward.
 *
 * the call-site update can be accomplished by replacing ns with struct
 * print_ctx *pr (which is required for tmpstrings anyhow), and looking at the
 * compiler's error messages.
 *
 * and the same should be done for rigid_type() and the like, too. maybe move
 * them into types.c or some such for good measure.
 */
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
				else if(IS_TIME_TYPE(type)) return g_strdup("L4_Time_t");
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
		case IDLN_EXCEPT_DCL: ident = IDL_EXCEPT_DCL(node).ident; break;
		case IDLN_CONST_DCL: ident = IDL_CONST_DCL(node).ident; break;

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


IDL_tree get_array_type(IDL_tree type)
{
	assert(IDL_NODE_TYPE(type) == IDLN_TYPE_ARRAY);
	IDL_tree parent = type->up;
	while(parent != NULL && IDL_NODE_TYPE(parent) == IDLN_LIST) {
		parent = parent->up;
	}
	if(parent == NULL) {
		fprintf(stderr, "%s: array without parent? what.\n", __func__);
		abort();
	}
	switch(IDL_NODE_TYPE(parent)) {
		case IDLN_TYPE_DCL:
			return get_type_spec(IDL_TYPE_DCL(parent).type_spec);

		default:
			/* FIXME where the node type is something that declares typed
			 * members.
			 */
			NOTDEFINED(parent);
	}
}


bool is_rigid_type(IDL_ns ns, IDL_tree type)
{
	if(type == NULL) return false;	/* void is not a rigid type either. */
	switch(IDL_NODE_TYPE(type)) {
		default: return is_value_type(type);
		case IDLN_TYPE_STRUCT: return is_struct_rigid(ns, type);

		case IDLN_TYPE_ARRAY:
			/* is rigid if the element type is. */
			return is_rigid_type(ns, get_array_type(type));

		case IDLN_TYPE_UNION:
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

		case IDLN_TYPE_ARRAY: {
			IDL_tree subtype = get_array_type(type);
			if(is_value_type(subtype)) return value_type(ns, subtype);
			else if(is_rigid_type(ns, subtype)) {
				return rigid_type(ns, subtype);
			} else {
				NOTDEFINED(subtype);
			}
		}

		case IDLN_TYPE_UNION:
			/* TODO */

		default:
			if(is_value_type(type)) return value_type(ns, type);
			else NOTDEFINED(type);
	}
}


static bool has_timeouts(IDL_tree op)
{
	static const char *const kinds[] = {
		"StubSendTimeout", "StubRecvTimeout",
		/* TODO: add xfer timeout also */
	};
	IDL_tree p = IDL_OP_DCL(op).ident;
	for(int i=0; i<G_N_ELEMENTS(kinds); i++) {
		if(IDL_tree_property_get(p, kinds[i]) != NULL) return true;
	}
	return false;
}


char *return_type(
	IDL_ns ns,
	IDL_tree op,
	bool *real_p,
	bool for_vtable)
{
	bool spare_bool;
	if(real_p == NULL) real_p = &spare_bool;

	IDL_tree op_type = get_type_spec(IDL_OP_DCL(op).op_type_spec);
	if(find_neg_exn(op) != NULL || (!for_vtable && has_timeouts(op))) {
		*real_p = is_real_nre_return_type(op_type);
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
				*ret = g_strdup_printf("const struct %s *", sname);
			g_free(sname);
			return ret;
		}

		case IDLN_TYPE_STRING: return g_strdup("const char *");
		case IDLN_TYPE_WIDE_STRING: return g_strdup("const wchar_t *");

		case IDLN_TYPE_ARRAY: {
			char *tname = rigid_type(ns, tree),
				*ret = g_strdup_printf("const %s *", tname);
			g_free(tname);
			return ret;
		}

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


void indent(struct print_ctx *pr, int change)
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


void close_brace(struct print_ctx *pr)
{
	indent(pr, -1);
	code_f(pr, "}");
}


void print_msg_encoder(
	struct print_ctx *pr,
	const struct message_info *msg,
	const char *retval_str,
	const char *msg_str,
	const char *var_prefix)
{
	if(var_prefix == NULL) var_prefix = "";

	int next_r = 1;
	if(retval_str != NULL) {
		code_f(pr, "L4_MsgAppendWord(%s, %s);", msg_str, retval_str);
		next_r++;
	}

	GLIST_FOREACH(cur_u, msg->untyped) {
		const struct msg_param *u = cur_u->data;
		IDL_tree type = u->X.untyped.type;
		bool short_type = IS_FPAGE_TYPE(type) || is_value_type(type);
		if(short_type && u->X.untyped.first_reg != next_r) {
			fprintf(stderr, "warning: next reg# is %d, but first_reg is %d (opdcl `%s')\n",
				next_r, (int)u->X.untyped.first_reg, u->name);
		}

		if(IS_FPAGE_TYPE(type)) {
			code_f(pr, "L4_MsgAppendWord(%s, %s%s.raw);", msg_str,
				var_prefix, u->name);
			next_r++;
		} else if(is_value_type(type)) {
			code_f(pr, "L4_MsgAppendWord(%s, %s%s);", msg_str, var_prefix,
				u->name);
			next_r++;
		} else {
			/* TODO: handle mapitems, rigid types, and long types
			 * as out-values too
			 */
			NOTDEFINED(type);
		}
	}

	/* inline sequences */
	GLIST_FOREACH(cur_s, msg->seq) {
		const struct msg_param *s = cur_s->data;
		const char *len_lvalue = seq_len_lvalue(pr, s->param_dcl, var_prefix,
			s->name, true);
		if(g_list_next(cur_s) != NULL) {
			/* not the last; encode a length word. */
			code_f(pr, "L4_MsgAppendWord(%s, %s);", msg_str, len_lvalue);
		}
		const int epw = s->X.seq.elems_per_word, bpe = s->X.seq.bits_per_elem;
		code_f(pr, "for(unsigned i=0, _l=%s; i<_l; i+=%d) {", len_lvalue,
			epw);
		indent(pr, 1);
		if(bpe == BITS_PER_WORD) {
			/* full word case. */
			code_f(pr, "L4_MsgAppendWord(%s, %s%s[i]);", msg_str,
				var_prefix, s->name);
		} else {
			/* shifting bit-pack case */
			uint64_t mask;
			if(bpe >= 64) mask = ~UINT64_C(0);
			else mask = (UINT64_C(1) << bpe) - 1;
			code_f(pr, "L4_Word_t t = 0;");
			for(int j=0; j<epw; j++) {
				if(j > 0) code_f(pr, "t <<= %d;", bpe);
				code_f(pr, "t |= %s%s[i * %d + %d] & UINT%d_C(%#llx);",
					var_prefix, s->name, epw, j, BITS_PER_WORD,
					(unsigned long long)mask);
			}
			code_f(pr, "L4_MsgAppendWord(%s, t);", msg_str);
		}
		close_brace(pr);
	}

	/* long items */
	GLIST_FOREACH(cur_l, msg->_long) {
		const struct msg_param *p = cur_l->data;
		IDL_tree type = p->X._long.type;
		const char *len_expr;
		if(IDL_NODE_TYPE(type) == IDLN_TYPE_STRING) {
			len_expr = tmp_f(pr, "strlen(%s%s)", var_prefix, p->name);
		} else if(IDL_NODE_TYPE(type) == IDLN_TYPE_WIDE_STRING) {
			len_expr = tmp_f(pr, "wcslen(%s%s) * sizeof(wchar_t)",
				var_prefix, p->name);
		} else if(IDL_NODE_TYPE(type) == IDLN_TYPE_SEQUENCE) {
			/* FIXME (or just remove in the LLVM branch) */
			code_f(pr, "/* would do something with sequences, but don't know what */");
			len_expr = "66642";
		} else {
			/* FIXME: set len_expr to byte size of each element of type,
			 * times one for structs and unions and times the length indication
			 * (static for arrays, variable for sequences).
			 */
			NOTDEFINED(type);
		}
		code_f(pr, "L4_MsgAppendSimpleStringItem(%s, L4_StringItem(%s, (char *)%s%s));",
			msg_str, len_expr, var_prefix, p->name);
	}
}


/* generic decoding of inline sequences. used by both dispatchers and
 * stubs.
 */
void print_decode_inline_seqs(
	struct print_ctx *pr,
	const struct message_info *req,
	const char *msgstr,
	const char *var_prefix)
{
	if(req->seq == NULL) return;

	code_f(pr, "int seq_base = %d;", req->tag_u);
	GLIST_FOREACH(cur_s, req->seq) {
		const struct msg_param *s = cur_s->data;
		const int epw = s->X.seq.elems_per_word, bpe = s->X.seq.bits_per_elem;
		const char *len_lvalue = seq_len_lvalue(pr, s->param_dcl,
			var_prefix, s->name, false);
		if(g_list_next(cur_s) != NULL) {
			/* not the last, therefore take a length word. */
			code_f(pr, "%s = L4_MsgWord(%s, seq_base++);",
				len_lvalue, msgstr);
		} else {
			/* compute length from untyped words counter. */
			code_f(pr, "%s = L4_UntypedWords((%s)->tag) - seq_base;",
				len_lvalue, msgstr);
		}

		/* NOTE: I'm quite sure this could be done smarter. but w/e...
		 *
		 * FIXME: make sure that when epw > 1, p_[name] is sized to accept a
		 * multiple of epw items, padding it out as necessary.
		 */
		code_f(pr, "for(unsigned i=0; i<%s; i+=%d) {", len_lvalue, epw);
		indent(pr, 1);
		if(bpe == BITS_PER_WORD) {
			/* full word case. */
			code_f(pr, "%s%s[i] = L4_MsgWord(%s, seq_base++);",
				var_prefix, s->name, msgstr);
		} else {
			/* masked-shift bit-packing case. */
			uint64_t mask;
			if(bpe >= 64) mask = ~UINT64_C(0);
			else mask = (UINT64_C(1) << bpe) - 1;
			code_f(pr, "L4_Word_t w = L4_MsgWord(%s, seq_base++);", msgstr);
			for(int j=0; j<epw; j++) {
				code_f(pr, "%s%s[i * %d + %d] = w & UINT%d_C(%#llx); w >>= %d;",
					var_prefix, s->name, epw, epw - j - 1, BITS_PER_WORD,
					(unsigned long long)mask, bpe);
			}
		}
		close_brace(pr);
	}
}


char *dispatcher_name(IDL_ns ns, IDL_tree iface, char **vtprefix_p)
{
	char *vtprefix = vtable_prefix(ns, iface),
		*ret = g_strdup_printf("_muidl_%s_dispatch", vtprefix);
	if(vtprefix_p != NULL) *vtprefix_p = vtprefix; else g_free(vtprefix);
	return ret;
}


/* FIXME: return errors somehow */
static void print_into(
	const char *filename,
	void (*prtfn)(struct print_ctx *),
	volatile struct print_ctx *pr)
{
	char *path;
	if(arg_dest_path == NULL) path = g_strdup(filename);
	else path = g_build_path("/", arg_dest_path, filename, NULL);

	FILE *f = fopen(path, "wb");
	if(f == NULL) {
		fprintf(stderr, "can't open `%s' for writing: %s\n", path,
			strerror(errno));
		exit(EXIT_FAILURE);
	}
	g_free(path);

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
	g_string_chunk_clear(pr->tmpstrchunk);
}


static LLVMModuleRef make_llvm_module(
	struct llvm_ctx *ctx,
	const char *basename,
	IDL_tree_func treefn)
{
	LLVMModuleRef mod = LLVMModuleCreateWithNameInContext(basename, ctx->ctx);
	LLVMSetTarget(mod, "i486-linux-gnu");
	ctx->module = mod;

	/* reset per-module context things */
	g_hash_table_remove_all(ctx->struct_decoder_fns);
	assert(ctx->malloc_ptrs == NULL);

	IDL_tree_walk_in_order(ctx->pr->tree, treefn, ctx);

	char *outmsg = NULL;
	if(LLVMVerifyModule(mod, LLVMPrintMessageAction, &outmsg)) {
		fprintf(stderr, "LLVM module verifier complained!\n");
		LLVMDumpModule(mod);
		abort();
		LLVMDisposeMessage(outmsg);
	}
	if(arg_dump_llvm) {
		LLVMDumpModule(mod);
	}

	ctx->module = NULL;
	return mod;
}


static void compile_module_to_asm(LLVMModuleRef mod, const char *filename)
{
	char *cmd = g_strdup_printf("llc-2.7 -O2 -filetype=asm -o %s", filename);
	FILE *p = popen(cmd, "w");
	g_free(cmd);
	if(p == NULL) {
		fprintf(stderr, "can't open pipe to llc-2.7: %s\n", strerror(errno));
		exit(EXIT_FAILURE);
	}

	int n = LLVMWriteBitcodeToFD(mod, fileno(p), 0, 0);
	if(n != 0) {
		fprintf(stderr, "bitcode writing failed; removing the file\n");
		unlink(filename);
		exit(EXIT_FAILURE);
	}

	pclose(p);
}


bool do_idl_file(const char *cppopts, const char *filename)
{
	IDL_tree tree = NULL;
	IDL_ns ns = NULL;

	reset_warn_once();

	int n = IDL_parse_filename(filename, cppopts, &msg_callback,
		&tree, &ns, IDLF_PROPERTIES | IDLF_XPIDL | IDLF_SHOW_CPP_ERRORS
			| IDLF_COMBINE_REOPENED_MODULES
			| (arg_verbose_idl ? IDLF_VERBOSE : 0)
			| IDLF_INHIBIT_INCLUDES,
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
		.tmpstrchunk = g_string_chunk_new(1024),
	};

	struct llvm_ctx *lc = create_llvm_ctx(&print_ctx);

	if(!arg_service_only && !arg_client_only) {
		print_into(commonname, &print_common_header, &print_ctx);

		LLVMModuleRef mod = make_llvm_module(lc, basename,
			&iter_build_common_module);
		compile_module_to_asm(mod, tmp_f(&print_ctx, "%s-common.S",
			basename));
		LLVMDisposeModule(mod);
	}
	if(!arg_defs_only && !arg_client_only) {
		LLVMModuleRef mod = make_llvm_module(lc, basename,
			&iter_build_dispatchers);
		compile_module_to_asm(mod, tmp_f(&print_ctx, "%s-service.S",
			basename));
		LLVMDisposeModule(mod);
	}
	if(!arg_defs_only && !arg_service_only) {
		LLVMModuleRef mod = make_llvm_module(lc, basename,
			&iter_build_stubs);
		compile_module_to_asm(mod, tmp_f(&print_ctx, "%s-client.S",
			basename));
		LLVMDisposeModule(mod);
	}

	g_string_chunk_free(print_ctx.tmpstrchunk);
	g_hash_table_destroy(ifaces);
	g_free(commonname);

	dispose_llvm_ctx(lc);

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
		{ "destination-path", 'd', 0, G_OPTION_ARG_STRING,
		  &arg_dest_path, "produce files in PATH", "PATH" },
		{ "dump-llvm", 0, 0, G_OPTION_ARG_NONE, &arg_dump_llvm,
		  "dump LLVM assembly to stderr (for debugging)", NULL },
		{ "defs-only", 0, 0, G_OPTION_ARG_NONE, &arg_defs_only,
		  "only produce `-defs.h'", NULL },
		{ "client-only", 0, 0, G_OPTION_ARG_NONE, &arg_client_only,
		  "only produce `-client.c'", NULL },
		{ "service-only", 0, 0, G_OPTION_ARG_NONE, &arg_service_only,
		  "only produce `-service.c'", NULL },
		{ "verbose", 'v', 0, G_OPTION_ARG_NONE, &arg_verbose,
		  "enable verbose output", NULL },
		{ "verbose-idl", 0, 0, G_OPTION_ARG_NONE, &arg_verbose_idl,
		  "enable verbose output from libIDL", NULL },
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

	int only = (arg_defs_only ? 1 : 0) + (arg_client_only ? 1 : 0)
		+ (arg_service_only ? 1 : 0);
	if(only > 1) {
		fprintf(stderr, "only one `--X-only' option may be given at a time.\n");
		exit(EXIT_FAILURE);
	}

	if(arg_dest_path != NULL
		&& !g_file_test(arg_dest_path, G_FILE_TEST_IS_DIR))
	{
		fprintf(stderr, "destination `%s' doesn't look like a directory",
			arg_dest_path);
		exit(EXIT_FAILURE);
	}
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
		printf("muidl (µidl) version 0.1\n"
			"(compiled for libIDL version %d.%d.%d)\n",
			LIBIDL_MAJOR_VERSION, LIBIDL_MINOR_VERSION,
			LIBIDL_MICRO_VERSION);
		return EXIT_SUCCESS;
	}

	/* combined module disinhibition first appears in libIDL 0.8.14;
	 * if it gets into Debian testing before µidl is released, remove this
	 * part.
	 */
	if(LIBIDL_VERSION_CODE < LIBIDL_GEN_VERSION(0, 8, 14)) {
		fprintf(stderr, "warning: libIDL version is earlier than 0.8.14. "
			"spurious module inhibits may occur.\n");
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

#ifndef NDEBUG
	g_free(opts);
	g_strfreev(arg_defines);
	g_strfreev(arg_idl_files);
	g_strfreev(arg_include_paths);
#endif

	return ok ? EXIT_SUCCESS : EXIT_FAILURE;
}
