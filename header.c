/*
 * header.c -- common header file generation
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
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <libIDL/IDL.h>

#include "muidl.h"


static void print_generic_stub_decl(
	struct print_ctx *pr,
	const char *stubpfx,
	IDL_tree op,
	int tok);		/* timeout kind mask */


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
		fprintf(of, "%s *%s_buf, unsigned *%s_len_p", b, name, name);
	} else if(is_value_type(type)) {
		b = value_type(ns, type);
		fprintf(of, "%s *%s_ptr", b, name);
	} else if(IS_MAPGRANT_TYPE(type)) {
		/* FIXME: this ignores GrantItems, but so does the IDL form. this will
		 * make things a bit ugly wherever grant items are passed, and should
		 * be considered more deeply whenever grant items are actually
		 * required.
		 *
		 * it's not like L4.X2 discriminates between the two when receiving
		 * mappings anyhow.
		 */
		fprintf(of, "L4_MapItem_t *%s_ptr", name);
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
	GLIST_FOREACH(cur, methods) {
		IDL_tree op = cur->data;

		char *rettypstr = return_type(ns, op, NULL, true),
			*name = decapsify(METHOD_NAME(op));
		fprintf(of, "\t%s%s(*%s)(", rettypstr, type_space(rettypstr), name);
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
					char *typestr;
					if(IDL_NODE_TYPE(type) == IDLN_TYPE_SEQUENCE) {
						IDL_tree subtype = get_type_spec(
							IDL_TYPE_SEQUENCE(type).simple_type_spec);
						typestr = in_param_type(ns, subtype);
						fprintf(of, "const %s%s*%s%s, ",
							typestr, type_space(typestr),
							is_reserved_word(name) ? "_" : "", name);
						fprintf(of, "unsigned %s%s_len",
							is_reserved_word(name) ? "_" : "", name);
					} else {
						/* TODO: arrays, strings, structs */
						typestr = in_param_type(ns, type);
						fprintf(of, "%s%s%s%s", typestr, type_space(typestr),
							is_reserved_word(name) ? "_" : "", name);
					}
					g_free(typestr);
					break;
				}

				/* in-out parameters are passed exactly as out-parameters,
				 * but with a value already present.
				 */
				case IDL_PARAM_OUT:
				case IDL_PARAM_INOUT:
					print_out_param(of, ns, type, name);
					break;
			}
		}

		fprintf(of, ");\n");
	}
	fprintf(of, "};\n");
	g_list_free(methods);
}


static void print_exn_raisers(struct print_ctx *pr, IDL_tree iface)
{
	GHashTable *ex_repo_ids = g_hash_table_new_full(&g_str_hash, &g_str_equal,
		&g_free, NULL);
	collect_exceptions(pr->ns, ex_repo_ids, iface);

	/* output preprocessor-guarded externs for each, except the ones that are
	 * raised by negative return value.
	 */
	GHashTableIter iter;
	g_hash_table_iter_init(&iter, ex_repo_ids);
	gpointer key = NULL, value = NULL;
	bool first = true;
	while(g_hash_table_iter_next(&iter, &key, &value)) {
		const char *repo_id = key;
		IDL_tree exn = value;
		if(is_negs_exn(exn)) continue;

		if(first) {
			first = false;
			code_f(pr, "\n/* exception raisers */\n");
		}
		char *mangled = mangle_repo_id(repo_id);
		char *def = g_strdup_printf("MUIDL_%s_RAISER_EXTERN", mangled);
		code_f(pr, "#ifndef %s", def);
		code_f(pr, "#define %s 1", def);
		code_f(pr, "/* for `%s' */", repo_id);
		char *raiser_name = exn_raise_fn_name(exn);
		code_f(pr, "extern void %s(%s", raiser_name,
			IDL_EXCEPT_DCL(exn).members == NULL ? "void);" : "");
		indent(pr, 1);
		for(IDL_tree cur = IDL_EXCEPT_DCL(exn).members;
			cur != NULL;
			cur = IDL_LIST(cur).next)
		{
			const char *m_suffix = IDL_LIST(cur).next == NULL ? ");" : ",";
			IDL_tree member = IDL_LIST(cur).data,
				mtype = get_type_spec(IDL_MEMBER(member).type_spec);
			for(IDL_tree d_cur = IDL_MEMBER(member).dcls;
				d_cur != NULL;
				d_cur = IDL_LIST(d_cur).next)
			{
				IDL_tree decl = IDL_LIST(d_cur).data;
				IDL_tree name;
				if(IDL_NODE_TYPE(decl) == IDLN_TYPE_ARRAY) {
					name = IDL_TYPE_ARRAY(decl).ident;
				} else if(IDL_NODE_TYPE(decl) == IDLN_IDENT) {
					name = decl;
				} else {
					/* FIXME: what are these anyway */
					NOTDEFINED(decl);
				}
				/* FIXME: output the correct type w/pointer when array */
				code_f(pr, "[%s] %s%s", IDL_NODE_TYPE_NAME(mtype),
					IDL_IDENT(name).str,
					IDL_LIST(d_cur).next == NULL ? m_suffix : ",");
			}
		}
		indent(pr, -1);

		code_f(pr, "#endif /* %s */", def);

		g_free(def);
		g_free(mangled);
		g_free(raiser_name);
	}

	g_hash_table_destroy(ex_repo_ids);
}


static void print_extern_prototype(
	struct print_ctx *pr,
	const char *stubpfx,
	IDL_tree op_dcl,
	int tok)
{
	fprintf(pr->of, "extern ");
	print_generic_stub_decl(pr, stubpfx, op_dcl, tok);
	int n = fseek(pr->of, -1, SEEK_CUR);
	if(n != 0) {
		/* FIXME: yeah, get us a variant of code_f() that can be told to skip
		 * newlines. this'll do until then.
		 */
		perror("fseek");
		exit(EXIT_FAILURE);
	}
	fprintf(pr->of, ";\n\n");
}


static char *fixed_type(IDL_ns ns, IDL_tree type)
{
	if(is_value_type(type)) return value_type(ns, type);
	else if(is_rigid_type(ns, type)) return rigid_type(ns, type);
	else NOTDEFINED(type);
}


static char *stub_name(
	struct print_ctx *pr,
	const char *stubpfx,
	IDL_tree op,
	int tok)
{
	char *name = decapsify(METHOD_NAME(op));
	char *ret = tmp_f(pr, "%s%s",
			stubpfx == NULL ? name : tmp_f(pr, "%s_%s", stubpfx, name),
			tok != 0 ? "_timeout" : "");
	g_free(name);
	return ret;
}


typedef void (*param_fn)(
	struct print_ctx *pr,
	int param_index,		/* first being 0, second 1 etc */
	const char *c_type,
	const char *name,
	bool is_last);


/* returns number of parameters seen */
static int each_stub_parameter(
	struct print_ctx *pr,
	IDL_tree op,
	int tok,		/* timeout kind mask */
	param_fn paramfn)
{
	assert((tok & ~(TIMEOUT_SEND | TIMEOUT_RECV)) == 0);
	int pnum = 0;	/* just a counter for the callback */
	IDL_tree params = IDL_OP_DCL(op).parameter_dcls,
		op_type = get_type_spec(IDL_OP_DCL(op).op_type_spec);

	if(!has_pager_target(pr->ns, op)) {
		(*paramfn)(pr, pnum++, "L4_ThreadId_t", "_service_tid",
			tok == 0 && params == NULL && op_type == NULL);
	}

	if(op_type != NULL) {
		assert(!IDL_OP_DCL(op).f_oneway);
		char *typ = rigid_type(pr->ns, op_type),
			*ptrtyp = g_strconcat(typ, " *", NULL);
		(*paramfn)(pr, pnum++, ptrtyp, "_retval_p",
			tok == 0 && params == NULL);
		g_free(typ);
		g_free(ptrtyp);
	}

	for(IDL_tree cur = params; cur != NULL; cur = IDL_LIST(cur).next) {
		IDL_tree param = IDL_LIST(cur).data;
		enum IDL_param_attr attr = IDL_PARAM_DCL(param).attr;
		bool is_last = tok == 0 && IDL_LIST(cur).next == NULL,
			in_only = attr == IDL_PARAM_IN;
		IDL_tree type = get_type_spec(
				IDL_PARAM_DCL(param).param_type_spec),
			decl = IDL_PARAM_DCL(param).simple_declarator;
		const char *name = IDL_IDENT(decl).str;
		const char *prefix;
		if(name[0] == '_' && name[1] == '_') prefix = ""; else prefix = "p_";
		char *c_name = tmp_f(pr, "%s%s", prefix, name);

		char *tmpstr = NULL;
		if(attr == IDL_PARAM_IN && is_value_type(type)) {
			tmpstr = value_type(pr->ns, type);
			(*paramfn)(pr, pnum++, tmpstr, c_name, is_last);
		} else if(is_rigid_type(pr->ns, type)) {
			tmpstr = fixed_type(pr->ns, type);
			(*paramfn)(pr, pnum++,
				tmp_f(pr, "%s%s *", in_only ? "const " : "",
					tmpstr), c_name, is_last);
		} else if(IDL_NODE_TYPE(type) == IDLN_TYPE_SEQUENCE) {
			IDL_tree subtype = get_type_spec(
				IDL_TYPE_SEQUENCE(type).simple_type_spec);
			tmpstr = fixed_type(pr->ns, subtype);
			(*paramfn)(pr, pnum++,
				tmp_f(pr, "%s%s *", in_only ? "const " : "", tmpstr),
				c_name, false);
			(*paramfn)(pr, pnum++,
				tmp_f(pr, "unsigned int%s", !in_only ? " *" : ""),
				tmp_f(pr, "%s_len%s", c_name, !in_only ? "_ptr" : ""),
				is_last);
		} else if(IDL_NODE_TYPE(type) == IDLN_TYPE_STRING) {
			(*paramfn)(pr, pnum++, in_only ? "const char *" : "char *",
				c_name, is_last);
		} else if(IDL_NODE_TYPE(type) == IDLN_TYPE_WIDE_STRING) {
			(*paramfn)(pr, pnum++, in_only ? "const wchar_t *" : "wchar_t *",
				c_name, is_last);
		} else {
			/* TODO: handle structs, unions, arrays, etc */
			NOTDEFINED(type);
		}
		g_free(tmpstr);
	}
	if(tok != 0) {
		/* add timeout parameters. */
		static const char *to_names[] = {
			"__send_timeout", "__recv_timeout",
		};
		for(int i=0; i<2; i++) {
			if((tok & (1 << i)) == 0) continue;
			(*paramfn)(pr, pnum++, "L4_Time_t", to_names[i],
				tok >> (i + 1) == 0);
		}

	}
	return pnum;
}


static void print_generic_stub_decl(
	struct print_ctx *pr,
	const char *stubpfx,
	IDL_tree op,
	int tok)		/* timeout kind mask */
{
	char *stubhead = tmp_f(pr, "int %s", stub_name(pr, stubpfx, op, tok));

	/* GCC rules teh skies */
	void each_param(
		struct print_ctx *pr,
		int param_ix,
		const char *c_type,
		const char *name,
		bool is_last)
	{
		if(param_ix == 0) {
			code_f(pr, "%s(%s%s%s%c", stubhead, c_type, type_space(c_type),
				name, is_last ? ')' : ',');
			if(!is_last) indent(pr, 1);
		} else {
			code_f(pr, "%s%s%s%c", c_type, type_space(c_type), name,
				is_last ? ')' : ',');
			if(is_last) indent(pr, -1);
		}
	};

	int n = each_stub_parameter(pr, op, tok, &each_param);
	if(n == 0) code_f(pr, "%s(void)", stubhead);
}


char *get_stub_prefix(IDL_tree opdcl)
{
	const char *stubpfx = IDL_tree_property_get(
		IDL_OP_DCL(opdcl).ident, "StubPrefix");
	if(stubpfx == NULL) {
		IDL_tree iface = IDL_get_parent_node(opdcl, IDLN_INTERFACE, NULL);
		if(iface != NULL) {
			stubpfx = IDL_tree_property_get(IDL_INTERFACE(iface).ident,
				"StubPrefix");
		}
	}
	return g_strdup(stubpfx);
}


static gboolean print_stub_protos(IDL_tree_func_data *tf, gpointer userdata)
{
	struct print_ctx *pr = userdata;
	switch(IDL_NODE_TYPE(tf->tree)) {
		default: return FALSE;

		case IDLN_LIST:
		case IDLN_MODULE:
		case IDLN_SRCFILE:
		case IDLN_INTERFACE:
			return TRUE;

		case IDLN_OP_DCL:
			/* whee! */
			break;
	}

	IDL_tree opdcl = tf->tree;

	char *stubpfx = get_stub_prefix(opdcl);
	int tok = op_timeout_kind(opdcl);
	if(tok != 0) print_extern_prototype(pr, stubpfx, tf->tree, tok);
	/* (TODO: should there be an attribute for not generating the "wait
	 * forever" default stub?)
	 */
	print_extern_prototype(pr, stubpfx, tf->tree, 0);
	g_free(stubpfx);

	/* looked at the op dcl already. */
	return FALSE;
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

	const bool packed = is_packed(tf->tree);
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

			/* and an array bound suffix, where applicable. */
			IDL_tree bound = NULL;
			int extra = 0;
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

				case IDLN_TYPE_STRUCT:
					break;

				default:
					NOTDEFINED(type);
			}
			if(bound != NULL) {
				max_size = IDL_INTEGER(bound).value;
				suffix = g_strdup_printf("[%lu]", max_size + extra);
			}
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


static gboolean print_consts(IDL_tree_func_data *tf, gpointer userdata)
{
	struct print_ctx *pr = userdata;

	/* we'll emit constant declarations found in interfaces and modules only.
	 * others needn't apply.
	 */
	switch(IDL_NODE_TYPE(tf->tree)) {
		default: return FALSE;

		case IDLN_MODULE:
		case IDLN_INTERFACE:
			return TRUE;

		/* (basic container stuff) */
		case IDLN_LIST:
		case IDLN_GENTREE:
			return TRUE;

		case IDLN_CONST_DCL:
			break;
	}

	char *ln = long_name(pr->ns, tf->tree);
	for(int i=0; ln[i] != '\0'; i++) ln[i] = toupper(ln[i]);
	IDL_tree value = IDL_resolve_const_exp(IDL_CONST_DCL(tf->tree).const_exp,
		IDLN_ANY);
	char *val;
	switch(IDL_NODE_TYPE(value)) {
		case IDLN_INTEGER:
			val = g_strdup_printf("%lld", (long long)IDL_INTEGER(value).value);
			break;

		case IDLN_STRING: {
			char *tmp = g_strescape(IDL_STRING(value).value, NULL);
			val = g_strconcat("\"", tmp, "\"", NULL);
			g_free(tmp);
			break;
		}

		case IDLN_CHAR: {
			char c = *IDL_CHAR(value).value;
			val = g_strdup_printf(isprint(c) ? "'%c'" : "'\\%o'", c);
			break;
		}

		case IDLN_BOOLEAN:
			val = g_strdup(IDL_BOOLEAN(value).value ? "true" : "false");
			break;

		case IDLN_FLOAT:
			val = g_strdup_printf("%f", IDL_FLOAT(value).value);
			break;

		/* TODO: */
		case IDLN_FIXED:
		case IDLN_WIDE_CHAR:
		case IDLN_WIDE_STRING:

		default:
			fprintf(stderr, "%s: can't handle constant type <%s>!\n",
				__FUNCTION__, IDL_NODE_TYPE_NAME(tf->tree));
			/* FIXME: fail softly */
			abort();
	}
	code_f(pr, "#define %s %s", ln, val);
	g_free(ln);
	g_free(val);

	return FALSE;
}


void print_common_header(struct print_ctx *pr)
{
	print_file_heading(pr);

	/* include guard */
	char *upper = g_utf8_strup(pr->common_header_name, -1);
	/* (assume it's valid utf8 after strup.) */
	for(char *p = upper; *p != '\0'; p = g_utf8_next_char(p)) {
		if(g_ascii_ispunct(*p)) *p = '_';
	}
	code_f(pr, "#ifndef _MUIDL_%s\n#define _MUIDL_%s\n", upper, upper);

	/* headers */
	static const char *hdrfiles[] = {
		"stdint.h",
		"stdbool.h",
		"kernel/types.h",
		"l4/types.h",
	};
	print_headers(pr, hdrfiles, G_N_ELEMENTS(hdrfiles));

	/* constant declarations. */
	IDL_tree_walk_in_order(pr->tree, &print_consts, pr);
	code_f(pr, " ");

	/* struct, union & enum declarations as they appear in the IDL source. this
	 * is appropriate because IDL doesn't permit forward declaration of
	 * structs, unions, enums, or typedefs.
	 */
	IDL_tree_walk_in_order(pr->tree, &print_struct_decls, pr);
	code_f(pr, " ");

	/* stub prototypes. */
	IDL_tree_walk_in_order(pr->tree, &print_stub_protos, pr);
	code_f(pr, " ");

	/* interface vtables and dispatcher prototypes, but only for service
	 * implementations (so as to avoid polluting the namespace).
	 */
	if(g_hash_table_size(pr->ifaces) > 0) {
		GHashTableIter iter;
		g_hash_table_iter_init(&iter, pr->ifaces);
		gpointer key, value;
		while(g_hash_table_iter_next(&iter, &key, &value)) {
			IDL_tree iface = (IDL_tree)value,
				mod = IDL_get_parent_node(iface, IDLN_MODULE, NULL);

			/* vtable etc. selector */
			char *modpfx = NULL, *ifpfx;
			if(mod != NULL) {
				modpfx = g_utf8_strup(IDL_IDENT(IDL_MODULE(mod).ident).str,
					-1);
			}
			ifpfx = g_utf8_strup(IDL_IDENT(IDL_INTERFACE(iface).ident).str,
				-1);
			code_f(pr, "#if defined(%s%s%s_IMPL_SOURCE) || defined(MUIDL_SOURCE)",
				modpfx == NULL ? " " : modpfx, mod == NULL ? "" : "_",
				ifpfx);
			g_free(modpfx);
			g_free(ifpfx);

			/* vtable declaration */
			code_f(pr, "\n/* vtable for `%s': */", (const char *)key);
			print_vtable(pr->of, pr->tree, pr->ns, iface);

			/* dispatcher prototype */
			code_f(pr, " ");
			char *vtprefix = NULL,
				*dispname = dispatcher_name(pr->ns, iface, &vtprefix);
			code_f(pr, "extern L4_Word_t %s(", dispname);
			g_free(dispname);
			indent(pr, 1);
			code_f(pr, "const struct %s_vtable *vtable);", vtprefix);
			indent(pr, -1);
			g_free(vtprefix);

			/* exception raisers */
			print_exn_raisers(pr, iface);

			/* close off vtable selector */
			code_f(pr, "\n#endif\n");
		}
	}

	code_f(pr, "#endif");
	g_free(upper);
}
