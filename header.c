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
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <ccan/htable/htable.h>
#include <ccan/hash/hash.h>
#include <ccan/str/str.h>
#include <ccan/talloc/talloc.h>
#include <libIDL/IDL.h>

#include "defs.h"


static void print_generic_stub_decl(
	struct print_ctx *pr,
	const char *stubpfx,
	IDL_tree op,
	int tok);		/* timeout kind mask */


/* returns the C type of value types, and the void type.
 *
 * TODO: make value_type() return a tmp string; this requires updates to all
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
static char *value_type(IDL_ns ns, IDL_tree type)
{
	if(type == NULL) return g_strdup("void");
	assert(is_value_type(type));
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

		case IDLN_TYPE_ARRAY: {
			IDL_tree subtype = get_array_type(type);
			if(is_value_type(subtype)) return value_type(ns, subtype);
			else if(is_rigid_type(subtype)) {
				return rigid_type(ns, subtype);
			} else {
				NOTDEFINED(subtype);
			}
		}

		case IDLN_NATIVE:
			if(IS_MAPGRANT_TYPE(type)) {
				return g_strdup("L4_MapItem_t");
			} else {
				return value_type(ns, type);
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
	if(find_exn(op, &is_negs_exn) != NULL || (!for_vtable && has_timeouts(op))) {
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

		case IDLN_NATIVE:
			if(IS_MAPGRANT_TYPE(tree)) {
				return g_strdup("L4_MapItem_t *");
			}
			/* FALL THRU */

		case IDLN_TYPE_UNION:
			/* TODO */

		default:
			NOTDEFINED(tree);
	}
}


static void print_out_param(
	FILE *of,
	IDL_ns ns,
	IDL_tree type,
	const char *name)
{
	char *b = NULL;
	if(IDL_NODE_TYPE(type) == IDLN_TYPE_SEQUENCE) {
		IDL_tree sub = SEQ_SUBTYPE(type);
		/* TODO: are there other rigid types that are already passed by
		 * reference? unions?
		 */
		if(IDL_NODE_TYPE(sub) != IDLN_TYPE_STRUCT) {
			b = in_param_type(ns, sub);
		} else {
			char *sname = long_name(ns, sub);
			b = g_strdup_printf("struct %s", sname);
			g_free(sname);
		}
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
	struct iface_info *iface)
{
	char *vtpfx = vtable_prefix(ns, iface->node);
	fprintf(of, "struct %s_vtable\n{\n", vtpfx);
	g_free(vtpfx);

	const int num_ops = g_list_length(iface->ops);
	const struct method_info *vtab_ops[num_ops];
	for(int i=0; i<num_ops; i++) vtab_ops[i] = NULL;
	GLIST_FOREACH(cur, iface->ops) {
		const struct method_info *op = cur->data;
		assert(op->vtab_offset < num_ops);
		vtab_ops[op->vtab_offset] = op;
	}

	for(int vtix = 0; vtix < num_ops; vtix++) {
		const struct method_info *op = vtab_ops[vtix];
		if(op == NULL) {
			g_error("%s:%d: vtab_ops[%d] not assigned by iface->ops",
				__func__, __LINE__, vtix);
		}

		char *rettypstr = return_type(ns, op->node, NULL, true),
			*name = decapsify(METHOD_NAME(op->node));
		fprintf(of, "\t%s%s(*%s)(", rettypstr, type_space(rettypstr), name);
		g_free(rettypstr);
		g_free(name);

		bool first_param = true;
		IDL_tree rettyp = get_type_spec(IDL_OP_DCL(op->node).op_type_spec);
		if(rettyp != NULL && !is_value_type(rettyp)) {
			/* compound type return values are declared as the first parameter,
			 * because they're on the "left side".
			 */
			first_param = false;
			print_out_param(of, ns, rettyp, "_result");
		}

		IDL_tree params = IDL_OP_DCL(op->node).parameter_dcls;
		if(first_param && params == NULL) fprintf(of, "void");
		IDL_LIST_FOREACH(iter, params) {
			if(first_param) first_param = false; else fprintf(of, ", ");
			IDL_tree p = IDL_LIST(iter).data,
				decl = IDL_PARAM_DCL(p).simple_declarator,
				type = get_type_spec(IDL_PARAM_DCL(p).param_type_spec);
			const char *name = IDL_IDENT(decl).str;
			switch(IDL_PARAM_DCL(p).attr) {
				case IDL_PARAM_IN: {
					char *typestr;
					if(IDL_NODE_TYPE(type) == IDLN_TYPE_SEQUENCE) {
						typestr = in_param_type(ns, SEQ_SUBTYPE(type));
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
}


static void print_exn_raisers(struct print_ctx *pr, IDL_tree iface)
{
	GList *exn_list = iface_exns_sorted(pr->ns, iface);

	/* output preprocessor-guarded externs for each, except the ones that are
	 * raised by negative return value.
	 */
	bool first = true;
	GLIST_FOREACH(e_cur, exn_list) {
		IDL_tree exn = e_cur->data;
		if(is_negs_exn(exn)) continue;
		const char *repo_id = IDL_IDENT_REPO_ID(IDL_EXCEPT_DCL(exn).ident);

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

		struct member_item *members = expand_member_list(
			IDL_EXCEPT_DCL(exn).members);
		for(int i=0; members[i].type != NULL; i++) {
			const char *m_suffix = members[i+1].type == NULL ? ");" : ",";
			IDL_tree mtype = members[i].type;
			if(members[i].dim > 0) {
				/* FIXME: output the correct type w/pointer when array */
			} else {
				/* FIXME: stick the right type in thur (strings, sequences,
				 * etc)
				 */
				code_f(pr, "[%s] %s%s", IDL_NODE_TYPE_NAME(mtype),
					members[i].name, m_suffix);
			}
		}
		g_free(members);

		indent(pr, -1);
		code_f(pr, "#endif  /* %s */", def);

		g_free(def);
		g_free(mangled);
		g_free(raiser_name);
	}

	g_list_free(exn_list);
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


/* FIXME: 'tarded. rigid_type() should handle value types also. */
static char *fixed_type(IDL_ns ns, IDL_tree type)
{
	if(is_value_type(type)) return value_type(ns, type);
	else if(is_rigid_type(type)) return rigid_type(ns, type);
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


/* return value is allocated in the heap. caller frees. */
static char *iface_context_type(struct print_ctx *pr, IDL_tree iface)
{
	char *parts[5];
	int p = 0;
//	parts[p++] = g_strdup("muidl");
	IDL_tree module = IDL_get_parent_node(iface, IDLN_MODULE, NULL);
	if(module != NULL) {
		parts[p++] = decapsify(IDL_IDENT(IDL_MODULE(module).ident).str);
	}
	parts[p++] = decapsify(IDL_IDENT(IDL_INTERFACE(iface).ident).str);
	parts[p++] = g_strdup("ctx");
	parts[p] = NULL;
	char *ret = g_strjoinv("_", parts);
	for(int i=0; i<p; i++) g_free(parts[i]);
	return ret;
}


typedef void (*param_fn)(
	struct print_ctx *pr,
	int param_index,		/* first being 0, second 1 etc */
	const char *c_type,
	const char *name,
	bool is_last,
	void *userdata);


/* returns number of parameters seen */
static int each_stub_parameter(
	struct print_ctx *pr,
	IDL_tree op,
	int tok,		/* timeout kind mask */
	param_fn paramfn,
	void *userdata)
{
	assert((tok & ~(TIMEOUT_SEND | TIMEOUT_RECV)) == 0);
	const bool has_ctx = has_complex_exn(op);
	int pnum = 0;	/* known as param_ix in newer code */
	IDL_tree params = IDL_OP_DCL(op).parameter_dcls,
		op_type = get_type_spec(IDL_OP_DCL(op).op_type_spec);

	if(has_mapped_param(op)) {
		/* computing is_last is pretty copypastey. this should be substituted
		 * by sticking all these things in a pointer array and iterating it at
		 * function end.
		 */
		(*paramfn)(pr, pnum++, "L4_Fpage_t", "_accept_range",
			!has_pager_target(op) && tok == 0 && params == NULL
				&& op_type == NULL && !has_ctx,
			userdata);
	}

	if(!has_pager_target(op)) {
		(*paramfn)(pr, pnum++, "L4_ThreadId_t", "_service_tid",
			tok == 0 && params == NULL && op_type == NULL && !has_ctx,
			userdata);
	}

	if(op_type != NULL) {
		assert(!IDL_OP_DCL(op).f_oneway);
		char *typ = rigid_type(pr->ns, op_type),
			*ptrtyp = g_strconcat(typ, " *", NULL);
		(*paramfn)(pr, pnum++, ptrtyp, "_retval_p",
			tok == 0 && params == NULL && !has_ctx, userdata);
		g_free(typ);
		g_free(ptrtyp);
	}

	for(IDL_tree cur = params; cur != NULL; cur = IDL_LIST(cur).next) {
		IDL_tree param = IDL_LIST(cur).data;
		enum IDL_param_attr attr = IDL_PARAM_DCL(param).attr;
		bool is_last = tok == 0 && !has_ctx && IDL_LIST(cur).next == NULL,
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
			(*paramfn)(pr, pnum++, tmpstr, c_name, is_last, userdata);
		} else if(is_rigid_type(type)) {
			tmpstr = fixed_type(pr->ns, type);
			(*paramfn)(pr, pnum++,
				tmp_f(pr, "%s%s *", in_only ? "const " : "",
					tmpstr), c_name, is_last, userdata);
		} else if(IDL_NODE_TYPE(type) == IDLN_TYPE_SEQUENCE) {
			tmpstr = fixed_type(pr->ns, SEQ_SUBTYPE(type));
			(*paramfn)(pr, pnum++,
				tmp_f(pr, "%s%s *", in_only ? "const " : "", tmpstr),
				c_name, false, userdata);
			(*paramfn)(pr, pnum++,
				tmp_f(pr, "unsigned int%s", !in_only ? " *" : ""),
				tmp_f(pr, "%s_len%s", c_name, !in_only ? "_ptr" : ""),
				is_last, userdata);
		} else if(IDL_NODE_TYPE(type) == IDLN_TYPE_STRING) {
			(*paramfn)(pr, pnum++, in_only ? "const char *" : "char *",
				c_name, is_last, userdata);
		} else if(IDL_NODE_TYPE(type) == IDLN_TYPE_WIDE_STRING) {
			(*paramfn)(pr, pnum++, in_only ? "const wchar_t *" : "wchar_t *",
				c_name, is_last, userdata);
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
				!has_ctx && tok >> (i + 1) == 0, userdata);
		}
	}

	if(has_complex_exn(op)) {
		IDL_tree iface = IDL_get_parent_node(op, IDLN_INTERFACE, NULL);
		assert(iface != NULL);
		char *typ = iface_context_type(pr, iface);
		(*paramfn)(pr, pnum++, tmp_f(pr, "%s_t *", typ), "_context_ptr", true,
			userdata);
		g_free(typ);
	}

	return pnum;
}


struct generic_stub_each_param_data {
	char *stubhead;
};

static void generic_stub_each_param(
	struct print_ctx *pr,
	int param_ix,
	const char *c_type,
	const char *name,
	bool is_last,
	void *userdata)
{
	struct generic_stub_each_param_data *data = userdata;
	if(param_ix == 0) {
		code_f(pr, "%s(%s%s%s%c", data->stubhead, c_type, type_space(c_type),
			name, is_last ? ')' : ',');
		if(!is_last) indent(pr, 1);
	} else {
		code_f(pr, "%s%s%s%c", c_type, type_space(c_type), name,
			is_last ? ')' : ',');
		if(is_last) indent(pr, -1);
	}
}


static void print_generic_stub_decl(
	struct print_ctx *pr,
	const char *stubpfx,
	IDL_tree op,
	int tok)		/* timeout kind mask */
{
	struct generic_stub_each_param_data data = {
		.stubhead = tmp_f(pr, "int %s", stub_name(pr, stubpfx, op, tok)),
	};

	int n = each_stub_parameter(pr, op, tok, &generic_stub_each_param, &data);
	if(n == 0) code_f(pr, "%s(void)", data.stubhead);
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


static char *long_exn_name(IDL_tree exn)
{
	char *parts[3];
	int p = 0;
	IDL_tree iface = IDL_get_parent_node(exn, IDLN_INTERFACE, NULL);
	if(iface != NULL) {
		parts[p++] = decapsify(IDL_IDENT(IDL_INTERFACE(iface).ident).str);
	} else {
		IDL_tree mod = IDL_get_parent_node(exn, IDLN_MODULE, NULL);
		if(mod != NULL) {
			parts[p++] = decapsify(IDL_IDENT(IDL_MODULE(mod).ident).str);
		}
	}
	parts[p++] = decapsify(IDL_IDENT(IDL_EXCEPT_DCL(exn).ident).str);
	parts[p] = NULL;
	char *ret = g_strjoinv("_", parts);
	for(int i=0; i<p; i++) g_free(parts[i]);
	return ret;
}


static void print_iface_context_info(struct print_ctx *pr, IDL_tree iface)
{
	GList *exns = iface_exns_sorted(pr->ns, iface);
	bool has_complex = false;
	GLIST_FOREACH(cur, exns) {
		IDL_tree exn = cur->data;
		if(is_complex_exn(exn)) {
			has_complex = true;
			break;
		}
	}
	if(!has_complex) return;

	char *name = iface_context_type(pr, iface);
	code_f(pr, "typedef union {");
	indent(pr, 1);
	code_f(pr, "uint32_t tag;");

	/* give them sufficiently unique names for use in the context union. */
	GPtrArray *exn_names = g_ptr_array_new_with_free_func(&g_free);
	GLIST_FOREACH(e_cur, exns) {
		IDL_tree exn = e_cur->data, ident = IDL_EXCEPT_DCL(exn).ident;
		char *ex_name = decapsify(IDL_IDENT(ident).str);
		/* is the name already in use? */
		int loc = -1;
		for(int i=0; i < exn_names->len; i++) {
			if(strcmp(ex_name, exn_names->pdata[i]) == 0) {
				loc = i;
				break;
			}
		}
		/* (only longify the current exception name for reserved words.) */
		if(loc >= 0 || is_reserved_word(ex_name)) {
			/* prefix both by their immediate parent interface or module's
			 * name, where available.
			 */
			g_free(ex_name);
			ex_name = long_exn_name(exn);
			if(loc >= 0) {
				g_free(exn_names->pdata[loc]);
				exn_names->pdata[loc] = long_exn_name(g_list_nth_data(exns, loc));
				assert(strcmp(ex_name, exn_names->pdata[loc]) != 0);
			}
		}
		g_ptr_array_add(exn_names, ex_name);
	}

	int exn_pos = -1;
	GLIST_FOREACH(e_cur, exns) {
		exn_pos++;
		IDL_tree exn = e_cur->data;
		code_f(pr, "struct {");
		indent(pr, 1);
		code_f(pr, "uint32_t _tag;");

		IDL_LIST_FOREACH(m_cur, IDL_EXCEPT_DCL(exn).members) {
			IDL_tree memb = IDL_LIST(m_cur).data,
				mtype = get_type_spec(IDL_MEMBER(memb).type_spec);
			char *typestr = NULL;
			IDL_LIST_FOREACH(d_cur, IDL_MEMBER(memb).dcls) {
				IDL_tree dcl = IDL_LIST(d_cur).data;
				if(IDL_NODE_TYPE(dcl) == IDLN_TYPE_ARRAY) {
					assert(is_rigid_type(mtype));
					long long size = IDL_INTEGER(IDL_LIST(
						IDL_TYPE_ARRAY(dcl).size_list).data).value;
					typestr = fixed_type(pr->ns, mtype);
					code_f(pr, "%s %s[%lld];", typestr,
						IDL_IDENT(IDL_TYPE_ARRAY(dcl).ident).str, size);
				} else if(IDL_NODE_TYPE(dcl) == IDLN_IDENT) {
					const char *mname = IDL_IDENT(dcl).str;
					if(is_reserved_word(mname)) {
						mname = tmp_f(pr, "_%s", mname);
					}
					if(is_rigid_type(mtype)) {
						typestr = fixed_type(pr->ns, mtype);
						code_f(pr, "%s %s;", typestr, mname);
					} else if(IDL_NODE_TYPE(mtype) == IDLN_TYPE_STRING) {
						int len = IDL_INTEGER(
							IDL_TYPE_STRING(mtype).positive_int_const).value;
						code_f(pr, "char %s[%d];", mname, len + 1);
						/* TODO: wide strings */
					} else if(IDL_NODE_TYPE(mtype) == IDLN_TYPE_SEQUENCE) {
						int len = IDL_INTEGER(
							IDL_TYPE_SEQUENCE(mtype).positive_int_const).value;
						char *subtypestr = fixed_type(pr->ns,
							SEQ_SUBTYPE(mtype));
						code_f(pr, "%s %s[%d];", subtypestr, mname, len);
						code_f(pr, "uint32_t %s_len;", mname);
						g_free(subtypestr);
					} else {
						NOTDEFINED(mtype);
					}
				} else {
					g_assert_not_reached();
				}
			}
			g_free(typestr);
		}

		indent(pr, -1);
		code_f(pr, "} %s;", (char *)exn_names->pdata[exn_pos]);
	}

	indent(pr, -1);
	code_f(pr, "} %s_t;", name);

	g_free(name);
	g_list_free(exns);
	g_ptr_array_free(exn_names, TRUE);
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


static size_t hash_ident_repo_id(const void *ptr, void *priv) {
	return hash_string(IDL_IDENT((IDL_tree)ptr).repo_id);
}


static bool cmp_ident_repo_id_str(const void *cand, void *key) {
	return streq(IDL_IDENT((IDL_tree)cand).repo_id, (char *)key);
}


static gboolean _find_struct_decl_by_ident(IDL_tree_func_data *tf, void *priv)
{
	void **param = priv;
	if(IDL_NODE_TYPE(tf->tree) != IDLN_TYPE_STRUCT) return TRUE;
	IDL_tree ident = IDL_TYPE_STRUCT(tf->tree).ident, want = (IDL_tree)param[0];
	if(!streq(IDL_IDENT(want).repo_id, IDL_IDENT(ident).repo_id)) return TRUE;

	*(IDL_tree *)param[1] = tf->tree;
	return FALSE;
}


static IDL_tree find_local_struct(struct print_ctx *pr, IDL_tree ident)
{
	assert(IDL_NODE_TYPE(ident) == IDLN_IDENT);

	IDL_tree struc = NULL;
	void *param[] = { ident, &struc };
	IDL_tree_walk_in_order(pr->tree, &_find_struct_decl_by_ident, param);
	return struc;
}


struct forward_structs_ctx {
	struct print_ctx *pr;
	struct htable *seen;
	bool first;
};


/* TODO: extend this to also print union and enum forwards */
static gboolean print_struct_forwards(IDL_tree_func_data *tf, void *priv)
{
	struct forward_structs_ctx *ctx = priv;
	if(IDL_NODE_TYPE(tf->tree) != IDLN_OP_DCL) return TRUE;

	IDL_LIST_FOREACH(cur, IDL_OP_DCL(tf->tree).parameter_dcls) {
		IDL_tree p = IDL_LIST(cur).data,
			typ = get_type_spec(IDL_PARAM_DCL(p).param_type_spec);
		if(IDL_NODE_TYPE(typ) != IDLN_TYPE_STRUCT) continue;

		/* do each repo_id once */
		IDL_tree ident = IDL_TYPE_STRUCT(typ).ident;
		size_t hash = hash_ident_repo_id(ident, NULL);
		IDL_tree prev = htable_get(ctx->seen, hash, &cmp_ident_repo_id_str,
			IDL_IDENT(ident).repo_id);
		if(prev != NULL) continue;
		htable_add(ctx->seen, hash, ident);

		/* determine if the structure is in fact defined in the current IDL
		 * source, and don't forward-declare it if so.
		 */
		if(find_local_struct(ctx->pr, ident) != NULL) continue;

		if(ctx->first) {
			ctx->first = false;
			fprintf(ctx->pr->of, "/* forwards of out-of-file structs */\n");
		}
		char *name = long_name(ctx->pr->ns, typ);
		code_f(ctx->pr, "struct %s;", name);
		g_free(name);
	}

	return TRUE;
}


static void print_struct_decl(struct print_ctx *pr, IDL_tree struc)
{
	assert(IDL_NODE_TYPE(struc) == IDLN_TYPE_STRUCT);

	void *talctx = talloc_new(NULL);
	char *l = long_name(pr->ns, struc), *upl = g_utf8_strup(l, -1);

	/* cpp guard. this is nowhere near secure; really we'd like to siphash the
	 * signature string, define as many variables as there are 64-bit limbs in
	 * its output, and check all of them in the preprocessor. that type of
	 * fugly can easily stay in a header file, so no worries about aesthetics.
	 */
	char *sig = struct_signature(talctx, struc);
	code_f(pr, "#if defined(_MUIDL_%s_SIG) && _MUIDL_%s_SIG != %s", upl, upl, sig);
	code_f(pr, "#error \"struct signature mismatch on %s!\"",
		IDL_IDENT(IDL_TYPE_STRUCT(struc).ident).repo_id);
	code_f(pr, "#elif !defined(_MUIDL_%s_SIG)", upl);
	code_f(pr, "#define _MUIDL_%s_SIG %s", upl, sig);
	g_free(upl);

	IDL_tree mems = IDL_TYPE_STRUCT(struc).member_list;
	code_f(pr, "struct %s%s{", l, IDL_list_length(mems) > 2 ? "\n" : " ");
	indent(pr, 1);
	g_free(l);

	IDL_LIST_FOREACH(cur, mems) {
		IDL_tree member = IDL_LIST(cur).data,
			type = get_type_spec(IDL_MEMBER(member).type_spec);

		char *typestr = NULL, *suffix = NULL;
		unsigned long max_size = 0;
		if(is_value_type(type)) {
			typestr = value_type(pr->ns, type);
		} else {
			assert(is_rigid_type(type));
			typestr = rigid_type(pr->ns, type);

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

				default:
					break;
			}
			if(bound != NULL) {
				max_size = IDL_INTEGER(bound).value;
				suffix = g_strdup_printf("[%lu]", max_size + extra);
			}
		}

		IDL_LIST_FOREACH(dcl_node, IDL_MEMBER(member).dcls) {
			IDL_tree data = IDL_LIST(dcl_node).data;
			if(IDL_NODE_TYPE(data) == IDLN_IDENT) {
				const char *name = IDL_IDENT(data).str;
				if(is_value_type(type)) {
					code_f(pr, "\t %s %s;", typestr, name);
				} else if(is_rigid_type(type)) {
					if(IDL_NODE_TYPE(type) == IDLN_TYPE_SEQUENCE) {
						const char *t;
						if(max_size <= 255) t = "uint8_t";
						else if(max_size <= 65535) t = "uint16_t";
						else t = "unsigned";
						code_f(pr, "%s %s_len;", t, name);
					}
					code_f(pr, "%s %s%s%s;", typestr, name,
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
				code_f(pr, "%s %s%s[%ld];", typestr, name,
					is_reserved_word(name) ? "_" : "",
					(long)IDL_INTEGER(IDL_LIST(size_list).data).value);
			} else {
				NOTDEFINED(data);
			}
		}

		g_free(typestr);
		g_free(suffix);
	}
	indent(pr, -1);
	code_f(pr, "}%s;", is_packed(struc) ? " __attribute__((__packed__))" : "");

	code_f(pr, "#endif");
	talloc_free(talctx);
}


struct struct_decl_ctx {
	struct print_ctx *pr;
	bool first;
	struct htable *seen;
};


static gboolean print_struct_decls(IDL_tree_func_data *tf, void *priv)
{
	struct struct_decl_ctx *ctx = priv;
	switch(IDL_NODE_TYPE(tf->tree)) {
		default: return FALSE;

		case IDLN_LIST:
		case IDLN_MODULE:
		case IDLN_SRCFILE:
		case IDLN_INTERFACE:
			return TRUE;

		case IDLN_TYPE_STRUCT: break;
	}

	IDL_tree ident = IDL_TYPE_STRUCT(tf->tree).ident;
	size_t hash = hash_ident_repo_id(ident, NULL);
	IDL_tree prev = htable_get(ctx->seen, hash, &cmp_ident_repo_id_str, IDL_IDENT(ident).repo_id);
	if(prev == NULL) {
		htable_add(ctx->seen, hash, ident);
		if(ctx->first) {
			ctx->first = false;
			if(htable_count(ctx->seen) > 0) {
				code_f(ctx->pr, "/* locally declared structs */");
			}
		}
		print_struct_decl(ctx->pr, tf->tree);
	}
	return FALSE;
}


static gboolean print_composed_structs(IDL_tree_func_data *tf, void *priv)
{
	struct struct_decl_ctx *ctx = priv;
	if(IDL_NODE_TYPE(tf->tree) != IDLN_TYPE_STRUCT) return TRUE;

	IDL_LIST_FOREACH(cur, IDL_TYPE_STRUCT(tf->tree).member_list) {
		IDL_tree member = IDL_LIST(cur).data,
			type = get_type_spec(IDL_MEMBER(member).type_spec);
		if(IDL_NODE_TYPE(type) != IDLN_TYPE_STRUCT) continue;
		if(find_local_struct(ctx->pr, IDL_TYPE_STRUCT(type).ident) != NULL) continue;

		IDL_tree m_id = IDL_TYPE_STRUCT(type).ident;
		size_t hash = hash_ident_repo_id(m_id, NULL);
		IDL_tree prev = htable_get(ctx->seen, hash, &cmp_ident_repo_id_str,
			IDL_IDENT(m_id).repo_id);
		if(prev != NULL) continue;
		htable_add(ctx->seen, hash, m_id);

		if(ctx->first) {
			ctx->first = false;
			code_f(ctx->pr, "/* structures declared in other IDL files */");
		}
		print_struct_decl(ctx->pr, type);
	}

	return TRUE;
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
		"l4/types.h",
	};
	print_headers(pr, hdrfiles, G_N_ELEMENTS(hdrfiles));

	/* constant declarations. */
	IDL_tree_walk_in_order(pr->tree, &print_consts, pr);
	code_f(pr, " ");

	/* local declaration of structs #included from other IDL files and used as
	 * members of structs declared locally.
	 */
	struct htable struc_seen = HTABLE_INITIALIZER(struc_seen, &hash_ident_repo_id, NULL);
	struct struct_decl_ctx sdc = { .pr = pr, .first = true, .seen = &struc_seen };
	IDL_tree_walk_in_order(pr->tree, &print_composed_structs, &sdc);
	code_f(pr, " ");

	/* struct, union & enum declarations as they appear in the IDL source. */
	sdc.first = true;
	IDL_tree_walk_in_order(pr->tree, &print_struct_decls, &sdc);
	code_f(pr, " ");

	/* forward declaration of structs #included from other IDL files, which
	 * only appear as operation parameters.
	 */
	IDL_tree_walk_in_order(pr->tree, &print_struct_forwards,
		&(struct forward_structs_ctx){ .pr = pr, .first = true, .seen = &struc_seen });
	code_f(pr, " ");
	htable_clear(&struc_seen);

	/* context types and exception tag defines for the stub declarations. */
	GLIST_FOREACH(cur, pr->ifaces) {
		struct iface_info *inf = cur->data;
		print_iface_context_info(pr, inf->node);
	}
	if(pr->ifaces != NULL) code_f(pr, " ");

	/* stub prototypes. */
	IDL_tree_walk_in_order(pr->tree, &print_stub_protos, pr);
	code_f(pr, " ");

	/* interface vtables and dispatcher prototypes, but only for service
	 * implementations (so as to avoid polluting the namespace).
	 */
	GLIST_FOREACH(cur, pr->ifaces) {
		struct iface_info *iface = cur->data;
		IDL_tree mod = IDL_get_parent_node(iface->node, IDLN_MODULE, NULL);

		/* vtable etc. selector */
		char *modpfx = NULL, *ifpfx;
		if(mod != NULL) {
			modpfx = g_utf8_strup(IDL_IDENT(IDL_MODULE(mod).ident).str,
				-1);
		}
		ifpfx = g_utf8_strup(IDL_IDENT(IDL_INTERFACE(iface->node).ident).str,
			-1);
		code_f(pr, "#if defined(%s%s%s_IMPL_SOURCE) || defined(MUIDL_SOURCE)",
			modpfx == NULL ? " " : modpfx, mod == NULL ? "" : "_",
			ifpfx);
		code_f(pr, "#include <l4/types.h>");
		g_free(modpfx);
		g_free(ifpfx);

		/* vtable declaration */
		code_f(pr, "\n/* vtable for `%s': */", iface->name);
		print_vtable(pr->of, pr->tree, pr->ns, iface);

		/* dispatcher prototype */
		code_f(pr, " ");
		char *vtprefix = NULL,
			*dispname = dispatcher_name(pr->ns, iface->node, &vtprefix);
		code_f(pr, "extern L4_Word_t %s(", dispname);
		g_free(dispname);
		indent(pr, 1);
		code_f(pr, "const struct %s_vtable *vtable);", vtprefix);
		indent(pr, -1);
		g_free(vtprefix);

		/* exception raisers */
		print_exn_raisers(pr, iface->node);

		/* close off vtable selector */
		code_f(pr, "\n#endif\n");
	}

	code_f(pr, "#endif");
	g_free(upper);
}
