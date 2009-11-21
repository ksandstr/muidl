/*
 * gen-stubs.c -- generic stub generation for the L4.X2 API
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
#include <assert.h>
#include <libIDL/IDL.h>

#include "muidl.h"


/* TODO: move this to util.c */
char *fixed_type(IDL_ns ns, IDL_tree type)
{
	if(is_value_type(type)) return value_type(ns, type);
	else if(is_rigid_type(ns, type)) return rigid_type(ns, type);
	else NOTDEFINED(type);
}


/* TODO: export this, use in gen-common.c */
static void print_stub_decl(
	struct print_ctx *pr,
	const char *stubpfx,
	IDL_tree op,
	IDL_tree params)
{
	char *name = decapsify(METHOD_NAME(op)),
		*rettypstr = return_type(pr->ns, op, NULL);
	code_f(pr, "%s%s%s(%s", rettypstr, type_space(rettypstr),
		stubpfx == NULL ? name : tmp_f(pr, "%s_%s", stubpfx, name),
		params == NULL ? "L4_ThreadId_t _service_tid)" : "");
	g_free(name);
	g_free(rettypstr);
	if(params != NULL) {
		indent(pr, 1);
		code_f(pr, "L4_ThreadId_t _service_tid,");
	}
	for(IDL_tree cur = params; cur != NULL; cur = IDL_LIST(cur).next) {
		IDL_tree param = IDL_LIST(cur).data;
		enum IDL_param_attr attr = IDL_PARAM_DCL(param).attr;
		IDL_tree type = get_type_spec(
				IDL_PARAM_DCL(param).param_type_spec),
			decl = IDL_PARAM_DCL(param).simple_declarator;
		const char *name = IDL_IDENT(decl).str,
			*suffix = IDL_LIST(cur).next == NULL ? ")" : ",";

		char *tmpstr = NULL;
		if(attr == IDL_PARAM_IN && is_value_type(type)) {
			tmpstr = value_type(pr->ns, type);
			code_f(pr, "%s %s%s", tmpstr, name, suffix);
		} else if(is_rigid_type(pr->ns, type)) {
			tmpstr = fixed_type(pr->ns, type);
			code_f(pr, "%s%s *%s_p%s",
				attr == IDL_PARAM_IN ? "const " : "",
				tmpstr, name, suffix);
		} else if(IDL_NODE_TYPE(type) == IDLN_TYPE_SEQUENCE) {
			IDL_tree subtype = get_type_spec(
				IDL_TYPE_SEQUENCE(type).simple_type_spec);
			tmpstr = fixed_type(pr->ns, subtype);
			code_f(pr, "%s%s *seq_%s,",
				attr == IDL_PARAM_IN ? "const " : "",
				tmpstr, name);
			code_f(pr, "unsigned int seq_%s_len%s", name, suffix);
		} else {
			/* TODO: handle structs, strings, arrays etc */
			NOTDEFINED(type);
		}
		g_free(tmpstr);
	}
	if(params != NULL) indent(pr, -1);
}


static void print_stubs_for_iface(struct print_ctx *pr, IDL_tree iface)
{
	const char *iface_stubpfx = IDL_tree_property_get(
		IDL_INTERFACE(iface).ident, "StubPrefix");

	GList *methods = all_methods_of_iface(pr->ns, iface);
	for(GList *cur = g_list_first(methods);
		cur != NULL;
		cur = g_list_next(cur))
	{
		IDL_tree op = cur->data,
			params = IDL_OP_DCL(op).parameter_dcls;

		const char *stubpfx = IDL_tree_property_get(IDL_OP_DCL(op).ident,
			"StubPrefix");
		if(stubpfx == NULL) stubpfx = iface_stubpfx;

		/* declaration */
		print_stub_decl(pr, stubpfx, op, params);

		/* body */
		code_f(pr, "{");
		indent(pr, 1);

		/* FIXME: this needs the analyse.c output for each operation, so pass
		 * that to this function instead of a pile of IDL_OP_DCLs.
		 */
		code_f(pr, "/* stub body goes here */");

		close_brace(pr);
		code_f(pr, "\n");
	}
}


void print_stubs_file(struct print_ctx *pr)
{
	print_file_heading(pr);

	static const char *hdrfiles[] = {
		"stdint.h", "stdbool.h", "kernel/types.h", "l4/types.h",
	};
	print_headers(pr, hdrfiles, G_N_ELEMENTS(hdrfiles));
	code_f(pr, "#include \"%s\"\n", pr->common_header_name);

	GHashTableIter iter;
	g_hash_table_iter_init(&iter, pr->ifaces);
	gpointer key, value;
	while(g_hash_table_iter_next(&iter, &key, &value)) {
		IDL_tree iface = (IDL_tree)value;
		print_stubs_for_iface(pr, iface);
	}
}
