/*
 * gen-stubs.c -- generic stub generation for the L4.X2 API
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
#include <stdbool.h>
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


static bool has_pager_target(IDL_ns ns, IDL_tree op_dcl)
{
	IDL_tree prop_node = IDL_OP_DCL(op_dcl).ident;
	const char *pt = IDL_tree_property_get(prop_node, "PagerTarget");
	return pt != NULL;
}


void print_generic_stub_decl(
	struct print_ctx *pr,
	const char *stubpfx,
	IDL_tree op,
	IDL_tree params)
{
	char *name = decapsify(METHOD_NAME(op)),
		*rettypstr = return_type(pr->ns, op, NULL);
	bool pt = has_pager_target(pr->ns, op);
	code_f(pr, "%s%s%s(%s", rettypstr, type_space(rettypstr),
		stubpfx == NULL ? name : tmp_f(pr, "%s_%s", stubpfx, name),
		params == NULL ? (pt ? "void)" : "L4_ThreadId_t _service_tid)") : "");
	g_free(name);
	g_free(rettypstr);
	if(params != NULL) {
		indent(pr, 1);
		if(!pt) code_f(pr, "L4_ThreadId_t _service_tid,");
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
			code_f(pr, "%s p_%s%s", tmpstr, name, suffix);
		} else if(is_rigid_type(pr->ns, type)) {
			tmpstr = fixed_type(pr->ns, type);
			code_f(pr, "%s%s *p_%s%s",
				attr == IDL_PARAM_IN ? "const " : "",
				tmpstr, name, suffix);
		} else if(IDL_NODE_TYPE(type) == IDLN_TYPE_SEQUENCE) {
			IDL_tree subtype = get_type_spec(
				IDL_TYPE_SEQUENCE(type).simple_type_spec);
			tmpstr = fixed_type(pr->ns, subtype);
			code_f(pr, "%s%s *p_%s,",
				attr == IDL_PARAM_IN ? "const " : "",
				tmpstr, name);
			code_f(pr, "unsigned int %sp_%s_len%s%s",
				attr != IDL_PARAM_IN ? "*" : "", name,
				attr != IDL_PARAM_IN ? "_ptr" : "", suffix);
		} else {
			/* TODO: handle structs, strings, arrays etc */
			NOTDEFINED(type);
		}
		g_free(tmpstr);
	}
	if(params != NULL) indent(pr, -1);
}


static void print_store_untyped_words(
	struct print_ctx *pr,
	const struct message_info *reply,
	const char *var_prefix)
{
	for(int i=0; i<reply->num_untyped; i++) {
		const struct untyped_param *u = reply->untyped[i];
		if(IS_WORD_TYPE(u->type)) {
			code_f(pr, "L4_StoreMR(%d, %s%s);", u->first_reg, var_prefix,
				u->name);
		} else if(IS_FPAGE_TYPE(u->type)) {
			code_f(pr, "L4_StoreMR(%d, &%s%s->raw);", u->first_reg,
				var_prefix, u->name);
		} else if(is_value_type(u->type)) {
			code_f(pr, "{ L4_Word_t _tmp; L4_StoreMR(%d, &_tmp); %s%s = _tmp; }",
				u->first_reg, var_prefix, u->name);
		} else {
			/* TODO: multi-word untyped parameters */
			NOTDEFINED(u->type);
		}
	}
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
		struct method_info *inf = analyse_op_dcl(pr, op);
		g_return_if_fail(inf != NULL);

		const char *stubpfx = IDL_tree_property_get(IDL_OP_DCL(op).ident,
			"StubPrefix");
		if(stubpfx == NULL) stubpfx = iface_stubpfx;

		/* declaration */
		print_generic_stub_decl(pr, stubpfx, op, params);

		/* body */
		code_f(pr, "{");
		indent(pr, 1);

		const struct message_info *req = inf->request;

		code_f(pr, "L4_Msg_t msg;\n"
				   "L4_MsgClear(&msg);\n"
				   "L4_Set_MsgLabel(&msg, %#x);",
			(unsigned)req->label);
		if(req->sublabel != NO_SUBLABEL) {
			code_f(pr, "L4_MsgAppendWord(&msg, %#x);", req->sublabel);
		}
		print_msg_encoder(pr, req, NULL, "&msg", "p_");
		if(req->tagmask != NO_TAGMASK) {
			/* force tagmask's bits to what is specified in the label. */
			code_f(pr, "msg.tag.raw = (msg.tag.raw & %#x) | (%#x & %#x);",
				~(unsigned)req->tagmask, (unsigned)req->label,
				(unsigned)req->tagmask);
		}

		/* TODO: use AcceptStrings, provide mapitem reception ranges, etc,
		 * where applicable
		 */
		code_f(pr, "L4_Accept(L4_UntypedWordsAcceptor);\n"
				   "L4_MsgLoad(&msg);");
		code_f(pr, "L4_MsgTag_t tag = L4_%s(%s);",
			inf->num_reply_msgs > 0 ? "Call" : "Send",
			has_pager_target(pr->ns, inf->node) ? "L4_Pager()" : "_service_tid");

		/* handle IPC failure. */
		code_f(pr, "if(L4_IpcFailed(tag)) return (int)L4_ErrorCode();");

		/* the reply message, if present */
		int reply_ix = 0;
		if(inf->num_reply_msgs > 0 && !IS_EXN_MSG(inf->replies[0])) {
			const struct message_info *reply = inf->replies[reply_ix++];
			code_f(pr, "if(L4_Label(tag) == 0) {");
			indent(pr, 1);
			print_store_untyped_words(pr, reply, "p_");
			if(reply->num_inline_seq > 0 || reply->num_long > 0) {
				code_f(pr, "L4_MsgStore(tag, &msg);");
				print_decode_inline_seqs(pr, reply, "&msg", "p_");
				/* TODO: decode string-carried things! */
			}
			code_f(pr, "return 0;");
		}
		/* exception messages */
		while(reply_ix < inf->num_reply_msgs) {
			const struct message_info *ex = inf->replies[reply_ix++];
			assert(IS_EXN_MSG(ex));

			const char *ifpfx = "";
			if(reply_ix > 1) {
				indent(pr, -1);
				ifpfx = "} else ";
			}
			if(is_negs_exn(ex->node)) {
				const unsigned label = 1;	/* FIXME: get from properties! */
				code_f(pr, "%sif(L4_Label(tag) == %u) {", ifpfx, label);
				indent(pr, 1);
				code_f(pr, "L4_Word_t _code;");
				code_f(pr, "L4_StoreMR(1, &_code);");
				code_f(pr, "return -(int)_code;");
			} else {
				code_f(pr, "%sif(false) {", ifpfx);
				indent(pr, 1);
				code_f(pr, "/* would check & decode exception `%s' */",
					EXN_REPO_ID(ex->node));
			}
		}
		if(reply_ix > 0) close_brace(pr);

		if(inf->num_reply_msgs == 0) {
			/* no replies expected. indicate success. */
			code_f(pr, "return 0;");
		} else {
			/* no replies matched. this is a communication error on the
			 * callee's part; signal as such.
			 *
			 * FIXME: record this case somewhere, i.e. that -235 indicates a
			 * non-conforming reply.
			 */
			code_f(pr, "return -235;");
		}

		close_brace(pr);
		code_f(pr, "\n");

		free_method_info(inf);
	}
}


void print_stubs_file(struct print_ctx *pr)
{
	print_file_heading(pr);

	static const char *hdrfiles[] = {
		"stdint.h", "stdbool.h",
		"l4/types.h", "l4/ipc.h", "l4/message.h",
		"kernel/types.h",
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
