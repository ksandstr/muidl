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


void print_generic_stub_decl(
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


static void print_store_untyped_words(
	struct print_ctx *pr,
	const struct message_info *reply,
	const char *var_prefix)
{
	GLIST_FOREACH(cur_u, reply->untyped) {
		const struct msg_param *u = cur_u->data;
		IDL_tree type = u->X.untyped.type;
		int first_reg = u->X.untyped.first_reg;
		if(IS_WORD_TYPE(type)) {
			code_f(pr, "L4_StoreMR(%d, %s%s);", first_reg, var_prefix,
				u->name);
		} else if(IS_FPAGE_TYPE(type)) {
			code_f(pr, "L4_StoreMR(%d, &%s%s->raw);", first_reg,
				var_prefix, u->name);
		} else if(is_value_type(type)) {
			code_f(pr, "{ L4_Word_t _tmp; L4_StoreMR(%d, &_tmp); %s%s = _tmp; }",
				first_reg, var_prefix, u->name);
		} else {
			/* TODO: multi-word untyped parameters */
			NOTDEFINED(type);
		}
	}
}


static void print_stub_timeout_wrapper(
	struct print_ctx *pr,
	const char *stubpfx,
	struct method_info *inf,
	int tok)
{
	assert(tok != 0);

	print_generic_stub_decl(pr, stubpfx, inf->node, 0);
	code_f(pr, "{"); indent(pr, 1);

	/* figure out if the stub is void or not. TODO: replace with an
	 * is_void_stub() or some such.
	 */
	char *rettypstr = return_type(pr->ns, inf->node, NULL, false);
	char *callhead = tmp_f(pr, "%s%s",
		strcmp(rettypstr, "void") == 0 ? "" : "return ",
		stub_name(pr, stubpfx, inf->node, tok));
	g_free(rettypstr);

	void each_param(
		struct print_ctx *pr,
		int param_ix,
		const char *c_type,
		const char *name,
		bool is_last)
	{
		const char *suffix = is_last ? ");" : ",";
		if(param_ix == 0) {
			code_f(pr, "%s(%s%s", callhead, name, suffix);
			if(!is_last) indent(pr, 1);
		} else {
			if(strcmp(name, "__send_timeout") == 0
				|| strcmp(name, "__recv_timeout") == 0)
			{
				name = "L4_Never";
			}
			code_f(pr, "%s%s", name, suffix);
			if(is_last) indent(pr, -1);
		}
	};

	int n = each_stub_parameter(pr, inf->node, tok, &each_param);
	if(n == 0) code_f(pr, "%s();", callhead);

	close_brace(pr);
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
		IDL_tree op = cur->data;
		const int tok = op_timeout_kind(op);
		struct method_info *inf = analyse_op_dcl(pr, op);
		g_return_if_fail(inf != NULL);

		const char *stubpfx = IDL_tree_property_get(IDL_OP_DCL(op).ident,
			"StubPrefix");
		if(stubpfx == NULL) stubpfx = iface_stubpfx;

		/* declaration */
		print_generic_stub_decl(pr, stubpfx, op, tok);

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
		code_f(pr, "const L4_ThreadId_t peer = %s;",
			has_pager_target(pr->ns, inf->node) ? "L4_Pager()" : "_service_tid");
		code_f(pr, "L4_ThreadId_t from_dummy;");
		code_f(pr, "L4_MsgTag_t tag = L4_Ipc(peer, %s,\n"
					"\tL4_Timeouts(%s, %s), &from_dummy);",
			IDL_OP_DCL(inf->node).f_oneway ? "L4_nilthread" : "peer",
			(tok & TIMEOUT_SEND) ? "__send_timeout" : "L4_Never",
			(tok & TIMEOUT_RECV) ? "__recv_timeout" : "L4_Never");

		/* handle IPC failure. */
		code_f(pr, "if(L4_IpcFailed(tag)) return (int)L4_ErrorCode();");

		/* the reply message, if present */
		int reply_ix = 0;
		if(inf->num_reply_msgs > 0 && !IS_EXN_MSG(inf->replies[0])) {
			const struct message_info *reply = inf->replies[reply_ix++];
			code_f(pr, "if(L4_Label(tag) == 0) {");
			indent(pr, 1);
			print_store_untyped_words(pr, reply, "p_");
			if(reply->seq != NULL || reply->_long != NULL) {
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

		if(tok != 0) {
			/* convenience wrapper, so that callers don't need to say
			 * "..._timeout(..., L4_Never, L4_Never)"
			 */
			print_stub_timeout_wrapper(pr, stubpfx, inf, tok);
		}

		free_method_info(inf);
	}
	g_list_free(methods);
}


void print_stubs_file(struct print_ctx *pr)
{
	print_file_heading(pr);

	static const char *hdrfiles[] = {
		"stdint.h", "stdbool.h", "string.h",
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
