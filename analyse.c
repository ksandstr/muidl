/*
 * analyse.c -- analysis of IDL elements
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
#include <stdint.h>
#include <string.h>
#include <stdbool.h>
#include <libIDL/IDL.h>

#include "muidl.h"


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


static bool get_msg_label(struct message_info *inf, IDL_tree prop_node)
{
	unsigned long tagmask = NO_TAGMASK;
	if(!get_ul_property(&tagmask, prop_node, "TagMask")) return false;
	inf->tagmask = tagmask;

	unsigned long labelval = 0;
	if(!get_ul_property(&labelval, prop_node, "Label")) return false;
	inf->label = labelval;

	IDL_tree iface = IDL_get_parent_node(prop_node, IDLN_INTERFACE, NULL);
	/* exceptions, among other things, can appear outside interfaces.
	 * fortunately sublabels don't apply to them.
	 */
	if(iface != NULL) {
		IDL_tree ifprop = IDL_INTERFACE(iface).ident;
		unsigned long ifacelabel = 0;
		if(get_ul_property(&ifacelabel, ifprop, "IfaceLabel")
			&& ifacelabel != 0)
		{
			inf->sublabel = inf->label;
			inf->label = ifacelabel;
		} else {
			inf->sublabel = NO_SUBLABEL;
		}
		/* TODO: disallow tagmasks alongside ifacelabels. */
	}

	return true;
}


static int size_in_bits(IDL_tree type)
{
	assert(is_value_type(type));

	int bits;
	switch(IDL_NODE_TYPE(type)) {
		case IDLN_TYPE_INTEGER: {
			static const int8_t bits_per[] = {
				[IDL_INTEGER_TYPE_SHORT] = 16,
				[IDL_INTEGER_TYPE_LONG] = 32,
				[IDL_INTEGER_TYPE_LONGLONG] = 64,
			};
			enum IDL_integer_type ityp = IDL_TYPE_INTEGER(type).f_type;
			assert(ityp >= 0 && ityp < G_N_ELEMENTS(bits_per));
			bits = bits_per[(int)ityp];
			break;
		}

		case IDLN_NATIVE:
			assert(IS_WORD_TYPE(type) || IS_FPAGE_TYPE(type));
			bits = BITS_PER_WORD;
			break;

		case IDLN_TYPE_OCTET:
		case IDLN_TYPE_CHAR:
			bits = 8;
			break;

		case IDLN_TYPE_BOOLEAN: bits = 1; break;

		default:
			/* TODO: float, widechar, enum types */
			NOTDEFINED(type);
	}

	return bits;
}


/* a type's maximum length in bytes. returns negative for unbounded.
 * doesn't include string terminator, since that's not transferred over IPC.
 */
static int max_size(IDL_tree type)
{
	if(is_value_type(type)) return size_in_bits(type) / 8;
	switch(IDL_NODE_TYPE(type)) {
		case IDLN_TYPE_SEQUENCE: {
			IDL_tree bound = IDL_TYPE_SEQUENCE(type).positive_int_const,
				elem = IDL_TYPE_SEQUENCE(type).simple_type_spec;
			if(bound == NULL) return -1;
			return max_size(elem) * IDL_INTEGER(bound).value;
		}

		case IDLN_TYPE_STRING: {
			IDL_tree bound = IDL_TYPE_STRING(type).positive_int_const;
			if(bound == NULL) return -1;
			return IDL_INTEGER(bound).value;
		}

		case IDLN_TYPE_WIDE_STRING:
		case IDLN_TYPE_ARRAY:
		case IDLN_TYPE_STRUCT:
		case IDLN_TYPE_UNION:
		case IDLN_TYPE_ENUM:
		default:
			NOTDEFINED(type);
	}
}


static bool is_bounded_seq(IDL_tree type)
{
	return IDL_NODE_TYPE(type) == IDLN_TYPE_SEQUENCE
		&& IDL_TYPE_SEQUENCE(type).positive_int_const != NULL
		&& IDL_INTEGER(IDL_TYPE_SEQUENCE(type).positive_int_const).value > 0;
}


/* turn a bunch of parameters into lists of untyped, inline-sequence and long
 * types. the first have fixed register assignments, inline sequences have
 * register ranges and long types appear in transmission order.
 */
static struct message_info *build_message(
	IDL_tree return_type,		/* not IDL_PARAM_DCL, therefore separate */
	const IDL_tree *params,
	int num_params)
{
	struct message_info *inf = NULL;
	GList *untyped = NULL, *seq = NULL,	/* struct [name]_parm * */
		*_long = NULL;	/* of IDL_PARAM_DCL */

	/* classify the parameters. */
	bool explicit_u[64];
	for(int i=0; i<64; i++) explicit_u[i] = false;
	unsigned long max_fixreg = 0;
	int inline_seq_alloc = 0;
	for(int i=0; i<num_params; i++) {
		IDL_tree p = params[i],
			type = get_type_spec(IDL_PARAM_DCL(p).param_type_spec),
			ident = IDL_PARAM_DCL(p).simple_declarator;
		const char *name = IDL_IDENT(ident).str;
		if(is_value_type(type)) {
			struct untyped_param *u = g_new(struct untyped_param, 1);
			u->name = name;
			u->type = type;
			u->param_dcl = p;

			unsigned long mr_n = 0;
			if(!get_ul_property(&mr_n, ident, "MR")) goto fail;
			u->reg_manual = mr_n > 0;
			if(u->reg_manual) {
				if(mr_n > 63) {
					fprintf(stderr, "%s: MR(%lu) too large\n", __FUNCTION__,
						mr_n);
					goto fail;
				}
				if(explicit_u[mr_n]) {
					fprintf(stderr, "%s: MR(%lu) specified twice\n", __FUNCTION__,
						mr_n);
					goto fail;
				}
				explicit_u[mr_n] = true;
				u->first_reg = mr_n;
				u->last_reg = mr_n;
				max_fixreg = MAX(max_fixreg, mr_n);
			} else {
				u->first_reg = 0;
				u->last_reg = 0;
			}

			untyped = g_list_prepend(untyped, u);
		} else if(is_bounded_seq(type)) {
			IDL_tree subtype = get_type_spec(
					IDL_TYPE_SEQUENCE(type).simple_type_spec),
				bound = IDL_TYPE_SEQUENCE(type).positive_int_const;
			assert(bound != NULL);
			int epw = BITS_PER_WORD / size_in_bits(subtype),
				max_words = (IDL_INTEGER(bound).value + epw - 1) / epw;
			/* FIXME: be smarter about allocating these. there should be a
			 * proper strategy and a nice policy.
			 */
			if(is_value_type(subtype) && inline_seq_alloc + max_words <= 48) {
				struct seq_param *s = g_new(struct seq_param, 1);
				s->name = name;
				s->max_elems = IDL_INTEGER(bound).value;
				s->param_dcl = p;
				s->elem_type = subtype;
				s->bits_per_elem = size_in_bits(s->elem_type);
				s->elems_per_word = epw;
				s->min_words = 0;
				s->max_words = max_words;
				seq = g_list_prepend(seq, s);
				inline_seq_alloc += max_words;
			} else {
				/* it's a long sequence then. */
				_long = g_list_prepend(_long, p);
			}
		} else {
			/* presume it's a long type or something... */
			_long = g_list_prepend(_long, p);
		}
	}
	untyped = g_list_reverse(untyped);
	seq = g_list_reverse(seq);
	_long = g_list_reverse(_long);

	const int num_long = g_list_length(_long);
	inf = g_malloc(sizeof(struct message_info)
		+ sizeof(struct long_param) * num_long);

	/* assign untyped words. */
	inf->num_untyped = g_list_length(untyped);
	inf->untyped = g_new(struct untyped_param *, inf->num_untyped);
	int next_mr = 1, pos = 0;
	/* TODO: handle "return_type"! */
	for(GList *cur = g_list_first(untyped);
		cur != NULL;
		cur = g_list_next(cur))
	{
		struct untyped_param *u = cur->data;
		inf->untyped[pos++] = u;
		if(u->first_reg != 0 || u->last_reg != 0) continue;

		/* TODO: this doesn't properly account for multi-word value types,
		 * such as structs that are encoded as untyped words.
		 */
		while(next_mr <= 63 && explicit_u[next_mr]) next_mr++;
		if(next_mr > 63) {
			fprintf(stderr, "%s: ran out of untyped registers\n",
				__FUNCTION__);
			goto fail;
		}
		u->first_reg = next_mr;
		u->last_reg = next_mr;
		next_mr++;
	}
	inf->untyped_words = next_mr - 1;

	/* allocate inline sequences. */
	inf->num_inline_seq = g_list_length(seq);
	inf->seq = g_new(struct seq_param *, inf->num_inline_seq);
	pos = 0;
	int min_mr = next_mr - 1, max_mr = next_mr - 1;
	for(GList *cur = g_list_first(seq);
		cur != NULL;
		cur = g_list_next(cur))
	{
		struct seq_param *s = cur->data;
		inf->seq[pos++] = s;

		if(g_list_next(cur) != NULL) {
			/* inline sequences before the last require an explicit length
			 * word.
			 */
			s->min_words++;
			s->max_words++;
		}

		if(min_mr + s->min_words > 63 || max_mr + s->max_words > 63) {
			fprintf(stderr, "%s: sequence of [%d..%d] words %s fit\n",
				__FUNCTION__, (int)s->min_words, (int)s->max_words,
				min_mr + s->min_words > 63 ? "won't" : "might not");
		}

		min_mr += s->min_words;
		max_mr += s->max_words;
	}

	/* long types: non-rigid and large structs, strings, wide strings,
	 * sequences that can't be encoded inline, etc
	 */
	inf->num_long = 0;
	GList *cur = g_list_first(_long);
	for(int i=0; i<num_long; i++, cur = g_list_next(cur)) {
		assert(cur != NULL);
		IDL_tree param = cur->data,
			type = get_type_spec(IDL_PARAM_DCL(param).param_type_spec),
			decl = IDL_PARAM_DCL(param).simple_declarator;

		struct long_param *p = g_new(struct long_param, 1);
		p->name = IDL_IDENT(decl).str;
		p->type = type;
		p->param_dcl = param;
		inf->long_params[inf->num_long++] = p;
	}

	g_list_free(untyped);
	g_list_free(seq);
	g_list_free(_long);
	return inf;

fail:
	list_dispose(untyped);
	list_dispose(seq);
	g_list_free(_long);
	g_free(inf->untyped);
	g_free(inf->seq);
	g_free(inf);
	return NULL;
}


static struct message_info *build_exception_message(IDL_tree exn)
{
	/* FIXME */
#if 0
	fprintf(stderr, "would build a credible exception message for `%s' here\n",
		IDL_IDENT_REPO_ID(IDL_EXCEPT_DCL(exn).ident));
#endif

	struct message_info *msg = g_malloc0(sizeof(struct message_info)
		+ 0 * sizeof(IDL_tree));
	msg->tagmask = NO_TAGMASK;
	msg->node = exn;

	return msg;
}


/* bump register offsets up by one. fails and returns false when either MR0 is
 * specified in a parameter attribute, or when the message would have been too
 * large (i.e. more than 62 words).
 */
static bool sublabel_bump(struct message_info *req, GError **error_p)
{
	assert(req->sublabel != NO_SUBLABEL);
	assert(IDL_NODE_TYPE(req->node) == IDLN_OP_DCL);

	if(req->untyped_words > 62) {
		g_set_error(error_p, 0, 0,
			"sublabel_bump: message won't fit in 63 words (%d given)",
			req->untyped_words);
		return false;
	}
	for(int i=0; i<req->num_untyped; i++) {
		struct untyped_param *u = req->untyped[i];
		if(u->reg_manual && (u->first_reg == 0 || u->last_reg == 0)) {
			g_set_error(error_p, 0, 0,
				"sublabel_bump: register 0 assigned manually");
			return false;
		}
	}

	for(int i=0; i<req->num_untyped; i++) {
		struct untyped_param *u = req->untyped[i];
		/* (untyped_words is a MsgWord offset, therefore +1.) */
		assert(u->first_reg < req->untyped_words + 1);
		assert(u->last_reg < req->untyped_words + 1);
		u->first_reg++;
		u->last_reg++;
	}
	req->untyped_words++;

	return true;
}


/* TODO: get the number and max dimensions of the string buffers we'll
 * send/receive once long types are implemented.
 */
struct method_info *analyse_op_dcl(
	struct print_ctx *pr,
	IDL_tree method)
{
	int num_replies;
	if(IDL_OP_DCL(method).f_oneway) num_replies = 0;
	else num_replies = 1 + IDL_list_length(IDL_OP_DCL(method).raises_expr);

	IDL_tree param_list = IDL_OP_DCL(method).parameter_dcls,
		return_type = get_type_spec(IDL_OP_DCL(method).op_type_spec);

	struct method_info *inf = g_malloc0(sizeof(struct method_info)
		+ sizeof(struct message_info *) * num_replies);
	inf->vtab_offset = -1;
	inf->node = method;
	inf->name = IDL_IDENT(IDL_OP_DCL(method).ident).str;
	inf->num_reply_msgs = num_replies;
	inf->return_type = return_type;

	/* the request message consists of in-parameters and the in-halves of inout
	 * parameters.
	 */
	IDL_tree pbuf[IDL_list_length(param_list) + 1];
	int nparms = 0;
	for(IDL_tree cur = param_list; cur != NULL; cur = IDL_LIST(cur).next) {
		IDL_tree param = IDL_LIST(cur).data;
		if(IDL_PARAM_DCL(param).attr != IDL_PARAM_OUT) {
			pbuf[nparms++] = param;
		}
	}
	inf->request = build_message(NULL, pbuf, nparms);
	if(inf->request == NULL) goto fail;
	inf->request->node = method;
	if(!get_msg_label(inf->request, IDL_OP_DCL(method).ident)) {
		/* FIXME: assign a label somehow, blow up only if that fails */
		fprintf(stderr, "error: can't assign automatic label to `%s'\n",
			METHOD_NAME(method));
		goto fail;
	}
	if(inf->request->sublabel != NO_SUBLABEL) {
		GError *err = NULL;
		if(!sublabel_bump(inf->request, &err)) {
			fprintf(stderr, "%s (op was `%s')\n", err->message,
				METHOD_NAME(method));
			g_error_free(err);
			goto fail;
		}
	}

	if(num_replies > 0) {
		/* the usual reply: return type and out, inout out-halves */
		nparms = 0;
		for(IDL_tree c = param_list; c != NULL; c = IDL_LIST(c).next) {
			IDL_tree param = IDL_LIST(c).data;
			if(IDL_PARAM_DCL(param).attr != IDL_PARAM_IN) {
				pbuf[nparms++] = param;
			}
		}
		inf->replies[0] = build_message(return_type, pbuf, nparms);
		inf->replies[0]->node = method;
		inf->replies[0]->label = 0;
		inf->replies[0]->tagmask = NO_TAGMASK;
	}

	if(num_replies > 1) {
		/* exceptions. */
		IDL_tree raises = IDL_OP_DCL(method).raises_expr;
		int expos = 1;
		for(IDL_tree c = raises; c != NULL; c = IDL_LIST(c).next) {
			assert(expos < num_replies); /* guaranteed by IDL_list_length() */
			IDL_tree propnode = IDL_LIST(c).data,
				exn = IDL_get_parent_node(propnode, IDLN_EXCEPT_DCL, NULL);
			inf->replies[expos] = build_exception_message(exn);
			if(!get_msg_label(inf->replies[expos], propnode)) {
				/* exceptions, however, always need to be marked. */
				fprintf(stderr, "exception `%s' doesn't have a Label property!\n",
					IDL_IDENT_REPO_ID(propnode));
				goto fail;
			}
			expos++;
		}
		assert(expos == num_replies);
	}

	/* check that there is at most one NegativeReturn exception per
	 * operation.
	 */
	IDL_tree nr_ex = NULL, raises_expr = IDL_OP_DCL(method).raises_expr;
	for(IDL_tree cur = raises_expr; cur != NULL; cur = IDL_LIST(cur).next) {
		IDL_tree ex = IDL_get_parent_node(IDL_LIST(cur).data,
			IDLN_EXCEPT_DCL, NULL);
		if(is_negs_exn(ex)) {
			if(nr_ex == NULL) nr_ex = ex;
			else {
				fprintf(stderr,
					"operation `%s' raises two NegativeReturn exceptions:\n"
					"`%s' and `%s'. at most one is permitted for each operation.\n",
					METHOD_NAME(method), EXN_REPO_ID(nr_ex), EXN_REPO_ID(ex));
				goto fail;
			}
		}
	}

	/* oneway restrictions. */
	if(IDL_OP_DCL(method).f_oneway) {
		if(return_type != NULL) {
			fprintf(stderr, "can't have non-void return type for oneway operation `%s'\n",
				METHOD_NAME(method));
			return false;
		}
		if(raises_expr != NULL) {
			fprintf(stderr, "can't have exceptions for a oneway operation `%s'\n",
				METHOD_NAME(method));
			return false;
		}
	}

	/* if a NegativeReturn exception is declared, check that the return type is
	 * either void, unambiguously returnable (octet, ushort), or something we'd
	 * return as an outparameter.
	 */
	if(nr_ex != NULL) {
		const bool valid_neg = (return_type == NULL
			|| is_real_nre_return_type(return_type)
			|| !is_value_type(return_type));
		if(!valid_neg) {
			fprintf(stderr,
				"return type for a NegativeReturn raising operation must be\n"
				"void, an unsigned short, an octet, or a non-value type.\n");
			goto fail;
		}
	}

	return inf;

fail:
	free_message_info(inf->request);
	for(int i=0; i<inf->num_reply_msgs; i++) {
		free_message_info(inf->replies[i]);
	}
	g_free(inf);
	return NULL;
}


static int method_info_by_label_cmp(gconstpointer ap, gconstpointer bp)
{
	const struct method_info *amsg = ap, *bmsg = bp;
	const struct message_info *a = amsg->request, *b = bmsg->request;
	if(a->label < b->label) return -1;
	else if(a->label > b->label) return 1;
	else if(a->sublabel < b->sublabel) return -1;
	else if(a->sublabel > b->sublabel) return 1;
	else return 0;
}


GList *analyse_methods_of_iface(
	struct print_ctx *pr,
	GList **tagmask_list_p,
	IDL_tree iface)
{
	GList *methods = all_methods_of_iface(pr->ns, iface);
	int vtab_pos = 0;
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

		inf->vtab_offset = vtab_pos++;

		cur->data = inf;
		if(inf->request->tagmask != NO_TAGMASK) {
			*tagmask_list_p = g_list_prepend(*tagmask_list_p, inf);
		}
	}
	*tagmask_list_p = g_list_reverse(*tagmask_list_p);
	return g_list_sort(methods, &method_info_by_label_cmp);
}


struct stritem_info *dispatcher_stritems(GList *method_info_list)
{
	GArray *result = g_array_new(FALSE, FALSE, sizeof(struct stritem_info));
	for(GList *cur = g_list_first(method_info_list);
		cur != NULL;
		cur = g_list_next(cur))
	{
		struct method_info *inf = cur->data;
		struct message_info *req = inf->request;

		for(int i=0; i<req->num_long; i++) {
			struct long_param *p = req->long_params[i];
			assert(IDL_PARAM_DCL(p->param_dcl).attr != IDL_PARAM_OUT);
			int length = max_size(p->type);
			bool stringlike = IDL_NODE_TYPE(p->type) == IDLN_TYPE_STRING
				|| IDL_NODE_TYPE(p->type) == IDLN_TYPE_WIDE_STRING;
			struct stritem_info *si;
			if(result->len <= i) {
				struct stritem_info tmp = {
					.length = 0,
					.stringlike = false,
				};
				g_array_append_val(result, tmp);
				si = &g_array_index(result, struct stritem_info,
					result->len - 1);
				assert(si == &g_array_index(result, struct stritem_info, i));
			} else {
				si = &g_array_index(result, struct stritem_info, i);
			}
			if(!si->stringlike && stringlike) si->stringlike = true;
			si->length = MAX(si->length, length);
		}
	}

	if(result->len > 0) {
		/* terminate and return. */
		struct stritem_info tmp = { .length = -1 };
		g_array_append_val(result, tmp);
		return (void *)g_array_free(result, FALSE);
	} else {
		g_array_free(result, TRUE);
		return NULL;
	}
}
