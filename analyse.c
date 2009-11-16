/*
 * analyse.c -- analysis of IDL elements
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
			if(!get_ul_property(&mr_n, ident, "MR")) return false;
			if(mr_n > 0) {
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
		} else if(is_bounded_seq(type)
			&& is_value_type(get_type_spec(IDL_TYPE_SEQUENCE(type).simple_type_spec)))
		{
			struct seq_param *s = g_new(struct seq_param, 1);
			s->name = name;
			s->max_elems = IDL_INTEGER(IDL_TYPE_SEQUENCE(type).positive_int_const).value;
			s->elem_type = get_type_spec(IDL_TYPE_SEQUENCE(type).simple_type_spec);
			s->bits_per_elem = size_in_bits(s->elem_type);
			s->elems_per_word = BITS_PER_WORD / s->bits_per_elem;
			s->min_words = 0;
			s->max_words = (s->max_elems + s->elems_per_word - 1) / s->elems_per_word;
			seq = g_list_prepend(seq, s);
		} else {
			/* presume it's a long type or something... */
			_long = g_list_prepend(_long, p);
		}
	}
	untyped = g_list_reverse(untyped);
	seq = g_list_reverse(seq);
	_long = g_list_reverse(_long);

	inf = g_malloc(sizeof(struct message_info)
		+ sizeof(IDL_tree) * g_list_length(_long));

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

	/* allocate sequences. */
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

	if(g_list_first(_long) != NULL) {
		fprintf(stderr, "%s: can't handle long types\n", __FUNCTION__);
		goto fail;
	}
	inf->num_long = 0;

	g_list_free(untyped);
	g_list_free(seq);
	g_list_free(_long);
	return inf;

fail:
	list_dispose(untyped);
	list_dispose(seq);
	list_dispose(_long);
	g_free(inf->untyped);
	g_free(inf->seq);
	g_free(inf);
	return NULL;
}


static struct message_info *build_exception_message(IDL_tree exn)
{
	/* FIXME */
	fprintf(stderr, "would build a credible exception message for `%s' here\n",
		IDL_IDENT_REPO_ID(IDL_EXCEPT_DCL(exn).ident));

	struct message_info *msg = g_malloc0(sizeof(struct message_info)
		+ 0 * sizeof(IDL_tree));
	msg->tagmask = NO_TAGMASK;
	msg->node = exn;

	return msg;
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
	inf->node = method;
	if(!get_msg_label(inf->request, IDL_OP_DCL(method).ident)) {
		/* FIXME: assign a label somehow, blow up only if that fails */
		fprintf(stderr, "error: can't assign automatic label to `%s'\n",
			METHOD_NAME(method));
		goto fail;
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
	 * appropriate.
	 */
	if(nr_ex != NULL) {
		bool valid_neg = (return_type == NULL	/* void is OK */
			|| IS_USHORT_TYPE(return_type)		/* unsigned short, too */
			|| IDL_NODE_TYPE(return_type) == IDLN_TYPE_OCTET	/* octet also */
			|| !is_value_type(return_type));	/* types we'd outparm anyway */
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
