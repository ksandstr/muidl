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


/* the size of a type's encoding in words, when encoded as a single
 * parameter or member of an array parameter.
 */
static int size_in_words(IDL_tree type);
static int array_size_in_words(IDL_tree type_array, IDL_tree dcl);
static int struct_size_in_words(IDL_tree type_struct);


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


static bool op_has_sublabel(IDL_tree prop_node)
{
	IDL_tree iface = IDL_get_parent_node(prop_node, IDLN_INTERFACE, NULL);
	/* exceptions, among other things, can appear outside interfaces.
	 * fortunately sublabels don't apply to them.
	 */
	if(iface == NULL) return false;
	else {
		IDL_tree ifprop = IDL_INTERFACE(iface).ident;
		unsigned long ifacelabel = 0;
		return get_ul_property(&ifacelabel, ifprop, "IfaceLabel")
			&& ifacelabel != 0;
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
			assert(op_has_sublabel(prop_node));
			inf->sublabel = inf->label;
			inf->label = ifacelabel;
		} else {
			assert(!op_has_sublabel(prop_node));
			inf->sublabel = NO_SUBLABEL;
		}
		/* TODO: disallow tagmasks alongside ifacelabels. */
	}

	return true;
}


static int size_in_bits(IDL_tree type)
{
	assert(is_rigid_type(NULL, type));

	switch(IDL_NODE_TYPE(type)) {
		case IDLN_TYPE_INTEGER: {
			static const int8_t bits_per[] = {
				[IDL_INTEGER_TYPE_SHORT] = 16,
				[IDL_INTEGER_TYPE_LONG] = 32,
				[IDL_INTEGER_TYPE_LONGLONG] = 64,
			};
			enum IDL_integer_type ityp = IDL_TYPE_INTEGER(type).f_type;
			assert(ityp >= 0 && ityp < G_N_ELEMENTS(bits_per));
			return bits_per[(int)ityp];
		}

		case IDLN_NATIVE:
			if(IS_WORD_TYPE(type) || IS_FPAGE_TYPE(type)
				|| IS_TIME_TYPE(type))
			{
				return BITS_PER_WORD;
			} else if(IS_MAPGRANT_TYPE(type)) {
				return BITS_PER_WORD * 2;
			} else {
				NOTDEFINED(type);
			}

		case IDLN_TYPE_OCTET:
		case IDLN_TYPE_CHAR:
			return 8;

		case IDLN_TYPE_BOOLEAN: return 1;
		case IDLN_TYPE_WIDE_CHAR: return 32;

		case IDLN_TYPE_ARRAY:
			return BITS_PER_WORD * array_size_in_words(
				get_array_type(type), type);

		case IDLN_TYPE_STRUCT:
			/* FIXME: array-member structs are supposed to be bitpacked in v1.
			 * compute actual bit length like struct_size_in_words() does for
			 * words.
			 */
			return BITS_PER_WORD * struct_size_in_words(type);

		case IDLN_TYPE_ENUM:
		case IDLN_TYPE_FLOAT:
			/* TODO */

		default:
			NOTDEFINED(type);
	}
}


static int array_size_in_words(IDL_tree type, IDL_tree dcl)
{
	IDL_tree size_list = IDL_TYPE_ARRAY(dcl).size_list;
	assert(IDL_list_length(size_list) == 1);

	int len = IDL_INTEGER(IDL_LIST(size_list).data).value,
		sib = size_in_bits(type);
	if(sib <= BITS_PER_WORD) {
		/* short types. */
		int epw = BITS_PER_WORD / sib;
		assert(epw > 0);
		return (len + epw - 1) / epw;
	} else {
		/* long types. */
		return len * size_in_words(type);
	}
}


/* a struct's size in words. see size_in_words() for explanation.
 *
 * this assumes the struct's members aren't encoded to minimize overhead (which
 * is a v2 feature).
 */
static int struct_size_in_words(IDL_tree type)
{
	assert(IDL_NODE_TYPE(type) == IDLN_TYPE_STRUCT);

	/* (optimization: could cache the result by repo id, since this function
	 * will likely be invoked a few times for each of the known structs.)
	 */
	int size = 0;
	for(IDL_tree cur = IDL_TYPE_STRUCT(type).member_list;
		cur != NULL;
		cur = IDL_LIST(cur).next)
	{
		IDL_tree data = IDL_LIST(cur).data;
		assert(IDL_NODE_TYPE(data) == IDLN_MEMBER);
		IDL_tree mtype = get_type_spec(IDL_MEMBER(data).type_spec);
		for(IDL_tree dcl_cur = IDL_MEMBER(data).dcls;
			dcl_cur != NULL;
			dcl_cur = IDL_LIST(dcl_cur).next)
		{
			IDL_tree dcl = IDL_LIST(dcl_cur).data;
			if(IDL_NODE_TYPE(dcl) == IDLN_IDENT) size += size_in_words(mtype);
			else if(IDL_NODE_TYPE(dcl) == IDLN_TYPE_ARRAY) {
				size += array_size_in_words(mtype, dcl);
			} else {
				NOTDEFINED(dcl);
			}
		}
	}
	fprintf(stderr, "%s: total size of `%s' is %d words.\n",
		__func__, IDL_IDENT(IDL_TYPE_STRUCT(type).ident).repo_id,
		size);
	return size;
}


static int size_in_words(IDL_tree type)
{
	switch(IDL_NODE_TYPE(type)) {
		case IDLN_TYPE_INTEGER:
			if(IDL_TYPE_INTEGER(type).f_type == IDL_INTEGER_TYPE_LONGLONG
				&& BITS_PER_WORD == 32)
			{
				return 2;
			} else {
				return 1;
			}

		case IDLN_TYPE_FLOAT:
			/* FIXME */
			NOTDEFINED(type);

		case IDLN_TYPE_BOOLEAN:
		case IDLN_TYPE_OCTET:
		case IDLN_TYPE_CHAR:
		case IDLN_TYPE_WIDE_CHAR:
		case IDLN_TYPE_ENUM:
			return 1;

		case IDLN_NATIVE:
			if(IS_WORD_TYPE(type) || IS_FPAGE_TYPE(type)
				|| IS_TIME_TYPE(type))
			{
				return 1;
			} else if(IS_MAPGRANT_TYPE(type)) {
				return 2;
			} else {
				NOTDEFINED(type);
			}

		case IDLN_TYPE_STRUCT:
			return struct_size_in_words(type);

		case IDLN_TYPE_ARRAY:
			return array_size_in_words(get_array_type(type), type);

		case IDLN_TYPE_UNION:
			/* TODO */
			NOTDEFINED(type);

		default:
			/* sequences, strings, bad things */
			NOTDEFINED(type);
	}
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


static struct untyped_param *new_untyped(
	const char *name,
	IDL_tree type,
	IDL_tree param,
	int param_ix)
{
	struct untyped_param *u = g_new(struct untyped_param, 1);
	u->name = name;
	u->type = type;
	u->param_dcl = param;
	u->param_ix = param_ix;
	u->arg_ix = -1;
	u->first_reg = 0;
	u->last_reg = 0;
	return u;
}


static struct seq_param *new_inline_seq(
	const char *name,
	IDL_tree type,
	IDL_tree subtype,
	IDL_tree param,
	int param_ix)
{
	IDL_tree bound = IDL_TYPE_SEQUENCE(type).positive_int_const;
	assert(bound != NULL);
	int bpe = size_in_bits(subtype), epw = BITS_PER_WORD / bpe,
		max_words = (IDL_INTEGER(bound).value + epw - 1) / epw;
	struct seq_param *s = g_new(struct seq_param, 1);
	s->name = name;
	s->max_elems = IDL_INTEGER(bound).value;
	s->param_dcl = param;
	s->elem_type = subtype;
	s->bits_per_elem = bpe;
	s->elems_per_word = epw;
	s->min_words = 0;
	s->max_words = max_words;
	s->param_ix = param_ix;
	s->arg_ix = -1;
	return s;
}


static struct long_param *new_long_param(
	const char *name,
	IDL_tree type,
	IDL_tree p,
	int param_ix)
{
	struct long_param *l = g_new(struct long_param, 1);
	l->name = name;
	l->type = type;
	l->param_dcl = p;
	l->param_ix = param_ix;
	l->arg_ix = -1;
	return l;
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
	int num_params,
	bool has_sublabel)
{
	struct message_info *inf = NULL;
	GList *untyped = NULL, *seq = NULL,	/* struct [name]_param * */
		*_long = NULL;	/* struct long_param * */

	/* classify parameters and make structures. */
	/* TODO: handle "return_type"! */
	int arg_pos = 0;
	for(int i=0; i<num_params; i++) {
		IDL_tree p = params[i],
			type = get_type_spec(IDL_PARAM_DCL(p).param_type_spec),
			ident = IDL_PARAM_DCL(p).simple_declarator;
		const char *name = IDL_IDENT(ident).str;
		if(is_rigid_type(NULL, type)) {
			/* this may include overlong items. */
			struct untyped_param *u = new_untyped(name, type, p, i);
			u->arg_ix = arg_pos++;
			untyped = g_list_prepend(untyped, u);
		} else if(is_bounded_seq(type)) {
			IDL_tree subtype = get_type_spec(
				IDL_TYPE_SEQUENCE(type).simple_type_spec);
			assert(is_rigid_type(NULL, subtype));
			/* this, too, will include overlong items. */
			struct seq_param *s = new_inline_seq(name, type, subtype, p, i);
			s->arg_ix = arg_pos;
			arg_pos += 2;
			seq = g_list_prepend(seq, s);
		} else {
			/* everything else is passed as long items. */
			struct long_param *l = new_long_param(name, type, p, i);
			l->arg_ix = arg_pos;
			arg_pos += 2;
			_long = g_list_prepend(_long, l);
		}
	}
	untyped = g_list_reverse(untyped);
	seq = g_list_reverse(seq);
	_long = g_list_reverse(_long);

	/* assign untyped words. also figure out how many typed and untyped words
	 * we're going to use, before inline sequences are allocated.
	 */
	bool reg_in_use[64];
	for(int i=1; i<64; i++) reg_in_use[i] = false;
	if(has_sublabel) reg_in_use[1] = true;
	/* those with explicit MR(%d) specs, first. */
	for(GList *cur = g_list_first(untyped);
		cur != NULL;
		cur = g_list_next(cur))
	{
		struct untyped_param *u = cur->data;
		IDL_tree ident = IDL_PARAM_DCL(u->param_dcl).simple_declarator;
		unsigned long mr_n = 0;
		if(!get_ul_property(&mr_n, ident, "MR")) goto fail;
		u->reg_manual = mr_n > 0;
		if(!u->reg_manual) continue;
		if(size_in_words(u->type) > 1) {
			fprintf(stderr, "%s: mr spec attribute not valid for <%s> (size %d words)\n",
				__func__, IDL_NODE_TYPE_NAME(u->type), size_in_words(u->type));
			goto fail;
		}
		if(mr_n > 63) {
			fprintf(stderr, "%s: MR(%lu) too large\n", __FUNCTION__, mr_n);
			goto fail;
		}
		if(mr_n == 1 && has_sublabel) {
			fprintf(stderr, "%s: can't specify MR(1) with sublabel\n",
				__func__);
			goto fail;
		}
		if(reg_in_use[mr_n]) {
			fprintf(stderr, "%s: MR(%lu) specified twice\n", __func__, mr_n);
			goto fail;
		}
		reg_in_use[mr_n] = true;
		u->first_reg = mr_n;
		u->last_reg = mr_n;
	}
	/* then the non-compound types (since there is no alternative encoding for
	 * them)
	 */
	int next_u = has_sublabel ? 2 : 1, num_compound = 0;
	for(GList *cur = g_list_first(untyped);
		cur != NULL;
		cur = g_list_next(cur))
	{
		struct untyped_param *u = cur->data;
		if(u->reg_manual) continue;
		if(!is_value_type(u->type)) {
			num_compound++;
			continue;
		}

		int size = size_in_words(u->type);
		while(next_u < 64) {
			int span = 0;
			while(span < size && next_u + span < 64
				&& !reg_in_use[next_u + span])
			{
				span++;
			}
			if(span == size) break;
			next_u++;
		}
		if(next_u == 64) {
			/* TODO: come up with a better error message. this occurred in the
			 * fixed part of the message, and therefore is an user error which
			 * should be explained.
			 */
			fprintf(stderr, "%s: untyped item won't fit!\n", __func__);
			abort();
		}
		u->first_reg = next_u;
		u->last_reg = next_u + size - 1;
		next_u += size;
		for(int i=u->first_reg; i<=u->last_reg; i++) {
			assert(!reg_in_use[i]);
			reg_in_use[i] = true;
		}
	}
	/* finally those parameters of compound type that we can squeeze in. this
	 * algorithm is simple for repeatability's sake; a solution to the knapsack
	 * problem would have to be proven optimal to fit as well. (v2 todo?)
	 */
	inf = g_new0(struct message_info, 1);		/* 0'd for g_free() safety */
	int num_seq = g_list_length(seq), num_long = g_list_length(_long);

	for(GList *cur = g_list_first(untyped), *next;
		cur != NULL;
		cur = next)
	{
		next = g_list_next(cur);
		struct untyped_param *u = cur->data;
		if(u->reg_manual || is_value_type(u->type)) continue;
		assert(is_rigid_type(NULL, u->type));
		num_compound--;
		assert(num_compound >= 0);
		/* accounts for typed items in the case that the following compound
		 * items and all inline sequences won't fit, and for long items.
		 */
		const int space = 64 - (num_compound + num_seq + num_long) * 2 - next_u,
			size = size_in_words(u->type);
		/* first-fit. */
		int start = has_sublabel ? 2 : 1;
		while(start + size < space) {
			int span = 0;
			while(span < size && start + span < space
				&& !reg_in_use[start + span])
			{
				span++;
			}
			if(span == size) break;
			start++;
		}
		if(start + size >= space) {
			/* can't fit. make into a hat. */
			struct long_param *l = new_long_param(u->name, u->type,
				u->param_dcl, u->param_ix);
			l->arg_ix = arg_pos;
			arg_pos += 2;
			_long = g_list_append(_long, l);
			g_free(u);
			untyped = g_list_delete_link(untyped, cur);
		} else {
			/* place. */
			u->first_reg = start;
			u->last_reg = start + size - 1;
			for(int i=u->first_reg; i<=u->last_reg; i++) {
				assert(!reg_in_use[i]);
				reg_in_use[i] = true;
			}
		}
	}

	inf->num_untyped = g_list_length(untyped);
	inf->untyped = g_new(struct untyped_param *, inf->num_untyped);
	int pos = 0;
	for(GList *cur = g_list_first(untyped);
		cur != NULL;
		cur = g_list_next(cur))
	{
		struct untyped_param *u = cur->data;
		inf->untyped[pos++] = u;
	}
	inf->tag_u = has_sublabel ? 1 : 0;
	for(int i=1; i<64; i++) {
		if(reg_in_use[i]) inf->tag_u = i;
	}


#if 0
	/* allocate inline sequences.
	 * (v2 todo: this, too, is really a knapsack problem. we'd want to get as
	 * few string transfers as possible. but the algorithm would have to
	 * provably output the optimal solution.)
	 */
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
	inf->num_long = num_long;
	inf->long_params = g_new(struct long_param *, num_long);
	GList *cur = g_list_first(_long);
	for(int i=0; i<num_long; i++, cur = g_list_next(cur)) {
		assert(cur != NULL);
		struct long_param *l = cur->data;
		IDL_tree param = l->param_dcl, type = l->type,
			decl = IDL_PARAM_DCL(param).simple_declarator;

		struct long_param *p = g_new(struct long_param, 1);
		p->name = IDL_IDENT(decl).str;
		p->type = type;
		p->param_dcl = param;
		inf->long_params[inf->num_long++] = p;
	}
#endif

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
	g_free(inf->long_params);
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

	if(req->tag_u > 62) {
		g_set_error(error_p, 0, 0,
			"sublabel_bump: message won't fit in 63 words (%d given)",
			req->tag_u);
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
		assert(u->first_reg < req->tag_u + 1);
		assert(u->last_reg < req->tag_u + 1);
		u->first_reg++;
		u->last_reg++;
	}
	req->tag_u++;

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
	inf->request = build_message(NULL, pbuf, nparms, op_has_sublabel(method));
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
		inf->replies[0] = build_message(return_type, pbuf, nparms,
			op_has_sublabel(method));
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
