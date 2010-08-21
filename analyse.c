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


int size_in_bits(IDL_tree type)
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

		case IDLN_TYPE_STRUCT: {
			/* only structs that're shorter than a single word can be packed in
			 * the same word with another value of a rigid type. that is to
			 * say, structs the same size or larger than a single word may
			 * result in up to BITS_PER_WORD - 1 wasted space.
			 */
			const struct packed_format *pf = packed_format_of(type);
			if(pf == NULL) return -1;
			if(pf->num_bits < BITS_PER_WORD) return pf->num_bits;
			else return pf->num_words * BITS_PER_WORD;
		}

		case IDLN_TYPE_ENUM:
		case IDLN_TYPE_FLOAT:
			/* TODO */

		default:
			NOTDEFINED(type);
	}
}


/* NOTE: this function is only defined for types that can be encoded as untyped
 * words. structs larger than 63 words will pop blow that assert down there.
 */
static int array_size_in_words(IDL_tree type, IDL_tree dcl)
{
	IDL_tree size_list = IDL_TYPE_ARRAY(dcl).size_list;
	assert(IDL_list_length(size_list) == 1);

	int len = IDL_INTEGER(IDL_LIST(size_list).data).value,
		sib = size_in_bits(type);
	assert(sib > 0);
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


/* a struct's size in words. see size_in_words() for explanation. */
static int struct_size_in_words(IDL_tree type)
{
	assert(IDL_NODE_TYPE(type) == IDLN_TYPE_STRUCT);

	const struct packed_format *fmt = packed_format_of(type);
	if(fmt == NULL) {
		/* TODO: return -1 once packed_format_of() returns NULL for them */
		fprintf(stderr, "%s: not implemented for non-packable structs\n",
			__func__);
		abort();
	}

	fprintf(stderr, "%s: total size of `%s' is %d words (%d bits).\n",
		__func__, IDL_IDENT(IDL_TYPE_STRUCT(type).ident).repo_id,
		fmt->num_words, fmt->num_bits);
	return fmt->num_words;
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


static struct msg_param *new_untyped(
	const char *name,
	IDL_tree type,
	IDL_tree param,
	int param_ix)
{
	struct msg_param *u = g_new0(struct msg_param, 1);
	u->kind = P_UNTYPED;
	u->name = name;
	u->param_dcl = param;
	u->param_ix = param_ix;
	u->arg_ix = -1;
	u->X.untyped.type = type;
	u->X.untyped.first_reg = 0;
	u->X.untyped.last_reg = 0;
	return u;
}


static struct msg_param *new_inline_seq(
	const char *name,
	IDL_tree type,
	IDL_tree subtype,
	IDL_tree param,
	int param_ix)
{
	IDL_tree bound = IDL_TYPE_SEQUENCE(type).positive_int_const;
	assert(bound != NULL);
	int bpe = size_in_bits(subtype);
	assert(bpe > 0);
	int epw = BITS_PER_WORD / bpe,
		max_words = (IDL_INTEGER(bound).value + epw - 1) / epw;
	struct msg_param *s = g_new0(struct msg_param, 1);
	s->kind = P_SEQ;
	s->name = name;
	s->param_dcl = param;
	s->param_ix = param_ix;
	s->arg_ix = -1;
	s->X.seq.max_elems = IDL_INTEGER(bound).value;
	s->X.seq.elem_type = subtype;
	s->X.seq.bits_per_elem = bpe;
	s->X.seq.elems_per_word = epw;
	s->X.seq.min_words = 0;
	s->X.seq.max_words = max_words;
	return s;
}


static struct msg_param *new_long_param(
	const char *name,
	IDL_tree type,
	IDL_tree p,
	int param_ix)
{
	struct msg_param *l = g_new0(struct msg_param, 1);
	l->kind = P_LONG;
	l->name = name;
	l->param_dcl = p;
	l->param_ix = param_ix;
	l->arg_ix = -1;
	l->X._long.type = type;
	return l;
}


static bool is_bounded_seq(IDL_tree type)
{
	return IDL_NODE_TYPE(type) == IDLN_TYPE_SEQUENCE
		&& IDL_TYPE_SEQUENCE(type).positive_int_const != NULL
		&& IDL_INTEGER(IDL_TYPE_SEQUENCE(type).positive_int_const).value > 0;
}


/* turn a parameter list into lists of untyped, inline-sequence and long types.
 * the first have fixed register assignments, inline sequences have register
 * ranges and long types appear in transmission order.
 */
static struct message_info *build_message(
	IDL_tree return_type,		/* separate because not IDL_PARAM_DCL */
	IDL_tree param_list,
	bool has_sublabel,
	bool is_outhalf)
{
	struct message_info *inf = NULL;
	/* of <struct msg_param *>, freed by "params" */
	GList *params = NULL, *untyped = NULL, *seq = NULL, *_long = NULL;

	/* classify parameters and construct msg_param structures for them. */
	int arg_pos = 0, param_ix = 0;
	for(IDL_tree cur = param_list;
		cur != NULL;
		cur = IDL_LIST(cur).next, param_ix++)
	{
		IDL_tree p = IDL_LIST(cur).data,
			type = get_type_spec(IDL_PARAM_DCL(p).param_type_spec),
			ident = IDL_PARAM_DCL(p).simple_declarator;
		enum IDL_param_attr attr = IDL_PARAM_DCL(p).attr;
		const char *name = IDL_IDENT(ident).str;
		int nargs = 0;
		bool accept = (is_outhalf && attr != IDL_PARAM_IN)
			|| (!is_outhalf && attr != IDL_PARAM_OUT);
		if(is_rigid_type(NULL, type)) {
			nargs = 1;
			if(accept) {
				/* this may include overlong items. */
				struct msg_param *u = new_untyped(name, type, p, param_ix);
				u->arg_ix = arg_pos;
				untyped = g_list_prepend(untyped, u);
				params = g_list_prepend(params, u);
			}
		} else if(is_bounded_seq(type)) {
			nargs = 2;
			if(accept) {
				IDL_tree subtype = get_type_spec(
					IDL_TYPE_SEQUENCE(type).simple_type_spec);
				assert(is_rigid_type(NULL, subtype));
				/* this, too, will include overlong items. */
				struct msg_param *s = new_inline_seq(name, type, subtype, p,
					param_ix);
				s->arg_ix = arg_pos;
				seq = g_list_prepend(seq, s);
				params = g_list_prepend(params, s);
			}
		} else {
			nargs = 1;
			if(accept) {
				/* everything else is passed as long items. */
				struct msg_param *l = new_long_param(name, type, p,
					param_ix);
				l->arg_ix = arg_pos;
				_long = g_list_prepend(_long, l);
				params = g_list_prepend(params, l);
			}
		}
		arg_pos += nargs;
	}
	untyped = g_list_reverse(untyped);
	seq = g_list_reverse(seq);
	_long = g_list_reverse(_long);
	params = g_list_reverse(params);

	/* assign untyped words. also figure out how many typed and untyped words
	 * we're going to use, before inline sequences are allocated.
	 */
	bool reg_in_use[64];
	reg_in_use[0] = true;		/* message tag, always present */
	for(int i=1; i<64; i++) reg_in_use[i] = false;
	int next_u = 1;
	if(has_sublabel) reg_in_use[next_u++] = true;
	if(return_type != NULL) {
		assert(is_value_type(return_type));
		int rt_words = size_in_words(return_type);
		assert(next_u + rt_words <= 63);
		for(int i=0; i<rt_words; i++) reg_in_use[next_u + i] = true;
		next_u += rt_words;
	}

	/* those with explicit MR(%d) specs, first. */
	GLIST_FOREACH(cur, untyped) {
		struct msg_param *u = cur->data;
		IDL_tree ident = IDL_PARAM_DCL(u->param_dcl).simple_declarator,
			type = u->X.untyped.type;
		unsigned long mr_n = 0;
		if(!get_ul_property(&mr_n, ident, "MR")) goto fail;
		u->X.untyped.reg_manual = mr_n > 0;
		if(mr_n == 0) continue;
		/* TODO: move these three conditions into verify.c */
		if(size_in_words(type) > 1) {
			fprintf(stderr, "%s: mr spec attribute not valid for <%s> (size %d words)\n",
				__func__, IDL_NODE_TYPE_NAME(type), size_in_words(type));
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
			fprintf(stderr, "%s: MR%lu already in use\n", __func__, mr_n);
			if(return_type != NULL || has_sublabel) {
				fprintf(stderr, "\t(this operation has");
				if(return_type != NULL) fprintf(stderr, " a return type");
				if(return_type != NULL && has_sublabel) {
					fprintf(stderr, " and");
				}
				if(has_sublabel) fprintf(stderr, " a sublabel");
				fprintf(stderr, ", occupying the first %d registers.)\n",
					(has_sublabel ? 1 : 0) + (return_type == NULL ? 0
						: size_in_words(return_type)));
			}
			goto fail;
		}
		reg_in_use[mr_n] = true;
		u->X.untyped.first_reg = mr_n;
		u->X.untyped.last_reg = mr_n;
	}
	/* then the non-compound types (since there is no alternative encoding for
	 * them)
	 */
	int num_compound = 0;
	GLIST_FOREACH(cur, untyped) {
		struct msg_param *u = cur->data;
		if(u->X.untyped.reg_manual) continue;
		IDL_tree type = u->X.untyped.type;
		if(!is_value_type(type)) {
			num_compound++;
			continue;
		}

		int size = size_in_words(type);
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
		u->X.untyped.first_reg = next_u;
		u->X.untyped.last_reg = next_u + size - 1;
		next_u += size;
		for(int i=u->X.untyped.first_reg; i<=u->X.untyped.last_reg; i++) {
			assert(!reg_in_use[i]);
			reg_in_use[i] = true;
		}
	}
	/* finally those parameters of compound type that we can squeeze in. this
	 * algorithm is simple for repeatability's sake; a solution to the knapsack
	 * problem would have to be proven optimal to fit as well. (v2 todo?)
	 */
	inf = g_new0(struct message_info, 1);		/* 0'd for g_free() safety */
	inf->params = params;
	int num_seq = g_list_length(seq), num_long = g_list_length(_long);

	GList *untyped_remove = NULL;
	GLIST_FOREACH(cur, untyped) {
		struct msg_param *u = cur->data;
		IDL_tree type = u->X.untyped.type;
		if(u->X.untyped.reg_manual || is_value_type(type)) continue;
		assert(is_rigid_type(NULL, type));
		num_compound--;
		assert(num_compound >= 0);
		/* accounts for typed items in the case that the following compound
		 * items and all inline sequences won't fit, and for long items.
		 */
		const int space = 64 - (num_compound + num_seq + num_long) * 2 - next_u,
			size = size_in_words(type);
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
			struct msg_param *l = new_long_param(u->name, type,
				u->param_dcl, u->param_ix);
			l->arg_ix = arg_pos;
			arg_pos += 2;
			_long = g_list_append(_long, l);
			g_free(u);
			untyped_remove = g_list_prepend(untyped_remove, cur);
		} else {
			/* place. */
			u->X.untyped.first_reg = start;
			u->X.untyped.last_reg = start + size - 1;
			for(int i=u->X.untyped.first_reg; i<=u->X.untyped.last_reg; i++) {
				assert(!reg_in_use[i]);
				reg_in_use[i] = true;
			}
		}
	}
	GLIST_FOREACH(c, untyped_remove) {
		untyped = g_list_delete_link(untyped, c->data);
	}
	if(untyped_remove != NULL) g_list_free(untyped_remove);

	inf->untyped = untyped;
	inf->tag_u = 0;
	for(int i = 1; i < 64; i++) {
		if(reg_in_use[i]) inf->tag_u = i;
	}
	GLIST_FOREACH(cur, inf->untyped) {
		struct msg_param *p = cur->data;
		printf("untyped `%s': p %d, arg %d, regs [%d, %d]\n",
			p->name, p->param_ix, p->arg_ix,
			p->X.untyped.first_reg, p->X.untyped.last_reg);
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
#endif
	inf->seq = seq;
	inf->_long = _long;

	IDL_tree op = NULL;
	if(return_type != NULL) {
		op = IDL_get_parent_node(return_type, IDLN_OP_DCL, NULL);
		assert(op != NULL);
	}
	inf->ret_type = is_outhalf ? return_type : NULL;
	inf->ret_by_ref = return_type != NULL
		&& (!is_value_type(return_type)
			|| (op != NULL && find_neg_exn(op) != NULL
				&& !is_real_nre_return_type(return_type)));

	return inf;

fail:
	free_message_info(inf);
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
	GLIST_FOREACH(cur, req->untyped) {
		struct msg_param *u = cur->data;
		if(u->X.untyped.reg_manual
			&& (u->X.untyped.first_reg == 0 || u->X.untyped.last_reg == 0))
		{
			g_set_error(error_p, 0, 0,
				"sublabel_bump: register 0 assigned manually");
			return false;
		}
	}

	GLIST_FOREACH(cur, req->untyped) {
		struct msg_param *u = cur->data;
		/* (untyped_words is a MsgWord offset, therefore +1.) */
		assert(u->X.untyped.first_reg < req->tag_u + 1);
		assert(u->X.untyped.last_reg < req->tag_u + 1);
		u->X.untyped.first_reg++;
		u->X.untyped.last_reg++;
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

	/* build the request. */
	inf->request = build_message(return_type, param_list,
		op_has_sublabel(method), false);
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
		inf->replies[0] = build_message(return_type, param_list,
			false, true);
		inf->replies[0]->node = method;
		inf->replies[0]->label = 0;
		inf->replies[0]->tagmask = NO_TAGMASK;
		inf->replies[0]->sublabel = NO_SUBLABEL;
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
	GLIST_FOREACH(cur, method_info_list) {
		struct method_info *inf = cur->data;
		struct message_info *req = inf->request;

		int i = 0;
		GLIST_FOREACH(cur_l, req->_long) {
			struct msg_param *p = cur_l->data;
			assert(IDL_PARAM_DCL(p->param_dcl).attr != IDL_PARAM_OUT);
			IDL_tree type = p->X._long.type;
			int length = max_size(type);
			bool stringlike = IDL_NODE_TYPE(type) == IDLN_TYPE_STRING
				|| IDL_NODE_TYPE(type) == IDLN_TYPE_WIDE_STRING;
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
			i++;
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
