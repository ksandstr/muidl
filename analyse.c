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
static int array_size_in_words(IDL_tree type_array, IDL_tree dcl);
static int struct_size_in_words(IDL_tree type_struct);


/* returns true when a value was assigned, false when not, and exits with a
 * complaint when the value is malformed.
 */
static bool get_ul_property(
	unsigned long *value_p,
	IDL_tree ident,
	const char *name)
{
	const char *p = IDL_tree_property_get(ident, name);
	if(p == NULL) return false;
	char *endptr = NULL;
	unsigned long ret = strtoul(p, &endptr, 0);
	if(ret == 0 && endptr == p) {
		fprintf(stderr, "error: malformed %s property on `%s': `%s'\n",
			name, IDL_IDENT_REPO_ID(ident), p);
		exit(EXIT_FAILURE);
	}

	*value_p = ret;
	return true;
}


static bool op_has_sublabel(IDL_tree prop_node)
{
	IDL_tree iface = IDL_get_parent_node(prop_node, IDLN_INTERFACE, NULL);
	/* exceptions, among other things, can appear outside interfaces.
	 * fortunately sublabels don't apply to them.
	 */
	if(iface == NULL) return false;
	unsigned long ifacelabel = 0;
	return get_ul_property(&ifacelabel, IDL_INTERFACE(iface).ident,
		"IfaceLabel");
}


static bool get_msg_label(struct message_info *inf, IDL_tree prop_node)
{
	unsigned long tagmask = NO_TAGMASK;
	get_ul_property(&tagmask, prop_node, "TagMask");
	inf->tagmask = tagmask;

	unsigned long labelval = 0;
	get_ul_property(&labelval, prop_node, "Label");
	inf->label = labelval;

	IDL_tree iface = IDL_get_parent_node(prop_node, IDLN_INTERFACE, NULL);
	/* exceptions, among other things, can appear outside interfaces.
	 * fortunately sublabels don't apply to them.
	 */
	if(iface != NULL) {
		IDL_tree ifprop = IDL_INTERFACE(iface).ident;
		unsigned long ifacelabel = 0;
		if(get_ul_property(&ifacelabel, ifprop, "IfaceLabel")) {
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
			g_assert_not_reached();

		default:
			assert(is_rigid_type(type));
			g_assert_not_reached();
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


int size_in_words(IDL_tree type)
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


int max_size(IDL_tree type)
{
	if(is_value_type(type)) return (size_in_bits(type) + 7) / 8;
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
	u->type = type;
	u->name = name;
	u->param_dcl = param;
	u->param_ix = param_ix;
	u->arg_ix = -1;
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
	s->type = type;
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


static struct msg_param *new_typed(
	enum msg_param_kind kind,
	const char *name,
	IDL_tree type,
	IDL_tree p,
	int param_ix)
{
	assert(kind == P_STRING || kind == P_MAPPED);
	struct msg_param *t = g_new0(struct msg_param, 1);
	t->kind = kind;
	t->type = type;
	t->name = name;
	t->param_dcl = p;
	t->param_ix = param_ix;
	t->arg_ix = -1;
	return t;
}


/* FIXME: move to types.c */
static bool is_bounded_seq(IDL_tree type)
{
	return IDL_NODE_TYPE(type) == IDLN_TYPE_SEQUENCE
		&& IDL_TYPE_SEQUENCE(type).positive_int_const != NULL
		&& IDL_INTEGER(IDL_TYPE_SEQUENCE(type).positive_int_const).value > 0;
}


/* FIXME: move to types.c, or a props.c */
#define has_map_property(ident) (IDL_tree_property_get((ident), "map") != NULL)


/* turn a parameter list into lists of untyped, inline-sequence and long types.
 * the first have fixed register assignments, inline sequences have register
 * ranges and long types appear in transmission order.
 */
static struct message_info *build_message(
	IDL_ns ns,
	IDL_tree op_dcl,
	IDL_tree return_type,		/* separate because not IDL_PARAM_DCL */
	bool has_sublabel,
	bool is_outhalf)
{
	struct message_info *inf = NULL;
	/* of <struct msg_param *>, freed by "params" */
	GList *params = NULL, *untyped = NULL, *seq = NULL, *string = NULL,
		*mapped = NULL;

	IDL_tree param_list = IDL_OP_DCL(op_dcl).parameter_dcls;

	/* classify parameters and construct msg_param structures for them. also
	 * compute how much space typed parameters' encodings take up in the worst
	 * case, so that inline sequences can be assigned or rejected accordingly.
	 */
	int arg_pos = 0, param_ix = 0, typed_use = 0;
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
		const bool accept = (is_outhalf && attr != IDL_PARAM_IN)
				|| (!is_outhalf && attr != IDL_PARAM_OUT),
			is_mapped = IS_MAPPING_TYPE(type) || (IS_MAPGRANT_TYPE(type)
					&& has_map_property(ident));
		if(is_rigid_type(type) && (!IS_MAPGRANT_TYPE(type)
				|| !has_map_property(ident)))
		{
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
				assert(is_rigid_type(subtype));
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
				/* everything else is passed as typed items. */
				struct msg_param *l = new_typed(is_mapped ? P_MAPPED : P_STRING,
					name, type, p, param_ix);
				l->arg_ix = arg_pos;
				if(is_mapped) mapped = g_list_prepend(mapped, l);
				else string = g_list_prepend(string, l);
				params = g_list_prepend(params, l);
				if(IS_MAPGRANT_TYPE(type) && has_map_property(ident)) {
					assert(is_mapped);
					typed_use += 2;		/* a single map/grant item */
				} else if(IS_MAPPING_TYPE(type)) {
					assert(is_mapped);
					typed_use += 46;	/* up to 23 map/grant items */
				} else {
					assert(!is_mapped);
					typed_use += 2;		/* a simple string item */
				}
			}
		}
		arg_pos += nargs;
	}
	untyped = g_list_reverse(untyped);
	seq = g_list_reverse(seq);
	mapped = g_list_reverse(mapped);
	string = g_list_reverse(string);
	params = g_list_reverse(params);

	/* assign untyped words. also figure out how many typed and untyped words
	 * we're going to use, before inline sequences are allocated.
	 */
	bool reg_in_use[64];
	reg_in_use[0] = true;		/* message tag, always present */
	for(int i=1; i<64; i++) reg_in_use[i] = false;
	int next_u = 1;
	if(has_sublabel) reg_in_use[next_u++] = true;
	if(return_type != NULL && is_outhalf) {
		assert(is_rigid_type(return_type));
		int rt_words = size_in_words(return_type);
		assert(next_u + rt_words <= 63);
		for(int i=0; i<rt_words; i++) reg_in_use[next_u + i] = true;
		next_u += rt_words;
	}

	/* those with explicit MR(%d) specs, first. */
	GLIST_FOREACH(cur, untyped) {
		struct msg_param *u = cur->data;
		IDL_tree ident = IDL_PARAM_DCL(u->param_dcl).simple_declarator;
		unsigned long mr_n = 0;
		u->X.untyped.reg_manual = get_ul_property(&mr_n, ident, "MR");
		if(mr_n == 0) continue;
		/* TODO: move these three conditions into verify.c */
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
	inf->ctx_index = -1;
	inf->params = params;

	GList *untyped_remove = NULL;
	GLIST_FOREACH(cur, untyped) {
		struct msg_param *u = cur->data;
		if(u->X.untyped.reg_manual || is_value_type(u->type)) continue;
		assert(is_rigid_type(u->type));
		num_compound--;
		assert(num_compound >= 0);
		const int space = 63 - (next_u - 1) - typed_use,
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
			struct msg_param *l = new_typed(P_STRING, u->name, u->type,
				u->param_dcl, u->param_ix);
			l->arg_ix = arg_pos;
			arg_pos += 2;
			string = g_list_append(string, l);	/* already reversed. */
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
	inf->seq = seq;
	inf->mapped = mapped;
	inf->string = string;

	inf->tag_u = 0;
	for(int i = 1; i < 64; i++) {
		if(reg_in_use[i]) inf->tag_u = i;
	}

	inf->ret_type = is_outhalf ? return_type : NULL;
	inf->ret_by_ref = return_type != NULL
		&& (!is_value_type(return_type)
			|| (find_exn(op_dcl, &is_negs_exn) != NULL
				&& !is_real_nre_return_type(return_type)));

	return inf;

fail:
	free_message_info(inf);
	return NULL;
}


static const char *get_dcl_name(IDL_tree dcl)
{
	switch(IDL_NODE_TYPE(dcl)) {
		case IDLN_IDENT: return IDL_IDENT(dcl).str;
		case IDLN_TYPE_ARRAY: return IDL_IDENT(IDL_TYPE_ARRAY(dcl).ident).str;
		default: NOTDEFINED(dcl);
	}
}


static void add_rigid_dcl(
	struct message_info *msg,
	IDL_tree mtype,
	IDL_tree dcl,
	int param_ix,
	int arg_ix)
{
	int count = -1, words = -1;
	bool is_array = false;
	if(IDL_NODE_TYPE(dcl) == IDLN_IDENT) {
		count = 1;
		words = size_in_words(mtype);
	} else if(IDL_NODE_TYPE(dcl) == IDLN_TYPE_ARRAY) {
		count = 1;
		is_array = true;
		IDL_LIST_FOREACH(size_cur, IDL_TYPE_ARRAY(dcl).size_list) {
			count *= IDL_INTEGER(IDL_LIST(size_cur).data).value;
		}
		words = size_in_words(dcl);
	} else {
		g_assert_not_reached();
	}

	struct msg_param *u = new_untyped(get_dcl_name(dcl),
		is_array ? dcl : mtype, NULL, param_ix);
	u->X.untyped.first_reg = msg->tag_u;
	u->X.untyped.last_reg = msg->tag_u + words - 1;
	u->arg_ix = arg_ix;
	msg->tag_u += words;
	msg->untyped = g_list_prepend(msg->untyped, u);
}


static void add_str_dcl(
	struct message_info *msg,
	IDL_tree mtype,
	IDL_tree dcl,
	int param_ix,
	int *arg_ix_p)
{
	struct msg_param *lp = new_typed(P_STRING, get_dcl_name(dcl), mtype,
		NULL, param_ix);
	lp->arg_ix = *arg_ix_p;
	if(IDL_NODE_TYPE(mtype) == IDLN_TYPE_SEQUENCE) (*arg_ix_p) += 2;
	else (*arg_ix_p)++;
	msg->string = g_list_prepend(msg->string, lp);
}


static void add_seq_dcl(
	struct message_info *msg,
	IDL_tree mtype,
	IDL_tree dcl,
	int param_ix,
	int *arg_ix_p)
{
	/* TODO: construct inline sequences from eligible types... eventually...
	 * (also these would appear in a mixed order with the untyped parameters,
	 * hardly ideal for positioning. there'd have to be an another pass for
	 * that in build_exception_message().)
	 */
	add_str_dcl(msg, mtype, dcl, param_ix, arg_ix_p);
}


struct message_info *build_exception_message(IDL_tree exn)
{
	struct message_info *msg = g_new(struct message_info, 1);
	msg->label = 2;
	msg->sublabel = exn_hash(exn);
	msg->tagmask = NO_TAGMASK;
	msg->ctx_index = -1;

	msg->node = exn;
	msg->ret_type = NULL;
	msg->ret_by_ref = false;

	msg->params = NULL;
	msg->untyped = NULL;
	msg->seq = NULL;
	msg->mapped = NULL;
	msg->string = NULL;

	/* construct parameters for exception members. arg_ix is the index of the
	 * member's first argument in the raiser function's prototype; arg_ix + 1
	 * is the index of the corresponding member in the exception struct.
	 */
	int param_ix = 0, arg_ix = 0;
	msg->tag_u = 2;
	IDL_LIST_FOREACH(m_cur, IDL_EXCEPT_DCL(exn).members) {
		IDL_tree member = IDL_LIST(m_cur).data,
			mtype = get_type_spec(IDL_MEMBER(member).type_spec);
		IDL_LIST_FOREACH(d_cur, IDL_MEMBER(member).dcls) {
			IDL_tree dcl = IDL_LIST(d_cur).data;
			if(is_rigid_type(mtype)) {
				add_rigid_dcl(msg, mtype, dcl, param_ix++, arg_ix++);
			} else if(IDL_NODE_TYPE(mtype) == IDLN_TYPE_SEQUENCE) {
				add_seq_dcl(msg, mtype, dcl, param_ix++, &arg_ix);
			} else {
				add_str_dcl(msg, mtype, dcl, param_ix++, &arg_ix);
			}
		}
	}

	/* this leaves msg->params NULL, as exceptions don't really have
	 * parameters as such.
	 */
	msg->untyped = g_list_reverse(msg->untyped);
	msg->seq = g_list_reverse(msg->seq);
	msg->string = g_list_reverse(msg->string);
	assert(msg->mapped == NULL);
	msg->tag_u--;

	return msg;
}


/* TODO: get the number and max dimensions of the string buffers we'll
 * send/receive once long types are implemented.
 *
 * this function keeps label == 0 || (tagmask set && sublabel == 0) as
 * indication that the label has not been set.
 */
struct method_info *analyse_op_dcl(IDL_ns ns, IDL_tree method)
{
	int num_replies;
	if(IDL_OP_DCL(method).f_oneway) num_replies = 0;
	else num_replies = 1 + IDL_list_length(IDL_OP_DCL(method).raises_expr);

	IDL_tree return_type = get_type_spec(IDL_OP_DCL(method).op_type_spec);

	struct method_info *inf = g_malloc0(sizeof(struct method_info)
		+ sizeof(struct message_info *) * num_replies);
	inf->oneway = IDL_OP_DCL(method).f_oneway ? true : false;
	inf->vtab_offset = -1;
	inf->node = method;
	inf->name = IDL_IDENT(IDL_OP_DCL(method).ident).str;
	inf->num_reply_msgs = num_replies;

	/* build the request. */
	inf->request = build_message(ns, method, return_type,
		op_has_sublabel(method), false);
	if(inf->request == NULL) goto fail;
	inf->request->node = method;
	if(!get_msg_label(inf->request, IDL_OP_DCL(method).ident)) {
		IDL_tree iface = IDL_get_parent_node(method, IDLN_INTERFACE, NULL);
		fprintf(stderr, "error: invalid label specification for `%s' in `%s'\n",
			METHOD_NAME(method), IDL_IDENT_REPO_ID(IDL_INTERFACE(iface).ident));
		goto fail;
	}

	if(num_replies > 0) {
		/* the usual reply: return type and out, inout out-halves */
		inf->replies[0] = build_message(ns, method, return_type,
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
			IDL_tree ex_ident = IDL_LIST(c).data,
				exn = IDL_get_parent_node(ex_ident, IDLN_EXCEPT_DCL, NULL);
			inf->replies[expos++] = build_exception_message(exn);
		}
		assert(expos == num_replies);

		/* fill in their index in the interface context union. */
		IDL_tree iface = IDL_get_parent_node(method, IDLN_INTERFACE, NULL);
		assert(iface != NULL);
		GList *order_exns = iface_exns_sorted(ns, iface);
		int ctx_pos = 0;
		GLIST_FOREACH(e_cur, order_exns) {
			ctx_pos++;		/* numbered from 1 on */
			struct message_info *e = NULL;
			for(int i=1; i<expos; i++) {
				if(inf->replies[i]->node == e_cur->data) {
					e = inf->replies[i];
					break;
				}
			}
			if(e != NULL) {
				/* exception appears in this method. */
				e->ctx_index = ctx_pos;
			}
		}
		g_list_free(order_exns);

#ifndef NDEBUG
		for(int i=1; i<expos; i++) {
			assert(inf->replies[i]->ctx_index > 0);
		}
#endif
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

	/* oneway restrictions.
	 *
	 * TODO: move these into verify.c!
	 * TODO: and the chunk below this one!
	 */
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


static struct method_info *find_method_by_label(GList *list, uint32_t label)
{
	GLIST_FOREACH(c, list) {
		struct method_info *inf = c->data;
		if(inf->request->label == label) return inf;
	}
	return NULL;
}


static struct method_info *find_method_by_sublabel(GList *list, uint32_t label)
{
	GLIST_FOREACH(c, list) {
		struct method_info *inf = c->data;
		if(inf->request->sublabel == label) return inf;
	}
	return NULL;
}


static void assign_method_labels(
	IDL_tree iface,
	GList *methods,
	GList *tagmask_methods)
{
	IDL_tree iface_ident = IDL_INTERFACE(iface).ident;

	/* assign labels to those methods that haven't got any. */
	unsigned long ifacelabel = 0;
	get_ul_property(&ifacelabel, iface_ident, "IfaceLabel");
	/* skips reply labels of success (0), MSG_ERROR (1) and complex exceptions
	 * (2), when not given an interface label.
	 */
	unsigned long min_label = ifacelabel == 0 ? 3 : 0;
	if(get_ul_property(&min_label, iface_ident, "FirstLabel")) min_label--;

	/* FIXME: this method of label assignment will cause wailing & gnashing of
	 * teeth when someone provokes a label clash.
	 */
	uint32_t max_label = min_label;
	GLIST_FOREACH(m_cur, methods) {
		struct method_info *inf = m_cur->data;
		struct message_info *req = inf->request;

		if(ifacelabel == 0 && req->label == 0) {
			assert(req->tagmask == NO_TAGMASK);
			int label = max_label + 1;
			/* FIXME: set limit to where L4.X2 reserved labels begin */
			while(find_method_by_label(methods, label) != NULL
				&& label < 0x10000)
			{
				label++;
			}
			if(label >= 0x10000) goto fail;
			req->label = label;
			max_label = label;
		} else if(ifacelabel != 0 && req->sublabel == 0) {
			assert(req->tagmask == NO_TAGMASK);
			int sublabel = max_label + 1;
			/* the limit is arbitrary (but reasonable). */
			while(find_method_by_sublabel(tagmask_methods, sublabel) != NULL
				&& sublabel < 0x10000)
			{
				sublabel++;
			}
			if(sublabel >= 0x10000) goto fail;
			req->sublabel = sublabel;
			max_label = sublabel;
		}

		if(ifacelabel != 0) max_label = MAX(max_label, req->sublabel);
		else max_label = MAX(max_label, req->label);
	}

#ifndef NDEBUG
	GHashTable *lab_hash = g_hash_table_new(&g_direct_hash, &g_direct_equal);
	GLIST_FOREACH(mcur, methods) {
		struct method_info *inf = mcur->data;
		struct message_info *req = inf->request;

		int label = ifacelabel > 0 ? req->sublabel : req->label;
		if(g_hash_table_lookup(lab_hash, GINT_TO_POINTER(label)) != NULL) {
			fprintf(stderr, "error: duplicate op labels in `%s'\n",
				IDL_IDENT_REPO_ID(iface_ident));
			exit(EXIT_FAILURE);
		} else {
			g_hash_table_insert(lab_hash, GINT_TO_POINTER(label), inf);
		}
	}
	g_hash_table_destroy(lab_hash);
#endif

	return;

fail:
	fprintf(stderr, "error: can't assign labels to `%s'\n",
		IDL_IDENT_REPO_ID(iface_ident));
	exit(EXIT_FAILURE);
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


/* collect all operations of the interface given (including those in
 * subclasses, as per all_methods_of_iface()), separate tag-masked operations
 * into a given list, sort the rest by ascending label and return. result is a
 * GList of <struct method_info *> with vtab_offset filled in for each.
 */
static GList *analyse_methods_of_iface(
	IDL_ns ns,
	GList **tagmask_list_p,
	IDL_tree iface)
{
	GList *methods = all_methods_of_iface(ns, iface);
	int vtab_pos = 0;
	GLIST_FOREACH(cur, methods) {
		IDL_tree method = cur->data;
		struct method_info *inf = analyse_op_dcl(ns, method);
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

	assign_method_labels(iface, methods, *tagmask_list_p);

	return g_list_sort(methods, &method_info_by_label_cmp);
}


/* add a new string item for a parameter, or fold it into an existing one. */
static void add_stritem(GArray *result, int i, struct msg_param *p)
{
	int length = max_size(p->type);
	bool stringlike = IDL_NODE_TYPE(p->type) == IDLN_TYPE_STRING
		|| IDL_NODE_TYPE(p->type) == IDLN_TYPE_WIDE_STRING;
	struct stritem_info *si;
	assert(i <= result->len + 1);
	if(result->len <= i) {
		struct stritem_info tmp = {
			.length = 0,
			.stringlike = false,
			.reply_pos = 0,		/* default */
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


/* STRINGS */
struct stritem_info *stub_stritems(const struct method_info *inf)
{
	if(IDL_OP_DCL(inf->node).f_oneway) return NULL;

	GArray *result = g_array_new(FALSE, FALSE, sizeof(struct stritem_info));

	/* since exceptions are encoded as replies, this works for them too.
	 * pretty nice.
	 */
	for(int i=0; i < inf->num_reply_msgs; i++) {
		int str_ix = 0;
		/* this loop is not about mappings. it is about STRINGS. */
		GLIST_FOREACH(long_cur, inf->replies[i]->string) {
			struct msg_param *lp = long_cur->data;
			assert(lp->param_dcl == NULL
				|| IDL_PARAM_DCL(lp->param_dcl).attr != IDL_PARAM_IN);
			add_stritem(result, str_ix++, lp);	/* STRINGS */
		}
	}

	/* terminate and return. */
	struct stritem_info tmp = { .length = -1 };
	g_array_append_val(result, tmp);
	/* STRINGS */
	return (void *)g_array_free(result, result->len == 1);
}


struct stritem_info *dispatcher_stritems(GList *method_info_list)
{
	GArray *result = g_array_new(FALSE, FALSE, sizeof(struct stritem_info));
	GLIST_FOREACH(cur, method_info_list) {
		struct method_info *inf = cur->data;
		struct message_info *req = inf->request;

		int i = 0;
		GLIST_FOREACH(cur_l, req->string) {
			struct msg_param *p = cur_l->data;
			assert(IDL_PARAM_DCL(p->param_dcl).attr != IDL_PARAM_OUT);
			add_stritem(result, i++, p);
		}
	}

	/* terminate and return. */
	struct stritem_info tmp = { .length = -1 };
	g_array_append_val(result, tmp);
	return (void *)g_array_free(result, result->len == 1);
}


struct iface_info *analyse_interface(IDL_ns ns, IDL_tree iface)
{
	struct iface_info *inf = g_new(struct iface_info, 1);
	inf->node = iface;
	inf->name = IDL_IDENT(IDL_INTERFACE(iface).ident).str;
	inf->tagmask_ops = NULL;
	inf->ops = analyse_methods_of_iface(ns, &inf->tagmask_ops, iface);

	inf->has_replies = false;
	GLIST_FOREACH(cur, inf->ops) {
		const struct method_info *op = cur->data;
		if(op->num_reply_msgs > 0) {
			inf->has_replies = true;
			break;
		}
	}

	return inf;
}
