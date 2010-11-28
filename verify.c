/*
 * verify.c -- verification of IDL tree inputs
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
#include <ctype.h>
#include <libIDL/IDL.h>

#include "muidl.h"


struct ver_ctx
{
	IDL_ns ns;
	IDL_tree tree;
	bool failed;
	const char *clause_text;
};


static void fail(struct ver_ctx *v, const char *fmt, ...)
	__attribute__((format(printf, 2, 3)));


static void failv(struct ver_ctx *v, const char *fmt, va_list al)
{
	char *tmp = g_strdup_vprintf(fmt, al);
	size_t len = strlen(tmp);
	while(isblank(tmp[len - 1])) {
		tmp[len - 1] = '\0';
		len--;
	}
	assert(strlen(tmp) == len);
	if(v->clause_text != NULL) {
		fprintf(stderr, "verify: in clause `%s':\n", v->clause_text);
	}
	if(len > 0) {
		fprintf(stderr, "verify: %s%s\n",
			v->clause_text != NULL ? "  " : "", tmp);
	}
	if(v->clause_text == NULL && len == 0) {
		fprintf(stderr, "verify: failed\n");
	}
	v->failed = true;
	g_free(tmp);
}


static void fail(struct ver_ctx *v, const char *fmt, ...)
{
	va_list al;
	va_start(al, fmt);
	failv(v, fmt, al);
	va_end(al);
}


static gboolean no_reserved_idents(IDL_tree_func_data *tf, gpointer udptr)
{
	struct ver_ctx *v = udptr;

	/* a huge green fierce snake bars your way! */
	switch(IDL_NODE_TYPE(tf->tree)) {

		case IDLN_IDENT: break;

		default: return TRUE;	/* leculsion prease. */
	}

	if(is_reserved_word(IDL_IDENT(tf->tree).str)) {
		IDL_tree up = IDL_NODE_UP(tf->tree);
		switch(IDL_NODE_TYPE(up)) {
			case IDLN_TYPE_STRUCT:
			case IDLN_TYPE_UNION:
			case IDLN_TYPE_ENUM:
			case IDLN_MODULE:
			case IDLN_INTERFACE:
				/* reserved words in these structures' names are handled and
				 * prefixed by long_name().
				 */
				break;

			default:
				fail(v, "`%s' (repoid `%s') is a reserved word in C",
					IDL_IDENT(tf->tree).str, IDL_IDENT_REPO_ID(tf->tree));
		}
	}

	/* you may pass. */
	return FALSE;
}


static gboolean supported_types_only(IDL_tree_func_data *tf, gpointer udptr)
{
	struct ver_ctx *v = udptr;

	IDL_tree node = tf->tree;
	while(node != NULL && IDL_NODE_TYPE(node) == IDLN_IDENT) {
		node = IDL_get_parent_node(node, IDLN_ANY, NULL);
	}

	bool in_const = IDL_get_parent_node(node, IDLN_CONST_DCL, NULL) != NULL;
	switch(IDL_NODE_TYPE(node)) {
		case IDLN_INTERFACE: {
			/* what do badgers eat?
			 * where cannot interface types appear?
			 */
			static const IDL_tree_type nogo[] = {
				IDLN_PARAM_DCL,		/* not in parameters. */
				IDLN_TYPE_STRUCT,	/* and not in structs. */
				IDLN_OP_DCL,		/* not in return types... */
				IDLN_TYPE_SEQUENCE,	/* or sequences */
				IDLN_TYPE_ARRAY,	/* let alone arrays */
				IDLN_TYPE_DCL,		/* or even typedefs. */
				IDLN_TYPE_UNION,	/* in unions? ha ha. */
				/* so sad. */
			};
			if(tf->up == NULL) break;
			for(int i=0; i<G_N_ELEMENTS(nogo); i++) {
				IDL_tree parent = IDL_get_parent_node(tf->up->tree, nogo[i], NULL);
				if(parent != NULL) {
					fail(v, "interface types are not supported inside <%s>",
						IDL_NODE_TYPE_NAME(parent));
					return FALSE;
				}
			}
			break;
		}

		case IDLN_TYPE_OBJECT:
		case IDLN_TYPE_ANY:
		case IDLN_TYPE_TYPECODE:
			fail(v, "type <%s> not supported", IDL_NODE_TYPE_NAME(node));
			return FALSE;

		case IDLN_TYPE_SEQUENCE:
			if(IDL_TYPE_SEQUENCE(node).positive_int_const == NULL
				&& !in_const)
			{
				fail(v, "sequences must be bounded");
			}
			break;

		case IDLN_TYPE_STRING:
			if(IDL_TYPE_STRING(node).positive_int_const == NULL
				&& !in_const)
			{
				fail(v, "string types must be bounded");
			}
			return FALSE;	/* no point */

		case IDLN_TYPE_WIDE_STRING:
			if(IDL_TYPE_WIDE_STRING(node).positive_int_const == NULL
				&& !in_const)
			{
				fail(v, "wide strings must be bounded");
			}
			return FALSE;

		case IDLN_TYPE_ARRAY: {
			if(IDL_list_length(IDL_TYPE_ARRAY(node).size_list) > 1) {
				fail(v, "arrays must have a single dimension");
			}

			IDL_tree atype = get_array_type(node);
			if(!is_rigid_type(atype)) {
				fail(v, "arrays must be of rigid type (<%s> not allowed)",
					IDL_NODE_TYPE_NAME(atype));
			}

			break;
		}

		/* unions must be rigid, and therefore can't contain sequences or
		 * strings.
		 */
		case IDLN_TYPE_UNION: {
			const char *uname = IDL_IDENT(IDL_TYPE_UNION(node).ident).str;
			IDL_tree swtype = get_type_spec(
				IDL_TYPE_UNION(node).switch_type_spec);
			if(!is_value_type(swtype)) {
				fail(v, "union %s: discriminator type can't be <%s>", uname,
					IDL_NODE_TYPE_NAME(swtype));
			}
			IDL_LIST_FOREACH(cur, IDL_TYPE_UNION(node).switch_body) {
				IDL_tree body = IDL_LIST(cur).data,
					elem = IDL_CASE_STMT(body).element_spec,
					elemtype = get_type_spec(IDL_MEMBER(elem).type_spec);
				if(!is_rigid_type(elemtype)) {
					fail(v, "union %s: field types must be rigid (not <%s>)",
						uname, IDL_NODE_TYPE_NAME(elemtype));
				}
			}
			break;
		}

		/* structs may only contain rigid types. */
		case IDLN_TYPE_STRUCT: {
			const char *sname = IDL_IDENT(IDL_TYPE_STRUCT(node).ident).str;
			struct member_item *members = expand_member_list(
				IDL_TYPE_STRUCT(node).member_list);
			for(int i=0; members[i].type != NULL; i++) {
				if(!is_rigid_type(members[i].type)) {
					fail(v, "struct %s: field `%s' must be rigid (not <%s>)",
						sname, members[i].name,
						IDL_NODE_TYPE_NAME(members[i].type));
				}
			}
			g_free(members);
			break;
		}

		case IDLN_NATIVE: {
			bool ok = IS_MAPGRANT_TYPE(node)
				|| IS_FPAGE_TYPE(node) || IS_WORD_TYPE(node)
				|| IS_TIME_TYPE(node) || IS_MAPPING_TYPE(node);
			if(!ok) {
				fail(v, "unknown native type `%s'", NATIVE_NAME(node));
			}
			break;
		}

		default:
			break;
	}

	/* everything else, and sequence & array component types */
	return TRUE;
}


static gboolean no_recv_timeout_in_oneway(
	IDL_tree_func_data *tf,
	gpointer udptr)
{
	struct ver_ctx *v = udptr;

	switch(IDL_NODE_TYPE(tf->tree)) {
		case IDLN_MODULE:
		case IDLN_INTERFACE:
		case IDLN_LIST:
			return TRUE;

		case IDLN_OP_DCL:
			if(IDL_OP_DCL(tf->tree).f_oneway
				&& (op_timeout_kind(tf->tree) & TIMEOUT_RECV))
			{
				fail(v, "`%s': an oneway operation can't have a receive timeout",
					IDL_IDENT(IDL_OP_DCL(tf->tree).ident).repo_id);
			}
			return FALSE;

		default:
			return FALSE;
	}
}


static gboolean one_mapping_per_direction(
	IDL_tree_func_data *tf,
	gpointer udptr)
{
	struct ver_ctx *v = udptr;

	switch(IDL_NODE_TYPE(tf->tree)) {
		case IDLN_MODULE:
		case IDLN_INTERFACE:
		case IDLN_LIST:
			return TRUE;

		case IDLN_OP_DCL: {
			/* TODO: get a module::interface::opname kind of name, here */
			const char *name = METHOD_NAME(tf->tree);
			bool have_in = false, have_out = false;
			IDL_LIST_FOREACH(cur, IDL_OP_DCL(tf->tree).parameter_dcls) {
				IDL_tree param = IDL_LIST(cur).data,
					ptype = get_type_spec(IDL_PARAM_DCL(param).param_type_spec);
				if(!IS_MAPPING_TYPE(ptype)) continue;
				enum IDL_param_attr attr = IDL_PARAM_DCL(param).attr;
				if(attr == IDL_PARAM_IN || attr == IDL_PARAM_INOUT) {
					if(have_in) {
						fail(v, "`%s' already has an in-mapping", name);
					}
					have_in = true;
				}
				if(attr == IDL_PARAM_OUT || attr == IDL_PARAM_INOUT) {
					if(have_out) {
						fail(v, "`%s' already has an out-mapping", name);
					}
					have_out = true;
				}
			}
			return FALSE;
		}

		default:
			return FALSE;
	}
}


static gboolean explicit_mr_param_size(IDL_tree_func_data *tf, gpointer udptr)
{
	struct ver_ctx *v = udptr;
	switch(IDL_NODE_TYPE(tf->tree)) {
		case IDLN_MODULE:
		case IDLN_INTERFACE:
		case IDLN_LIST:
			return TRUE;

		default:
			return FALSE;

		case IDLN_OP_DCL:
			break;
	}

	IDL_LIST_FOREACH(cur, IDL_OP_DCL(tf->tree).parameter_dcls) {
		IDL_tree pdcl = IDL_LIST(cur).data,
			ident = IDL_PARAM_DCL(pdcl).simple_declarator;
		if(IDL_tree_property_get(ident, "MR") == NULL) continue;

		IDL_tree ptyp = get_type_spec(IDL_PARAM_DCL(pdcl).param_type_spec);
		int sz = size_in_words(ptyp);
		if(sz > 1) {
			fail(v, "parameter `%s' has type <%s>, encoded as %d words",
				IDL_IDENT(ident).str, IDL_NODE_TYPE_NAME(ptyp), sz);
			break;
		}
	}

	return FALSE;
}


static gboolean explicit_mr_no_overlap(IDL_tree_func_data *tf, gpointer udptr)
{
	struct ver_ctx *v = udptr;
	switch(IDL_NODE_TYPE(tf->tree)) {
		case IDLN_MODULE:
		case IDLN_INTERFACE:
		case IDLN_LIST:
			return TRUE;

		default:
			return FALSE;

		case IDLN_OP_DCL:
			break;
	}

	bool reg_in_use[64],
		has_sub = op_has_sublabel(IDL_OP_DCL(tf->tree).ident);
	for(int i=0; i<64; i++) reg_in_use[i] = false;
	reg_in_use[0] = true;		/* tag */
	if(has_sub) reg_in_use[1] = true;	/* sublabel */

	IDL_LIST_FOREACH(cur, IDL_OP_DCL(tf->tree).parameter_dcls) {
		IDL_tree pdcl = IDL_LIST(cur).data,
			ident = IDL_PARAM_DCL(pdcl).simple_declarator;
		unsigned long mrval = 0;
		if(!get_ul_property(&mrval, ident, "MR")) continue;
		long mr = (long)mrval;
		if(mr < 0 || mr > 63) {
			fail(v, "explicit MR%ld spec is out of range", mr);
			break;
		}
		if(mr == 1 && has_sub) {
			fail(v, "can't specify MR1 in IfaceLabel interface");
			break;
		}
		if(mr == 0) {
			fail(v, "can't reassign MR0 (the message tag register)");
			break;
		}
		if(reg_in_use[mr]) {
			fail(v, "property specifies unavailable MR%lu for parameter `%s'",
				mr, IDL_IDENT(ident).str);
#if 0
/* spared from analyse.c . TODO: make this work in some useful way, i.e.
 * prettyprinting of tl;dr in verify fail messages
 */
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
#endif
			break;
		}
		reg_in_use[mr] = true;
	}

	return FALSE;
}


/* returns true when everything's OK */
bool verify_idl_input(IDL_ns ns, IDL_tree tree)
{
	static const struct {
		IDL_tree_func fn;
		const char *name;
	} clauses[] = {
		{ &no_reserved_idents,
		  "no identifiers which are C reserved words in lowercase" },
		{ &supported_types_only,
		  "only supported forms of IDL types (no references or unbounded types)" },
		{ &no_recv_timeout_in_oneway,
		  "oneway methods cannot have a StubRecvTimeout or StubTimeout attribute" },
		{ &one_mapping_per_direction,
		  "at most one mapping per message (op direction)" },

		/* explicit MR attribute specs */
		{ &explicit_mr_param_size,
		  "`MR(n)' attributes can only apply to word-size and smaller parameters" },
		{ &explicit_mr_no_overlap, NULL }
	};

	struct ver_ctx v = { .ns = ns, .tree = tree, .failed = false };
	for(int i=0; i < G_N_ELEMENTS(clauses); i++) {
		v.clause_text = clauses[i].name;
		IDL_tree_walk_in_order(tree, clauses[i].fn, &v);
		if(v.failed) return false;
	}

	/* succeed */
	return true;
}
