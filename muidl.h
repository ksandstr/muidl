/*
 * muidl.h -- extern definitions for µidl
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

#ifndef SEEN_MUIDL_H
#define SEEN_MUIDL_H

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdarg.h>
#include <assert.h>
#include <setjmp.h>
#include <libIDL/IDL.h>
#include <glib.h>


/* utilities for libIDL */
#define IFACE_REPO_ID(t) IDL_IDENT_REPO_ID(IDL_INTERFACE((t)).ident)
#define IFACE_NAME(t) (IDL_IDENT(IDL_INTERFACE((t)).ident).str)
#define METHOD_NAME(t) (IDL_IDENT(IDL_OP_DCL((t)).ident).str)
#define EXN_REPO_ID(t) (IDL_IDENT_REPO_ID(IDL_EXCEPT_DCL((t)).ident))
#define NATIVE_NAME(t) (IDL_IDENT(IDL_NATIVE((t)).ident).str)

#define NODETYPESTR(n) ({ \
		IDL_tree __n = (n); \
		__n == NULL ? "(nil)" : IDL_tree_type_names[IDL_NODE_TYPE(__n)]; \
	})

/* predicates for word, Fpage, MapGrantItem, unsigned short types */
#define IS_MAPGRANT_TYPE(t) (IDL_NODE_TYPE((t)) == IDLN_NATIVE \
	&& strcmp(NATIVE_NAME((t)), "l4_mapgrantitem_t") == 0)
#define IS_FPAGE_TYPE(t) (IDL_NODE_TYPE((t)) == IDLN_NATIVE \
	&& strcmp(NATIVE_NAME((t)), "l4_fpage_t") == 0)
#define IS_WORD_TYPE(t) (IDL_NODE_TYPE((t)) == IDLN_NATIVE \
	&& strcmp(NATIVE_NAME((t)), "l4_word_t") == 0)

#define IS_USHORT_TYPE(op_type) \
	(IDL_NODE_TYPE((op_type)) == IDLN_TYPE_INTEGER \
	&& !IDL_TYPE_INTEGER((op_type)).f_signed \
	&& IDL_TYPE_INTEGER((op_type)).f_type == IDL_INTEGER_TYPE_SHORT)


/* diagnostics, or something -- all NOTDEFINED() and NOTSUPPORTED() states
 * should be caught in analysis functions, however.
 */
#define NOTDEFINED(t) do { \
		fprintf(stderr, "%s: not defined for <%s>\n", __FUNCTION__, \
			NODETYPESTR((t))); \
		assert(0); \
		abort(); \
	} while(0)

#define NOTSUPPORTED(what) do { \
		fprintf(stderr, "%s: µidl does not support %s\n", \
			__FUNCTION__, (what)); \
		exit(EXIT_FAILURE); \
	} while(0)


struct print_ctx
{
	FILE *of;
	IDL_ns ns;
	IDL_tree tree;
	const char *idlfilename;
	const char *common_header_name;
	GHashTable *ifaces;
	int indent_level;

	/* error handling bits */
	jmp_buf fail_to;
};


struct param_info
{
	const char *name;
	IDL_tree type, param_dcl;
	uint8_t first_reg, last_reg;
};


/* TODO: add has_tagmask bool instead of special NO_TAGMASK value */
struct method_info
{
	IDL_tree node;
	uint16_t label;
	uint32_t tagmask;		/* tag mask, or NO_TAGMASK if not set */
	int num_params, num_out_params;
	struct param_info params[];
};

#define NO_TAGMASK ((uint32_t)~0u)


/* from muidl.c */

/* NOTE: code_f() and code_vf() add a newline to the output. */
extern int code_vf(struct print_ctx *pr, const char *fmt, va_list args);
extern int code_f(struct print_ctx *pr, const char *fmt, ...)
	__attribute__((format(printf, 2, 3)));

extern char *vtable_prefix(IDL_ns ns, IDL_tree iface);
extern GList *all_methods_of_iface(IDL_ns ns, IDL_tree iface);
extern char *decapsify(const char *name);
extern char *return_type(IDL_ns ns, IDL_tree opdcl, bool *real_p);
extern char *in_param_type(IDL_ns ns, IDL_tree tree);
extern char *long_name(IDL_ns ns, IDL_tree node);

/* returns the C type of value types and void. */
extern char *value_type(IDL_ns ns, IDL_tree type);

/* handles the "rigid" compound types, i.e. structs, unions, and arrays.
 * return value is allocated and must be g_free()'d.
 */
extern char *rigid_type(IDL_ns ns, IDL_tree type);

/* returns what should go in between this "type-section" and the variable name
 * to satisfy arbitrary aesthetic criteria.
 *
 * this doesn't play well with declaring multiple variables on a single line,
 * but fuck it -- i'm not carrying "type section" and "variable section"
 * separately.
 */
extern const char *type_space(const char *ctype);

/* returns true for strings that cannot appear as identifiers in C code. */
extern bool is_reserved_word(const char *str);

extern bool is_value_type(IDL_tree type);
extern bool is_rigid_type(IDL_ns ns, IDL_tree type);

/* chase typedefs down to the final (non-typedef) type. */
extern IDL_tree get_type_spec(IDL_tree node);

/* is this IDLN_EXCEPT_DCL a NegativeReturn exception? */
extern bool is_negs_exn(IDL_tree except_dcl);


/* from analyse.c */

extern struct method_info *analyse_op_dcl(struct print_ctx *pr, IDL_tree op);


/* from gen-common.c */

extern void print_common_header(struct print_ctx *pr);


#endif
