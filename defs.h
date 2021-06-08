/*
 * muidl.h -- extern definitions for µidl
 * Copyright 2009, 2010, 2011  Kalle A. Sandström <ksandstr@iki.fi>
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

#ifndef SEEN_MUIDL_DEFS_H
#define SEEN_MUIDL_DEFS_H

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>
#include <setjmp.h>
#include <libIDL/IDL.h>
#include <glib.h>
#include <ccan/strmap/strmap.h>

#include <llvm-c/Core.h>


/* this is useful for debugging missing args[] members, etc. */
#if 0
#define LLVMBuildStructGEP(a, b, c, d) ({ \
		fprintf(stderr, "LLVMBuildStructGEP called from %s:%d\n", __FILE__, __LINE__); \
		LLVMValueRef _v = LLVMBuildStructGEP((a), (b), (c), (d)); \
		_v; \
	})
#endif


/* FIXME: make this per-target, i.e. independent of the current platform. */
#define BITS_PER_WORD 32

/* this applies to in- and out-halves of operation separately. it's the maximum
 * length of "args" arrays, such as are passed to build_msg_encoder() and the
 * like.
 */
#define MAX_PARMS_PER_OP 32


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

#define ARRAY_TYPE_LENGTH(n) (IDL_INTEGER(IDL_LIST( \
	IDL_TYPE_ARRAY((n)).size_list).data).value)

#define SEQ_SUBTYPE(seq) (get_type_spec( \
	IDL_TYPE_SEQUENCE((seq)).simple_type_spec))
#define SEQ_BOUND_VAL(seq) (IDL_INTEGER( \
	IDL_TYPE_SEQUENCE((seq)).positive_int_const).value)

#define STR_BOUND_VAL(str) (IDL_INTEGER( \
	IDL_TYPE_STRING((str)).positive_int_const).value)



/* predicates for word, Fpage, MapGrantItem, unsigned short types */
#define NAT_IS(t, name) (IDL_NODE_TYPE((t)) == IDLN_NATIVE \
	&& strcmp(NATIVE_NAME((t)), (name)) == 0)
#define IS_MAPGRANT_TYPE(t) NAT_IS(t, "l4_mapgrantitem_t")
#define IS_FPAGE_TYPE(t) NAT_IS(t, "l4_fpage_t")
#define IS_WORD_TYPE(t) NAT_IS(t, "l4_word_t")
#define IS_TIME_TYPE(t) NAT_IS(t, "l4_time_t")

#define IS_USHORT_TYPE(op_type) \
	(IDL_NODE_TYPE((op_type)) == IDLN_TYPE_INTEGER \
	&& !IDL_TYPE_INTEGER((op_type)).f_signed \
	&& IDL_TYPE_INTEGER((op_type)).f_type == IDL_INTEGER_TYPE_SHORT)

#define IS_LONGLONG_TYPE(t) (IDL_NODE_TYPE((t)) == IDLN_TYPE_INTEGER \
	&& IDL_TYPE_INTEGER((t)).f_type == IDL_INTEGER_TYPE_LONGLONG)
#define IS_LONGDOUBLE_TYPE(t) (IDL_NODE_TYPE((t)) == IDLN_TYPE_FLOAT \
	&& IDL_TYPE_FLOAT((t)).f_type == IDL_FLOAT_TYPE_LONGDOUBLE)

/* LLVM predicate macros */
#define IS_VOID_TYPEREF(t) (LLVMGetTypeKind((t)) == LLVMVoidTypeKind)

/* is a <struct message_info *> a pointer to an exception message? */
#define IS_EXN_MSG(msg) (IDL_NODE_TYPE((msg)->node) == IDLN_EXCEPT_DCL)


/* diagnostics, or something -- all NOTDEFINED() and NOTSUPPORTED() states
 * should be caught in analysis functions, however.
 */
#define NOTDEFINED(t) do { \
		IDL_tree __t = (t); \
		if(IDL_NODE_TYPE(__t) == IDLN_NATIVE) { \
			fprintf(stderr, "%s:%d: %s not defined for native type `%s'\n", \
				__FILE__, (int)__LINE__, __func__, NATIVE_NAME(__t)); \
		} else { \
			fprintf(stderr, "%s:%d: %s not defined for <%s>\n", __FILE__, \
				(int)__LINE__, __func__, NODETYPESTR(__t)); \
		} \
		abort(); \
	} while(0)

#define NOTSUPPORTED(what) do { \
		fprintf(stderr, "%s:%d: µidl does not support %s\n", __FILE__, \
			(int)__LINE__, (what)); \
		abort(); \
	} while(0)

/* masked return values of op_timeout_kind().
 * these are from the stub's POV; the server only cares about transfer
 * timeouts.
 */
#define TIMEOUT_SEND (1 << 0)
#define TIMEOUT_RECV (1 << 1)

/* for fewer slip-ups in list handling.
 *
 * NOTE that this is not robust against modification from inside the list;
 * instead, accumulate a bunch of nodes and delete them afterward.
 */
#define GLIST_FOREACH(__name, list_head) \
	for(GList *__name = (list_head); \
		__name != NULL; \
		__name = g_list_next(__name))

#define IDL_LIST_FOREACH(__name, list) \
	for(IDL_tree __name = (list); \
		__name != NULL; \
		__name = IDL_LIST(__name).next)


struct print_ctx
{
	FILE *of;
	IDL_ns ns;
	IDL_tree tree;
	const char *idlfilename;
	const char *common_header_name;
	int indent_level;
	GList *ifaces;		/* of <struct iface_info *> */

	/* cleared at end of print_into() */
	GStringChunk *tmpstrchunk;

	/* error handling bits */
	jmp_buf fail_to;
};


/* talloc'd.
 *
 * TODO: rename this structure. it's not actually a LLVM context; call it a
 * build_ctx instead or something.
 */
struct llvm_ctx
{
	/* TODO: this should be talloc'd, parented into llvm_ctx. however, right
	 * now it's only ever allocated on the stack in muidl.c, so there's no
	 * actual need.
	 */
	struct print_ctx *pr;

	IDL_ns ns;
	LLVMContextRef ctx;
	LLVMModuleRef module;
	LLVMBuilderRef builder;
	LLVMTypeRef wordt, i32t, voidptrt, mapgrant;
	LLVMValueRef zero;
	GList *malloc_ptrs;		/* LLVM free insns emitted at fn end */

	void (*build_msgerr_bb)(struct llvm_ctx *);
	LLVMBasicBlockRef msgerr_bb;

	/* key is talloc'd under the containing llvm_ctx. (this map also contains
	 * the encoder functions despite its name.)
	 */
	STRMAP(LLVMValueRef) struct_decoder_fns;

	/* used by build_msg_{en,de}coder()
	 * address of UTCB (pointer to ctx->wordt)
	 */
	LLVMValueRef utcb;
	/* used by build_msg_decoder(); IPC receive regs */
	LLVMValueRef mr1, mr2, tag;

	/* dispatcher-specific things */
	LLVMBasicBlockRef reply_bb, wait_bb, alloc_bb;
	LLVMValueRef vtab_arg, reply_tag, errval_phi;
	LLVMValueRef tpos, tmax, from, tpos_mem;
	LLVMValueRef supp_ctx_ptr;

	/* stub-specific things */
	LLVMBasicBlockRef exit_bb;
	LLVMValueRef retval_phi;
};


enum msg_param_kind {
	P_UNTYPED,
	P_SEQ,		/* inline sequences */
	P_STRING,	/* string items */
	P_MAPPED,	/* [map] MapGrantItem */
};


struct msg_param
{
	const char *name;
	IDL_tree param_dcl, type;
	int param_ix, arg_ix;
	enum msg_param_kind kind;

	union {
		struct {
			int first_reg, last_reg;
			bool reg_manual;
		} untyped;
		struct {
			IDL_tree elem_type;
			int max_elems, bits_per_elem, elems_per_word;
			int min_words, max_words;
		} seq;
	} X;
};


/* TODO: add has_tagmask bool instead of special NO_TAGMASK value */
struct message_info
{
	uint16_t label;
	uint32_t tagmask;		/* tag mask, or NO_TAGMASK if not set */
	uint32_t sublabel;		/* MR1 label, or NO_SUBLABEL if not applicable */
	int tag_u;				/* before inline sequences, incl. label & retval */

	IDL_tree node;			/* IDL_{EXCEPT,OP}_DCL */

	int ctx_index;			/* exceptions only */

	/* return value spec. ret_type == NULL if void.
	 *
	 * the return value starts at MR1 if sublabel == NO_SUBLABEL, and at MR2
	 * otherwise.
	 */
	IDL_tree ret_type;
	bool ret_by_ref;		/* pass retval by ref or value in args[0]? */

	/* all parameters in IDL order. */
	GList *params;

	/* parameters that are passed in untyped words. may contain multi-word
	 * types such as constant-length structs that are encoded as words, or
	 * arrays of word-packable types.
	 */
	GList *untyped;

	/* sequence types that are passed inline. */
	GList *seq;

	/* typed parameters are MapGrantItems with the [map] attribute */
	GList *mapped;

	/* other things (long sequences and arrays, strings, wide strings) are
	 * passed as string items.
	 */
	GList *string;
};

#define NO_TAGMASK (~0u)
#define NO_SUBLABEL (~0u)


struct method_info
{
	bool oneway;
	int vtab_offset;
	const char *name;
	IDL_tree node;
	struct message_info *request;

	int num_reply_msgs;	/* oneway ? 0 : 1 + len(raises_list) */
	struct message_info *replies[];
};


struct stritem_info
{
	int length;			/* < 0 = terminator */
	bool stringlike;	/* '\0' terminated? (not included in length.) */

	/* these aren't filled in. they're used in different ways by dispatch.c and
	 * stub.c .
	 */
	int offset;
	LLVMValueRef memptr;	/* <i8 *> */
	LLVMValueRef lenptr;	/* <i32 *> or NULL when static */

	int reply_pos;		/* index in a method_info->replies */
	struct msg_param *param;	/* long param in a message_info->_long */
};


struct iface_info
{
	IDL_tree node;
	const char *name;
	GList *ops;		/* of <struct method_info *> */
	GList *tagmask_ops;		/* subset of ops */

	bool has_replies;	/* is at least one method is not oneway? */
};


/* the bit-packed format of a structure. */

struct packed_item {
	int word, bit;	/* offsets; word from start, bit from low end */
	int len;		/* length in bits; when >= BITS_PER_WORD, word-aligned */
	int dim;		/* length in items. when > 1, this item is an array */
	IDL_tree type;
	char name[];	/* member name; they are unique to the struct. */
};


struct packed_format
{
	int num_words, num_bits;
	int num_items;
	struct packed_item *items[];
};


/* generally in an array, terminated with type == NULL */
struct member_item
{
	IDL_tree type;
	const char *name;
	int dim;		/* 0 for non-array, >0 for array */
};

#define MEMBER_BITS(mi) (size_in_bits((mi)->type) * MAX(1, (mi)->dim))


/* from muidl.c */

extern gboolean arg_verbose;

/* NOTE: code_f() and code_vf() add a newline to the output. */
extern int code_vf(struct print_ctx *pr, const char *fmt, va_list args);
extern int code_f(struct print_ctx *pr, const char *fmt, ...)
	__attribute__((format(printf, 2, 3)));

extern void indent(struct print_ctx *pr, int change);
extern void close_brace(struct print_ctx *pr);


extern char *vtable_prefix(IDL_ns ns, IDL_tree iface);
/* if vtprefix_p != NULL, *vtprefix_p will be filled in with a string that
 * the caller must g_free().
 */
extern char *dispatcher_name(IDL_ns ns, IDL_tree iface, char **vtprefix_p);
extern char *decapsify(const char *name);
extern char *long_name(IDL_ns ns, IDL_tree node);

/* returns what should go in between this "type-section" and the variable name
 * to satisfy arbitrary aesthetic criteria.
 *
 * this doesn't play well with declaring multiple variables on a single line,
 * but fuck it -- i'm not carrying "type section" and "variable section"
 * separately.
 */
extern const char *type_space(const char *ctype)
	__attribute__((pure));

/* returns true for strings that cannot appear as identifiers in C code. */
extern bool is_reserved_word(const char *str)
	__attribute__((pure));

extern bool is_value_type(IDL_tree type) __attribute__((pure));
extern bool is_rigid_type(IDL_tree type);

/* chase typedefs down to the final (non-typedef) type. */
extern IDL_tree get_type_spec(IDL_tree node)
	__attribute__((pure));

/* find the type declaration of an IDL_ARRAY_TYPE. */
extern IDL_tree get_array_type(IDL_tree type);

/* whether the given type can be returned to a dispatcher without ambiguity
 * even when a NegativeReturn exception is declared. (i.e. tests for octet and
 * ushort.)
 */
extern bool is_real_nre_return_type(IDL_tree typ);

extern void print_msg_encoder(
	struct print_ctx *pr,
	const struct message_info *msg,
	const char *retval_str,
	const char *msg_str,
	const char *var_prefix);

extern void print_decode_inline_seqs(
	struct print_ctx *pr,
	const struct message_info *msg,
	const char *msg_str,
	const char *var_prefix);


/* from util.c */

/* access to the thread-global llvm_ctx, where defined. (when NULL, caller
 * should invoke NOTDEFINED().)
 */
extern struct llvm_ctx *get_llvm_ctx(void);
extern struct llvm_ctx *replace_llvm_ctx(struct llvm_ctx *ctx);

#define GET_CONTEXT() (get_llvm_ctx())

extern void free_message_info(struct message_info *inf);
extern void free_method_info(struct method_info *inf);
extern void free_iface_info(struct iface_info *inf, void *unused);

/* output a "warning: %s[fmt]", but only once per program execution.
 * returns true if a message was output.
 */
extern bool warn_once(const char *fmt, ...)
	__attribute__((format(printf, 1, 2)));

/* reset the warn_once() "already printed" set. */
extern void reset_warn_once(void);

/* find x, where x->param_dcl == pdecl, or return NULL. */
extern struct msg_param *find_pdecl(GList *params, IDL_tree pdecl);

/* turn an IDL:blah/ItemName:1.0 repo id into a valid 1:1 C identifier */
extern char *mangle_repo_id(const char *repo_id);

/* expand list of IDL_MEMBER, i.e. <type, [decls]>, into <type, name, dim>.
 * terminated by NULL type (because void is never a valid member type), and
 * caller-freed.
 */
extern struct member_item *expand_member_list(IDL_tree list);


/* these allocate from pr->tmpstrchunk. strings are valid until the next outer
 * print_into() returns.
 */
extern char *tmp_f(struct print_ctx *pr, const char *fmt, ...)
	__attribute__((format(printf, 2, 3)));
extern char *tmp_vf(struct print_ctx *pr, const char *fmt, va_list args);

/* "this file was generated with. modify this instead." */
extern void print_file_heading(struct print_ctx *pr);

extern void print_headers(
	struct print_ctx *pr,
	const char * const *strs,
	int len);

/* length lvalue for a sequence. for_dispatcher is true when called from a
 * dispatcher's reply encoder, and false when called from a stub. allocates
 * temp memory.
 */
extern const char *seq_len_lvalue(
	struct print_ctx *pr,
	IDL_tree param,
	const char *var_prefix,
	const char *name,
	bool for_dispatcher);

/* returns ptr to data object. */
extern LLVMValueRef build_local_storage(
	struct llvm_ctx *ctx,
	LLVMTypeRef type,
	LLVMValueRef count,
	const char *name);

/* returns ptr to data object. memory is free(2)'d at end of function. */
extern LLVMValueRef build_malloc_storage(
	struct llvm_ctx *ctx,
	LLVMTypeRef type,
	LLVMValueRef count,
	const char *name);

/* selects between stack and heap depending on size. */
extern LLVMValueRef build_seq_param_storage(
	struct llvm_ctx *ctx,
	IDL_tree ptyp,
	const char *name);


/* from analyse.c */

extern struct iface_info *analyse_interface(IDL_ns ns, IDL_tree iface);

extern struct method_info *analyse_op_dcl(IDL_ns ns, IDL_tree op);

extern struct message_info *build_exception_message(IDL_tree except_dcl);

/* result is a list of string buffers required by a dispatcher of the given
 * list of <struct method_info *>. caller frees. zero-length is represented by
 * NULL, or by the first item being the terminator.
 */
extern struct stritem_info *dispatcher_stritems(GList *methods);

/* same for stubs' out-halves. */
extern struct stritem_info *stub_stritems(const struct method_info *inf);

/* size of a rigid type in bits. negative if the type is a struct or union that
 * µIDL cannot pack into fewer than 64 untyped words.
 */
extern int size_in_bits(IDL_tree type);

/* size of a rigid type in words, when encoded by itself. */
extern int size_in_words(IDL_tree type);

/* a type's maximum length in bytes. mostly applicable to sequences, strings
 * and wide strings. returns negative for unbounded (used from verify.c).
 * length of strings doesn't include terminator as that's not passed over IPC.
 */
extern int max_size(IDL_tree type);

extern bool op_has_sublabel(IDL_tree op_ident);

/* returns true when a value was assigned, false when not, and exits with a
 * complaint when the value is malformed.
 *
 * TODO: the error handling method could be better, i spose.
 */
extern bool get_ul_property(
	unsigned long *value_p,
	IDL_tree ident,
	const char *name);


/* from verify.c */

/* true on success, false on failure */
extern bool verify_idl_input(IDL_ns ns, IDL_tree tree);


/* from attr.c */

#define has_property(ident, name) (IDL_tree_property_get((ident), (name)) != NULL)
#define has_map_property(ident) has_property(ident, "map")

extern bool is_packed(IDL_tree struct_type);

/* is this IDLN_EXCEPT_DCL a NegativeReturn, NoReply, or complex exception?
 *
 * (complex is defined as !neg && !noreply, but this is made available for use
 * with find_exn().)
 */
extern bool is_negs_exn(IDL_tree except_dcl);
extern bool is_noreply_exn(IDL_tree except_dcl);
extern bool is_complex_exn(IDL_tree except_dcl);


/* from op.c */

/* find an exception by predicate, or return NULL */
extern IDL_tree find_exn(IDL_tree opdcl, bool (*pred)(IDL_tree exn));
#define find_neg_exn(op) find_exn((op), &is_negs_exn)
#define find_noreply_exn(op) find_exn((op), &is_noreply_exn)
#define has_complex_exn(op) (find_exn((op), &is_complex_exn) != NULL)

extern bool has_pager_target(IDL_tree op_dcl);
extern bool has_mapped_param(IDL_tree opdcl);
extern int op_timeout_kind(IDL_tree opdcl);	/* returns mask of TIMEOUT_* */


/* from header.c */

extern void print_common_header(struct print_ctx *pr);

extern char *get_stub_prefix(IDL_tree opdcl);

extern char *return_type(
	IDL_ns ns,
	IDL_tree opdcl,
	bool *real_p,
	bool for_vtable);


/* from signature.c */

extern char *struct_signature(void *talctx, IDL_tree struct_type);


/* from common.c */

extern gboolean iter_build_common_module(IDL_tree_func_data *, void *);


/* from dispatch.c */

extern LLVMValueRef build_dispatcher_function(
	struct llvm_ctx *ctx,
	const struct iface_info *iface);
extern LLVMValueRef build_fetch_supp_ctx(struct llvm_ctx *ctx);


/* from stub.c */

extern LLVMValueRef build_stubs_for_iface(
	struct llvm_ctx *ctx,
	const struct iface_info *iface);


/* from iface.c */

/* gather and resolve all exceptions that operations of the given interface,
 * including interfaces it's derived from, references. the resulting list is of
 * <IDL_tree> sorted by repository ID.
 *
 * (the sort order of this function determines exception structure index in the
 * type returned by context_type_of_iface().)
 */
extern GList *iface_exns_sorted(IDL_ns ns, IDL_tree iface);

extern GList *all_methods_of_iface(IDL_ns ns, IDL_tree iface);


/* from except.c */

extern char *exn_raise_fn_name(IDL_tree except_dcl);

/* this is expensive. */
extern uint32_t exn_hash(IDL_tree except_dcl);

extern LLVMValueRef build_exception_raise_fns_for_iface(
	struct llvm_ctx *ctx,
	const struct iface_info *iface);

/* (returns NULL when nth is past the end. [horrible, yes.]) */
extern LLVMTypeRef context_type_of_iface(
	struct llvm_ctx *ctx,
	IDL_tree iface,
	int nth);

extern void build_decode_exception(
	struct llvm_ctx *ctx,
	LLVMValueRef ex_ptr,
	const struct message_info *exception,
	int exception_index,		/* for stritems[n]->reply_pos */
	const struct stritem_info *stritems);


/* from sequence.c */

/* returns new upos. */
extern LLVMValueRef build_decode_inline_sequence(
	struct llvm_ctx *ctx,
	LLVMValueRef *dst,			/* dst[0] = bufptr, dst[1] = &len */
	const struct msg_param *seq,
	LLVMValueRef upos,			/* i32 # of first unclaimed u-word */
	bool is_last,
	LLVMValueRef errval_phi,	/* adds incoming of -errno */
	LLVMBasicBlockRef err_bb);	/* ... and branches here to pop error */

/* returns new upos. */
extern LLVMValueRef build_encode_inline_sequence(
	struct llvm_ctx *ctx,
	LLVMValueRef mem,
	LLVMValueRef num_items,
	const struct msg_param *seq,
	LLVMValueRef upos,
	bool is_last);

extern void build_encode_array(
	struct llvm_ctx *ctx,
	LLVMValueRef mr_pos,
	IDL_tree type,
	int size,
	const LLVMValueRef *val);

void build_decode_array(
	struct llvm_ctx *ctx,
	LLVMValueRef *dst,
	LLVMValueRef first_mr,
	IDL_tree type,
	int size);


/* from types.c */

extern bool is_signed(IDL_tree type);
extern bool is_integral_type(IDL_tree type);

/* returns true if param_dcl != NULL && attr == IN && is_value_type. */
extern bool is_byval_param(IDL_tree param_dcl);

extern LLVMTypeRef llvm_value_type(struct llvm_ctx *ctx, IDL_tree type);
extern LLVMTypeRef llvm_rigid_type(struct llvm_ctx *ctx, IDL_tree type);

/* @names_p, if nonzero, will be filled in with a NULL-terminated string
 * vector. it should be freed afterward with g_strfreev(). its contents'
 * indexes correspond to field indexes in the returned LLVM typeref.
 */
extern LLVMTypeRef llvm_struct_type(
	struct llvm_ctx *ctx,
	char ***names_p,
	IDL_tree type);


/* from struct.c */

/* returns NULL when the structure type can't be represented in a bit-packed
 * format (i.e. it would use more than 63 untyped words).
 */
extern const struct packed_format *packed_format_of(IDL_tree struct_type);

/* see if the packed format is short enough to en/decode inline. */
extern bool is_short_fmt(const struct packed_format *fmt);

extern LLVMValueRef get_struct_fn(
	struct llvm_ctx *ctx,
	IDL_tree structtype,
	bool for_encode);

#define get_struct_decoder_fn(c, s) get_struct_fn((c), (s), false)
#define get_struct_encoder_fn(c, s) get_struct_fn((c), (s), true)

extern void decode_packed_struct(
	struct llvm_ctx *ctx,
	LLVMValueRef *dst_p,
	IDL_tree ctyp,
	LLVMValueRef first_mr,
	LLVMValueRef bit_offset);

extern void decode_packed_struct_inline(
	struct llvm_ctx *ctx,
	LLVMValueRef dstptrval,
	IDL_tree ctyp,
	LLVMValueRef first_mr,
	LLVMValueRef bit_offset);

extern void decode_packed_struct_fncall(
	struct llvm_ctx *ctx,
	LLVMValueRef dstptrval,
	IDL_tree ctyp,
	LLVMValueRef first_mr,
	LLVMValueRef bit_offset);

/* if bit_offset != NULL, the sub-word structure is encoded into first_mr_word,
 * and the result is returned.
 *
 * otherwise, the super-word structure is encoded into words and stored under
 * words starting from first_mr, and NULL is returned.
 */
extern LLVMValueRef encode_packed_struct(
	struct llvm_ctx *ctx,
	LLVMValueRef first_mr_word,
	LLVMValueRef bit_offset,
	IDL_tree ctyp,
	LLVMValueRef src_base);

extern LLVMValueRef encode_packed_struct_inline(
	struct llvm_ctx *ctx,
	LLVMValueRef first_mr_word,
	LLVMValueRef bit_offset,
	IDL_tree ctyp,
	LLVMValueRef src_base);

extern LLVMValueRef encode_packed_struct_fncall(
	struct llvm_ctx *ctx,
	LLVMValueRef first_mr_word,
	LLVMValueRef bit_offset,
	IDL_tree ctyp,
	LLVMValueRef src_base);


/* from message.c */

/* turn a sequence of out-parameters into L4 instructions that store the
 * appropriate values in the message registers. returns the message tag.
 */
extern LLVMValueRef build_msg_encoder(
	struct llvm_ctx *ctx,
	const struct message_info *msg,
	const LLVMValueRef *ret_args,
	const LLVMValueRef *args,
	bool is_out_half);

extern void build_msg_decoder(
	struct llvm_ctx *ctx,
	LLVMValueRef *dst_ret_args,
	LLVMValueRef *dst_args,
	const struct message_info *msg,
	int msg_index,		/* as in stritems[]->reply_pos */
	const struct stritem_info *stritems,
	bool is_out_half);

/* must only be used if mr_ix is a constant. */
extern LLVMValueRef build_ipc_input_val_ix(
	struct llvm_ctx *ctx,
	LLVMValueRef mr_ix,
	const char *name);

/* computes the register offset of the first non-valuetype parameter encoded as
 * untyped words. that is to say, for a message of "long a, long b, tiny_struct
 * c" would return 3 -- where "c" starts; the corresponding tag_u value is the
 * return value minus one.
 *
 * if no such a parameter exists, returns > 64.
 */
extern int msg_min_u(const struct message_info *msg);

/* when @ctyp is a value type, @dst[0] is assigned to the value.
 * when @ctyp is a rigid type, @dst[0] must be a pointer to the appropriate
 * type.
 * otherwise, @dst[0] must point to the first element of a buffer of sufficient
 * maximum size, and @dst[1] will be assigned to the length value (i32).
 */
extern void build_read_ipc_parameter(
	struct llvm_ctx *ctx,
	LLVMValueRef *dst,
	IDL_tree ctyp,
	LLVMValueRef first_mr);

/* similar interface as build_read_ipc_parameter():
 * for value types, val[0] is the value itself.
 * for rigid types, val[0] is a pointer to the value.
 * for sequence types, val[0] is a pointer to the first value and val[1] is the
 * length value (i32).
 */
extern void build_write_ipc_parameter(
	struct llvm_ctx *ctx,
	LLVMValueRef first_mr,
	IDL_tree ctyp,
	const LLVMValueRef *val);


/* from stringfn.c */

extern LLVMValueRef get_strlen_fn(struct llvm_ctx *ctx);
extern LLVMValueRef get_memcpy_fn(struct llvm_ctx *ctx);


#endif
