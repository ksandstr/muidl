/*
 * muidl.h -- extern definitions for µidl
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

#ifndef SEEN_MUIDL_H
#define SEEN_MUIDL_H

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

#include <llvm-c/Core.h>


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


/* predicates for word, Fpage, MapGrantItem, unsigned short types */
#define IS_MAPGRANT_TYPE(t) (IDL_NODE_TYPE((t)) == IDLN_NATIVE \
	&& strcmp(NATIVE_NAME((t)), "l4_mapgrantitem_t") == 0)
#define IS_FPAGE_TYPE(t) (IDL_NODE_TYPE((t)) == IDLN_NATIVE \
	&& strcmp(NATIVE_NAME((t)), "l4_fpage_t") == 0)
#define IS_WORD_TYPE(t) (IDL_NODE_TYPE((t)) == IDLN_NATIVE \
	&& strcmp(NATIVE_NAME((t)), "l4_word_t") == 0)
#define IS_TIME_TYPE(t) (IDL_NODE_TYPE((t)) == IDLN_NATIVE \
	&& strcmp(NATIVE_NAME((t)), "l4_time_t") == 0)

#define IS_USHORT_TYPE(op_type) \
	(IDL_NODE_TYPE((op_type)) == IDLN_TYPE_INTEGER \
	&& !IDL_TYPE_INTEGER((op_type)).f_signed \
	&& IDL_TYPE_INTEGER((op_type)).f_type == IDL_INTEGER_TYPE_SHORT)

#define IS_LONGLONG_TYPE(t) (IDL_NODE_TYPE((t)) == IDLN_TYPE_INTEGER \
	&& IDL_TYPE_INTEGER((t)).f_type == IDL_INTEGER_TYPE_LONGLONG)
#define IS_LONGDOUBLE_TYPE(t) (IDL_NODE_TYPE((t)) == IDLN_TYPE_FLOAT \
	&& IDL_TYPE_FLOAT((t)).f_type == IDL_FLOAT_TYPE_LONGDOUBLE)

#define IS_VOID_TYPEREF(t) (LLVMGetTypeKind((t)) == LLVMVoidTypeKind)


/* is a <struct message_info *> a pointer to an exception message? */
#define IS_EXN_MSG(msg) (IDL_NODE_TYPE((msg)->node) == IDLN_EXCEPT_DCL)


/* diagnostics, or something -- all NOTDEFINED() and NOTSUPPORTED() states
 * should be caught in analysis functions, however.
 */
#define NOTDEFINED(t) do { \
		fprintf(stderr, "%s:%d: %s not defined for <%s>\n", __FILE__, \
			(int)__LINE__, __FUNCTION__, NODETYPESTR((t))); \
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
#define GLIST_FOREACH(__name, list) \
	for(GList *__name = g_list_first((list)); \
		__name != NULL; \
		__name = g_list_next(__name))


struct print_ctx
{
	FILE *of;
	IDL_ns ns;
	IDL_tree tree;
	const char *idlfilename;
	const char *common_header_name;
	GHashTable *ifaces;
	int indent_level;

	/* cleared at end of print_into() */
	GStringChunk *tmpstrchunk;

	/* error handling bits */
	jmp_buf fail_to;
};


/* TODO: rename this structure. it's not actually a LLVM context; call it a
 * build_ctx instead or something.
 */
struct llvm_ctx
{
	struct print_ctx *pr;
	IDL_ns ns;
	LLVMContextRef ctx;
	LLVMModuleRef module;
	LLVMBuilderRef builder;
	LLVMTypeRef wordt, i32t, voidptrt, mapgrant;
	LLVMValueRef zero;
	GList *malloc_ptrs;		/* LLVM free insns emitted at fn end */

	/* key: char * of struct repo id, value: LLVMValueRef of fn. key is
	 * freed.
	 *
	 * (also contains the encoder functions.)
	 */
	GHashTable *struct_decoder_fns;

	void (*build_msgerr_bb)(struct llvm_ctx *);

	/* used by dispatcher building etc; common L4 IPC stuff */
	LLVMValueRef utcb;		/* address of UTCB; <L4_Word_t *> */
	LLVMValueRef from, mr1, mr2, tag;	/* IPC receive regs */

	/* dispatcher-specific things */
	LLVMBasicBlockRef reply_bb, msgerr_bb, wait_bb, alloc_bb;
	LLVMValueRef vtab_arg, reply_tag, errval_phi, inline_seq_pos;
	LLVMValueRef tpos, tmax;
	LLVMValueRef stritem_len_fn;
};


enum msg_param_kind {
	P_UNTYPED,
	P_SEQ,
	P_LONG,
};


struct msg_param
{
	const char *name;
	IDL_tree param_dcl;
	int param_ix, arg_ix;
	enum msg_param_kind kind;

	union {
		struct {
			IDL_tree type;
			int first_reg, last_reg;
			bool reg_manual;
		} untyped;
		struct {
			IDL_tree elem_type;
			int max_elems, bits_per_elem, elems_per_word;
			int min_words, max_words;
		} seq;
		struct {
			IDL_tree type;
		} _long;
	} X;
};


/* TODO: add has_tagmask bool instead of special NO_TAGMASK value */
struct message_info
{
	uint16_t label;
	uint32_t tagmask;		/* tag mask, or NO_TAGMASK if not set */
	uint32_t sublabel;		/* MR1 label, or NO_SUBLABEL if not applicable */
	int tag_u;				/* before inline sequences, incl. label & retval */
	int tag_t;

	IDL_tree node;			/* IDL_{EXCEPT,OP}_DCL */

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

	/* other things (long sequences, long arrays, variable-length struct types)
	 * that are passed as string items.
	 */
	GList *_long;
};

#define NO_TAGMASK (~0u)
#define NO_SUBLABEL (~0u)


struct method_info
{
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

	/* these aren't filled in by dispatcher_stritems() */
	int offset;
	LLVMValueRef memptr;	/* memory in dispatcher; always <i8 *> */
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
extern GList *all_methods_of_iface(IDL_ns ns, IDL_tree iface);
extern char *decapsify(const char *name);
extern char *return_type(
	IDL_ns ns,
	IDL_tree opdcl,
	bool *real_p,
	bool for_vtable);
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
extern const char *type_space(const char *ctype)
	__attribute__((pure));

/* returns true for strings that cannot appear as identifiers in C code. */
extern bool is_reserved_word(const char *str)
	__attribute__((pure));

extern bool is_value_type(IDL_tree type) __attribute__((pure));
extern bool is_rigid_type(IDL_ns ns, IDL_tree type);

/* chase typedefs down to the final (non-typedef) type. */
extern IDL_tree get_type_spec(IDL_tree node)
	__attribute__((pure));

/* find the type declaration of an IDL_ARRAY_TYPE. */
extern IDL_tree get_array_type(IDL_tree type);

/* is this IDLN_EXCEPT_DCL a NegativeReturn exception? */
extern bool is_negs_exn(IDL_tree except_dcl);

/* find the NegativeReturn exception, or return NULL */
extern IDL_tree find_neg_exn(IDL_tree opdcl);

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

/* g_free()s every element, then g_list_free()s the list */
extern void list_dispose(GList *list);
extern void free_message_info(struct message_info *inf);
extern void free_method_info(struct method_info *inf);

/* output a "warning: %s[fmt]", but only once per program execution.
 * returns true if a message was output.
 */
extern bool warn_once(const char *fmt, ...)
	__attribute__((format(printf, 1, 2)));

/* reset the warn_once() "already printed" set. */
extern void reset_warn_once(void);

/* find x, where x->param_dcl == pdecl, or return NULL. */
extern struct msg_param *find_pdecl(GList *params, IDL_tree pdecl);


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


/* from analyse.c */

extern struct method_info *analyse_op_dcl(struct print_ctx *pr, IDL_tree op);

/* collect all operations of the interface given (including those in
 * subclasses, as per all_methods_of_iface()), separate tag-masked operations
 * into a given list, sort the rest by ascending label and return. result is a
 * GList of <struct method_info *> with vtab_offset filled in for each.
 *
 * the caller must do ``g_list_foreach(result, (GFunc)free_method_info,
 * NULL);'' followed by ``g_list_free(result); g_list_free(*tagmask_list_p);''
 * to not leak memory.
 */
extern GList *analyse_methods_of_iface(
	struct print_ctx *pr,
	GList **tagmask_list_p,
	IDL_tree iface);


/* result is a list of string buffers required by a dispatcher of the given
 * list of <struct method_info *>. caller frees. zero-length is represented by
 * NULL, or by the first item being the terminator.
 */
extern struct stritem_info *dispatcher_stritems(GList *methods);

/* size of a rigid type in bits. negative if the type is a struct or union that
 * µIDL cannot pack into fewer than 64 untyped words.
 */
extern int size_in_bits(IDL_tree type);


/* from verify.c */

/* true on success, false on failure */
extern bool verify_idl_input(IDL_ns ns, IDL_tree tree);


/* from header.c */

extern void print_common_header(struct print_ctx *pr);

extern int op_timeout_kind(IDL_tree opdcl);	/* returns mask of TIMEOUT_* */

extern char *get_stub_prefix(IDL_tree opdcl);


/* from gen-stubs.c */

extern void print_stubs_file(struct print_ctx *pr);
extern void print_generic_stub_decl(
	struct print_ctx *pr,
	const char *stubpfx,
	IDL_tree op,
	int timeout_kind);		/* nonzero adds _timeout to stub name */


/* from common.c */

extern gboolean iter_build_common_module(IDL_tree_func_data *, void *);


/* from dispatch.c */

extern gboolean iter_build_dispatchers(IDL_tree_func_data *tf, void *ctx);


/* from stub.c */

extern gboolean iter_build_stubs(IDL_tree_func_data *tf, void *ctxptr);


/* from l4x2.c */

/* returned valueref is a <L4_Word_t *> of the UTCB address. */
extern LLVMValueRef build_utcb_get(struct llvm_ctx *ctx);

/* returned valueref is L4_Word_t mr0 (message tag).
 *
 * never overwrites the from, mr1, mr2 valuerefs in *ctx; if you want that,
 * give their pointers as the out-values.
 */
extern LLVMValueRef build_l4_ipc_call(
	struct llvm_ctx *ctx,
	LLVMValueRef arg_to,
	LLVMValueRef arg_timeouts,
	LLVMValueRef arg_fromspec,
	LLVMValueRef arg_mr0,
	LLVMValueRef *from_p,		/* may be NULL */
	LLVMValueRef *mr1_p,
	LLVMValueRef *mr2_p);

/* ix is an utcb index, i.e. when positive, MR index. */
extern LLVMValueRef build_utcb_load(
	struct llvm_ctx *ctx,
	LLVMValueRef ix,
	const char *name);

extern LLVMValueRef build_u_from_tag(struct llvm_ctx *ctx, LLVMValueRef tag);
extern LLVMValueRef build_t_from_tag(struct llvm_ctx *ctx, LLVMValueRef tag);

/* after return, dest[0] is the first word, dest[1] is the second word */
extern void build_simple_string_item(
	struct llvm_ctx *ctx,
	LLVMValueRef *dest,
	LLVMValueRef data_ptr,
	LLVMValueRef data_len,		/* in bytes */
	LLVMValueRef cache_hint);	/* NULL = default policy */

/* return value is new tpos */
LLVMValueRef build_recv_stritem_len(
	struct llvm_ctx *ctx,
	LLVMValueRef *nullpos_p,
	LLVMValueRef tpos);


/* from sequence.c */

/* returns new upos. */
extern LLVMValueRef build_decode_inline_sequence(
	struct llvm_ctx *ctx,
	LLVMValueRef *dst,			/* must have room for two items */
	const struct msg_param *seq,
	LLVMValueRef upos,			/* i32 # of first unclaimed u-word */
	bool is_last,
	LLVMValueRef errval_phi,	/* adds incoming of -errno */
	LLVMBasicBlockRef err_bb);	/* ... and branches here to pop error */

/* returns new upos. */
extern LLVMValueRef build_encode_inline_sequence(
	struct llvm_ctx *ctx,
	LLVMValueRef mem,
	LLVMValueRef lenptr,
	const struct msg_param *seq,
	LLVMValueRef upos,
	bool is_last);


/* from types.c */

extern bool is_signed(IDL_tree type);
extern bool is_integral_type(IDL_tree type);

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

/* when @ctyp is a value type, @dst[0] is assigned to the value.
 * when @ctyp is a rigid type, @dst[0] must be a pointer to the appropriate
 * type.
 * otherwise, @dst[0] must point to the first element of a buffer of sufficient
 * maximum size, and @dst[1] will be assigned to the length value (i32).
 *
 * this function should be called build_read_ipc_argument() instead; parameters
 * are always translated according to direction attribute anyhow.
 *
 * FIXME: doesn't really belong in types.c but rather in a message.c or some
 * such.
 */
extern void build_read_ipc_parameter_ixval(
	struct llvm_ctx *ctx,
	LLVMValueRef *dst,
	IDL_tree ctyp,
	LLVMValueRef first_mr);

/* similar interface as build_read_ipc_parameter_ixval():
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


/* from struct.c */

/* returns NULL when the structure type can't be represented in a bit-packed
 * format (i.e. it would use more than 63 untyped words).
 */
extern const struct packed_format *packed_format_of(IDL_tree struct_type);

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
	const LLVMValueRef *args,
	bool is_out_half);

extern void build_msg_decoder(
	struct llvm_ctx *ctx,
	LLVMValueRef *dst_args,
	const struct message_info *msg,
	const struct stritem_info *stritems,
	bool is_out_half);

#endif
