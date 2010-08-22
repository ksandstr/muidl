/*
 * l4x2.h -- L4.X2 related definitions and exports from l4x2.c
 * Copyright 2010  Kalle A. Sandström <ksandstr@iki.fi>
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

#ifndef SEEN_L4X2_H
#define SEEN_L4X2_H


/* offsets for use as second parameter to UTCB_ADDR_VAL(), wrapped in
 * CONST_INT().
 */
#define MR_OFFSET(n) (n)
#define BR_OFFSET(n) (-(n) - 16)
#define TCR_XFER_TIMEOUTS (-8)
#define TCR_ERROR_CODE (-9)
#define TCR_PAGER (-12)


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
extern LLVMValueRef build_label_from_tag(
	struct llvm_ctx *ctx,
	LLVMValueRef tag);

/* after return, dest[0] is the first word, dest[1] is the second word */
extern void build_simple_string_item(
	struct llvm_ctx *ctx,
	LLVMValueRef *dest,
	LLVMValueRef data_ptr,
	LLVMValueRef data_len,		/* in bytes */
	LLVMValueRef cache_hint);	/* NULL = default policy */

/* return value is new tpos */
extern LLVMValueRef build_recv_stritem_len(
	struct llvm_ctx *ctx,
	LLVMValueRef *nullpos_p,
	LLVMValueRef tpos);


#endif
