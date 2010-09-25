/*
 * struct.c -- handling of struct and union types
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

#include <glib.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>
#include <libIDL/IDL.h>

#include "muidl.h"
#include "llvmutil.h"
#include "l4x2.h"


static GHashTable *packed_cache = NULL;


static struct packed_item *new_packed_item(
	int word,
	int bit,
	int len,
	const struct member_item *ci)
{
	int namelen = strlen(ci->name);
	struct packed_item *ret = g_malloc(sizeof(struct packed_item)
		+ sizeof(char) * (namelen + 1));
	ret->word = word;
	ret->bit = bit;
	ret->len = len;
	ret->dim = MAX(1, ci->dim);
	ret->type = ci->type;
	memcpy(ret->name, ci->name, namelen + 1);
	return ret;
}


static void cut_gpa(GPtrArray *dst, GPtrArray *src, gsize keep)
{
	for(int i=0; i<dst->len; i++) g_free(dst->pdata[i]);
	g_ptr_array_set_size(dst, src->len - keep);
	memcpy(&dst->pdata[0], &src->pdata[keep], dst->len * sizeof(void *));
	g_ptr_array_set_size(src, keep);
}


static int pack_items(
	GPtrArray *output,
	GList **items_per_size,
	const int num_items,
	const int *word_occ,
	const int num_words,
	const int max_sol)
{
	/* base case. */
	if(num_items == 0) return num_words;

	const int start_len = output->len;
	int my_occ[num_words + 1];
	memcpy(my_occ, word_occ, sizeof(int) * num_words);
	my_occ[num_words] = 0;
	GPtrArray *best_output = g_ptr_array_new();
	int best_num_words = max_sol, best_size = 0, best_wn = -1;
	/* for the first item of each size (where available) */
	assert(items_per_size[0] == NULL);
	for(int sz=1; sz < (BITS_PER_WORD - 1); sz++) {
		if(items_per_size[sz] == NULL) continue;
		GList *link = items_per_size[sz];
		struct member_item *ci = link->data;
		const int nbits = MEMBER_BITS(ci);
		items_per_size[sz] = g_list_delete_link(items_per_size[sz], link);

		/* try different destination words. */
		for(int wn=0; wn < num_words; wn++) {
			if(my_occ[wn] + nbits > BITS_PER_WORD) continue;
			struct packed_item *pi = new_packed_item(wn, my_occ[wn],
				nbits, ci);
			g_ptr_array_add(output, pi);
			my_occ[wn] += nbits;
			assert(my_occ[wn] <= BITS_PER_WORD);
			int sub_num_words = pack_items(output, items_per_size,
				num_items - 1, my_occ, num_words, best_num_words);
			assert(start_len <= output->len);
			if(sub_num_words < best_num_words) {
				/* record the subsolution */
				best_num_words = sub_num_words;
				best_size = sz;
				best_wn = wn;
				cut_gpa(best_output, output, start_len);
			} else if(start_len < output->len) {
				/* chuck it. */
				for(int i=start_len; i<output->len; i++) {
					g_free(output->pdata[i]);
				}
				g_ptr_array_set_size(output, start_len);
			}
			my_occ[wn] -= nbits;
			assert(my_occ[wn] > 0);
		}

		/* and the solution where we allocate a new word at the end,
		 * if the parameters permit.
		 */
		if(num_words + 1 <= max_sol) {
			struct packed_item *pi = new_packed_item(num_words, 0,
				nbits, ci);
			g_ptr_array_add(output, pi);
			my_occ[num_words] = nbits;
			int sub_num_words = pack_items(output, items_per_size,
				num_items - 1, my_occ, num_words + 1, best_num_words);
			/* (NOTE: copypasted from inside the loop, above.) */
			assert(start_len <= output->len);
			if(sub_num_words < best_num_words) {
				/* record the subsolution */
				best_num_words = sub_num_words;
				best_size = sz;
				best_wn = num_words;
				cut_gpa(best_output, output, start_len);
			} else if(start_len < output->len) {
				/* chuck it. */
				for(int i=start_len; i<output->len; i++) {
					g_free(output->pdata[i]);
				}
				g_ptr_array_set_size(output, start_len);
			}
		}

		/* restore the item. */
		items_per_size[sz] = g_list_prepend(items_per_size[sz], ci);
	}

	if(best_wn == -1) {
		/* no solution down here. */
		g_ptr_array_free(best_output, TRUE);
		return max_sol + 1;
	} else {
		assert(best_size > 0 && best_wn >= 0);
		/* return with the optimal solution. */
		for(int i=0; i<best_output->len; i++) {
			g_ptr_array_add(output, best_output->pdata[i]);
		}
		g_ptr_array_free(best_output, TRUE);
		return best_num_words;
	}
}


static int item_by_bitsize_cmp(gconstpointer a, gconstpointer b)
{
	const struct member_item *ca = a, *cb = b;
	int asize = MEMBER_BITS(ca), bsize = MEMBER_BITS(cb);
	if(asize > bsize) return 1;
	else if(asize < bsize) return -1;
	else return 0;
}


const struct packed_format *packed_format_of(IDL_tree stype)
{
	if(packed_cache == NULL) {
		/* NOTE: if there ever is a flush_packed_cache() type function, it
		 * should go over the hash table's bits and free each key
		 * (g_strdup()'d) and value (g_new()'d).
		 */
		packed_cache = g_hash_table_new(&g_str_hash, &g_str_equal);
	}

	const char *s_id = IDL_IDENT(IDL_TYPE_STRUCT(stype).ident).repo_id;
	struct packed_format *ret = g_hash_table_lookup(packed_cache, s_id);
	if(ret != NULL) return ret;

	struct member_item *items = expand_member_list(
		IDL_TYPE_STRUCT(stype).member_list);
	int num_items = 0;
	while(items[num_items].type != NULL) num_items++;
	qsort(items, num_items, sizeof(struct member_item), &item_by_bitsize_cmp);

	/* packing of small (sub-word) items */
	GList *items_by_size[BITS_PER_WORD - 1];
	for(int i=0; i < (BITS_PER_WORD - 1); i++) {
		items_by_size[i] = NULL;
	}
	int num_small = 0;
	for(int i=0; i<num_items; i++) {
		struct member_item *item = &items[i];
		/* TODO: produce N items for arrays where bits_each < BITS_PER_WORD, so
		 * that smaller items can be packed after e.g. an array member that
		 * leaves 11 bits unused in each word.
		 */
		int bits = MEMBER_BITS(item);
		if(bits >= BITS_PER_WORD) break;
		items_by_size[bits] = g_list_prepend(items_by_size[bits], item);
		num_small++;
	}
	for(int i=0; i < (BITS_PER_WORD - 1); i++) {
		items_by_size[i] = g_list_reverse(items_by_size[i]);
	}
	GPtrArray *packed = g_ptr_array_new();
	int num_words = pack_items(packed, items_by_size, num_small, NULL, 0, 64);
	if(num_words > 63) {
		warn_once("structure `%s' can't be bit-packed\n", s_id);
		return NULL;
	}
	assert(num_words < 64);
	for(int i=0; i < (BITS_PER_WORD - 1); i++) {
		g_list_free(items_by_size[i]);
	}

#if 0
	printf("%s: packed %d/%d small items into %d words from `%s'\n",
		__func__, (int)packed->len, num_small, num_words, s_id);
#endif

	/* packing of word-length, and longer, items */
	for(int i=0; i<num_items; i++) {
		struct member_item *item = &items[i];
		int nbits = MEMBER_BITS(item);
		if(nbits < BITS_PER_WORD) continue;
		g_ptr_array_add(packed, new_packed_item(num_words, 0, nbits, item));
		int words = (nbits + BITS_PER_WORD - 1) / BITS_PER_WORD;
#if 0
		printf("%s: packing item `%s' of %d words (%d bits) as-is\n",
			__func__, item->name, words, nbits);
#endif
		num_words += words;
	}

#if 0
	printf("%s: packed %d items into %d words from `%s'\n",
		__func__, items->len, num_words, s_id);
	for(int i=0; i<packed->len; i++) {
		const struct packed_item *pi = packed->pdata[i];
		printf("... `%s' -> word %d, bit %d\n", pi->name, pi->word,
			pi->bit);
	}
#endif

	assert(packed->len == num_items);
	g_free(items);
	items = NULL;

	ret = g_malloc(sizeof(struct packed_format)
		+ sizeof(struct packed_item *) * packed->len);
	ret->num_words = num_words;
	ret->num_items = packed->len;
	memcpy(ret->items, &g_ptr_array_index(packed, 0),
		packed->len * sizeof(void *));
	g_ptr_array_free(packed, TRUE);
	ret->num_bits = 0;
	for(int i=0; i<ret->num_items; i++) {
		ret->num_bits += ret->items[i]->len;
	}
	g_hash_table_insert(packed_cache, g_strdup(s_id), ret);

	return ret;
}


/* struct decoding. */

void decode_packed_struct_inline(
	struct llvm_ctx *ctx,
	LLVMValueRef dst,
	IDL_tree ctyp,
	LLVMValueRef first_mr,
	LLVMValueRef bit_offset)
{
	const struct packed_format *fmt = packed_format_of(ctyp);
	assert(fmt != NULL);
	char **names = NULL;
	T s_type = llvm_struct_type(ctx, &names, ctyp);
	int names_len = 0;
	while(names[names_len] != NULL) names_len++;
	assert(names_len == fmt->num_items);
	assert(LLVMCountStructElementTypes(s_type) == names_len);
	T types[names_len];
	LLVMGetStructElementTypes(s_type, types);
	int cur_word = 0;
	V wordval = NULL;
	for(int i=0; i<fmt->num_items; i++) {
		const struct packed_item *pi = fmt->items[i];
		int field_ix = 0;
		while(names[field_ix] != NULL
			&& strcmp(names[field_ix], pi->name) != 0)
		{
			field_ix++;
		}
		if(names[field_ix] == NULL) {
			fprintf(stderr, "%s: not the way to go.\n", __func__);
			abort();
		}

		V start_mr = LLVMBuildAdd(ctx->builder, first_mr,
			CONST_INT(pi->word), tmp_f(ctx->pr, "%s.start.mr", pi->name));
		V dstptr = LLVMBuildStructGEP(ctx->builder, dst, field_ix,
			tmp_f(ctx->pr, "%s.start.ptr", pi->name));
		if(pi->dim > 1) {
			/* array types. TODO */
			fprintf(stderr, "%s: struct-member arrays not implemented\n",
				__func__);
			abort();
		} else if(IDL_NODE_TYPE(pi->type) == IDLN_TYPE_STRUCT) {
			decode_packed_struct(ctx, &dstptr, pi->type, start_mr,
				CONST_INT(pi->bit));
		} else if(IDL_NODE_TYPE(pi->type) == IDLN_TYPE_UNION) {
			fprintf(stderr, "%s: union-member types not implemented\n",
				__func__);
			abort();
		} else if(IS_LONGLONG_TYPE(pi->type)) {
			/* long long on a 32-bit architecture. can be #ifdef'd out for
			 * 64-bit targets, where it'd be just a value type.
			 */
			V dtmp = NULL;
			build_read_ipc_parameter(ctx, &dtmp, pi->type,
				start_mr);
			LLVMBuildStore(ctx->builder, dtmp, dstptr);
		} else if(is_value_type(pi->type)) {
			/* word-size and smaller items. */
			if(cur_word != pi->word || wordval == NULL) {
				cur_word = pi->word;
				V old_wv = wordval;
				wordval = build_ipc_input_val_ix(ctx, start_mr,
					tmp_f(ctx->pr, "st.word%d", pi->word));
				if(old_wv == NULL) {
					/* shift it down, since we start at an offset. */
					wordval = LLVMBuildLShr(ctx->builder, wordval,
						bit_offset, "st.bitoffs.shifted");
				}
			}
			V subval = LLVMBuildLShr(ctx->builder, wordval,
				CONST_INT(pi->bit),
				tmp_f(ctx->pr, "st.word%d.shr%d", pi->word, pi->bit));
			if(pi->len < BITS_PER_WORD) {
				subval = LLVMBuildAnd(ctx->builder, subval,
					CONST_WORD((1 << pi->len) - 1),
					tmp_f(ctx->pr, "st.word%d.s%d.m%d", pi->word, pi->bit,
						pi->len));
			}
			subval = LLVMBuildTruncOrBitCast(ctx->builder, subval,
				types[field_ix], "st.val.cast");
			LLVMBuildStore(ctx->builder, subval, dstptr);
		} else if(IS_MAPGRANT_TYPE(pi->type)) {
			/* two words, like peas in a pod */
			assert(pi->bit == 0);
			build_read_ipc_parameter(ctx, &dstptr, pi->type, start_mr);
		} else {
			NOTDEFINED(pi->type);
		}
	}
	g_strfreev(names);
}


/* call a function to decode the structure. */
void decode_packed_struct_fncall(
	struct llvm_ctx *ctx,
	LLVMValueRef dstptr,
	IDL_tree ctyp,
	LLVMValueRef first_mr,
	LLVMValueRef bit_offset)
{
	V decode_fn = get_struct_decoder_fn(ctx, ctyp);
	const struct packed_format *fmt = packed_format_of(ctyp);
	assert(fmt != NULL);
	if(fmt->num_bits < BITS_PER_WORD) {
		/* decoder function does have a bit-offset parameter. */
		V parms[3] = { dstptr, first_mr, bit_offset };
		LLVMBuildCall(ctx->builder, decode_fn, parms, 3, "");
	} else {
		V parms[2] = { dstptr, first_mr };
		LLVMBuildCall(ctx->builder, decode_fn, parms, 2, "");
	}
}


/* see if the packed format is short enough to en/decode inline. */
bool is_short_fmt(const struct packed_format *fmt)
{
	for(int i=0, sub=0; i < fmt->num_items; i++) {
		const struct packed_item *item = fmt->items[i];
		if(item->len < BITS_PER_WORD) sub++;
		if(i > 3 || sub > 2) return false;
	}
	return true;
}


void decode_packed_struct(
	struct llvm_ctx *ctx,
	LLVMValueRef *dst_p,
	IDL_tree ctyp,
	LLVMValueRef first_mr,
	LLVMValueRef bit_offset)
{
	if(*dst_p == NULL) {
		/* TODO: fold this allocation with struct fields from other decoders,
		 * if invoked from a dispatcher. somehow. (v2?)
		 */
		T s_type = llvm_struct_type(ctx, NULL, ctyp);
		const char *s_name = IDL_IDENT(IDL_TYPE_STRUCT(ctyp).ident).str;
		*dst_p = build_local_storage(ctx, s_type, NULL, s_name);
	}
	const struct packed_format *fmt = packed_format_of(ctyp);
	assert(fmt != NULL);
	if(is_short_fmt(fmt)) {
		decode_packed_struct_inline(ctx, *dst_p, ctyp, first_mr, bit_offset);
	} else {
		decode_packed_struct_fncall(ctx, *dst_p, ctyp, first_mr, bit_offset);
	}
}


/* struct encoding. */

LLVMValueRef encode_packed_struct_inline(
	struct llvm_ctx *ctx,
	LLVMValueRef first_mr_word,
	LLVMValueRef bit_offset,
	IDL_tree ctyp,
	LLVMValueRef src_base)
{
	const struct packed_format *fmt = packed_format_of(ctyp);
	assert(fmt != NULL);

	assert(bit_offset == NULL || (fmt->num_bits < BITS_PER_WORD
		&& fmt->num_words == 1));
	assert(bit_offset != NULL || fmt->num_bits >= BITS_PER_WORD);

	char **names = NULL;
	T s_type = llvm_struct_type(ctx, &names, ctyp);
	int names_len = 0;
	while(names[names_len] != NULL) names_len++;
	assert(names_len == fmt->num_items);
	assert(LLVMCountStructElementTypes(s_type) == names_len);
	T types[names_len];
	LLVMGetStructElementTypes(s_type, types);
	int cur_word = 0;
	V wordval = CONST_WORD(0);
	for(int i=0; i<fmt->num_items; i++) {
		const struct packed_item *pi = fmt->items[i];
		assert(bit_offset == NULL || pi->word == 0);
		int field_ix = 0;
		while(names[field_ix] != NULL
			&& strcmp(names[field_ix], pi->name) != 0)
		{
			field_ix++;
		}
		if(names[field_ix] == NULL) {
			fprintf(stderr, "%s: not the way to go.\n", __func__);
			abort();
		}

		V start_mr = NULL;
		if(bit_offset == NULL) {
			start_mr = LLVMBuildAdd(ctx->builder, first_mr_word,
				CONST_INT(pi->word), "word.ix");
		}

		/* flush previous word? */
		if(bit_offset == NULL && cur_word != pi->word) {
			V mr_ix = LLVMBuildAdd(ctx->builder, first_mr_word,
				CONST_INT(cur_word), tmp_f(ctx->pr, "flush.w%d.ix", cur_word));
			LLVMBuildStore(ctx->builder, wordval, UTCB_ADDR_VAL(ctx, mr_ix,
				tmp_f(ctx->pr, "flush.w%d.addr", cur_word)));
			wordval = CONST_WORD(0);
			cur_word = pi->word;
		}

		V valptr = LLVMBuildStructGEP(ctx->builder, src_base, field_ix,
			tmp_f(ctx->pr, "%s.start.ptr", pi->name));
		if(pi->dim > 1) {
			/* array types. TODO */
			fprintf(stderr, "%s: struct-member arrays not implemented\n",
				__func__);
			abort();
		} else if(IDL_NODE_TYPE(pi->type) == IDLN_TYPE_STRUCT) {
			if(pi->len < BITS_PER_WORD) {
				wordval = encode_packed_struct(ctx, wordval,
					CONST_INT(pi->bit), pi->type, valptr);
			} else {
				encode_packed_struct(ctx, start_mr, NULL, pi->type, valptr);
			}
		} else if(IDL_NODE_TYPE(pi->type) == IDLN_TYPE_UNION) {
			fprintf(stderr, "%s: union-member types not implemented\n",
				__func__);
			abort();
		} else if(IS_LONGLONG_TYPE(pi->type)) {
			assert(bit_offset == NULL);
			V val = LLVMBuildLoad(ctx->builder, valptr, "longlong.fld");
			build_write_ipc_parameter(ctx, start_mr, pi->type, &val);
		} else if(is_value_type(pi->type)) {
			/* word-size and smaller items. */
			V val = LLVMBuildLoad(ctx->builder, valptr,
				tmp_f(ctx->pr, "fld.%s.val", pi->name));
			val = LLVMBuildZExtOrBitCast(ctx->builder, val, ctx->wordt,
				tmp_f(ctx->pr, "fld.%s.word", pi->name));
			V bitoffs = CONST_INT(pi->bit);
			V shifted = LLVMBuildShl(ctx->builder, val, bitoffs,
				tmp_f(ctx->pr, "fld.%s.shifted", pi->name));
			wordval = LLVMBuildOr(ctx->builder, shifted, wordval,
				tmp_f(ctx->pr, "word%d.%s.merged", cur_word, pi->name));
		} else if(IS_MAPGRANT_TYPE(pi->type)) {
			assert(pi->bit == 0);
			assert(bit_offset == NULL);
			build_write_ipc_parameter(ctx, start_mr, pi->type, &valptr);
		} else {
			NOTDEFINED(pi->type);
		}
	}

	if(bit_offset == NULL) {
		/* flush final word */
		V mr_ix = LLVMBuildAdd(ctx->builder, first_mr_word,
			CONST_INT(cur_word), tmp_f(ctx->pr, "flush.w%d.ix", cur_word));
		LLVMBuildStore(ctx->builder, wordval, UTCB_ADDR_VAL(ctx, mr_ix,
			tmp_f(ctx->pr, "flush.w%d.addr", cur_word)));
		wordval = NULL;
	} else {
		/* shift and merge */
		wordval = LLVMBuildOr(ctx->builder, first_mr_word,
			LLVMBuildShl(ctx->builder, wordval, bit_offset, "short.shifted"),
			"rv.merged");
	}

	g_strfreev(names);

	return wordval;
}


LLVMValueRef encode_packed_struct_fncall(
	struct llvm_ctx *ctx,
	LLVMValueRef first_mr_word,
	LLVMValueRef bit_offset,
	IDL_tree ctyp,
	LLVMValueRef src_base)
{
	V fn = get_struct_encoder_fn(ctx, ctyp);
	const struct packed_format *fmt = packed_format_of(ctyp);
	assert(fmt != NULL);
	if(fmt->num_bits < BITS_PER_WORD) {
		/* subword structure; pass bit offset & value. */
		if(bit_offset == NULL) bit_offset = CONST_INT(0);
		/* NOTE: (word) const void *, word, i32 */
		V parms[3] = { src_base, first_mr_word, bit_offset };
		return LLVMBuildCall(ctx->builder, fn, parms, 3, "wordval.next");
	} else {
		assert(bit_offset == NULL);
		/* NOTE: (void) const void *, i32 */
		V parms[2] = { src_base, first_mr_word };
		LLVMBuildCall(ctx->builder, fn, parms, 2, "");
		return NULL;
	}
}


LLVMValueRef encode_packed_struct(
	struct llvm_ctx *ctx,
	LLVMValueRef first_mr_word,
	LLVMValueRef bit_offset,
	IDL_tree ctyp,
	LLVMValueRef src_base)
{
	const struct packed_format *fmt = packed_format_of(ctyp);
	assert(fmt != NULL);
	if(is_short_fmt(fmt)) {
		return encode_packed_struct_inline(ctx, first_mr_word, bit_offset,
			ctyp, src_base);
	} else {
		return encode_packed_struct_fncall(ctx, first_mr_word, bit_offset,
			ctyp, src_base);
	}
}


/* function hash wrangling. */

LLVMValueRef get_struct_fn(
	struct llvm_ctx *ctx,
	IDL_tree ctyp,
	bool for_encode)
{
	const char *s_id = IDL_IDENT(IDL_TYPE_STRUCT(ctyp).ident).repo_id;
	char *lookup_name = g_strdup_printf("%c%s", for_encode ? 'e' : 'd', s_id);
	LLVMValueRef fn = g_hash_table_lookup(ctx->struct_decoder_fns,
		lookup_name);
	if(fn != NULL) {
		g_free(lookup_name);
		return fn;
	}

	const struct packed_format *fmt = packed_format_of(ctyp);
	assert(fmt != NULL);	/* only sane for packable structs */
	int namelen = strlen(s_id);
	char flatname[namelen + 1];
	/* FIXME: make this proper, i.e. use a name mangler that works */
	for(int i=0; i < namelen; i++) {
		flatname[i] = isalnum(s_id[i]) ? s_id[i] : '_';
	}
	flatname[namelen] = '\0';
	T types[3], rettyp = LLVMVoidTypeInContext(ctx->ctx);
	types[0] = LLVMPointerType(llvm_rigid_type(ctx, ctyp), 0);
	int nparms;
	if(!for_encode) {
		/* decoder */
		types[1] = ctx->i32t;
		types[2] = ctx->i32t;
		nparms = fmt->num_bits < BITS_PER_WORD ? 3 : 2;
	} else if(fmt->num_bits < BITS_PER_WORD) {
		/* subword encoder */
		rettyp = ctx->wordt;
		types[1] = ctx->wordt;
		types[2] = ctx->i32t;
		nparms = 3;
	} else {
		/* non-subword encoder */
		types[1] = ctx->i32t;
		nparms = 2;
	}
	T fntype = LLVMFunctionType(rettyp, types, nparms, 0);
	char *fnname = g_strdup_printf("__muidl_idl_%scode__%s",
		for_encode ? "en" : "de", flatname);
	fn = LLVMAddFunction(ctx->module, fnname, fntype);
	LLVMSetLinkage(fn, LLVMExternalLinkage);
	V params[nparms];
	assert(LLVMCountParams(fn) == nparms);
	LLVMGetParams(fn, params);
	LLVMAddAttribute(params[0], LLVMNoAliasAttribute);
	LLVMAddAttribute(params[0], LLVMNoCaptureAttribute);
	for(int i=0; i<nparms; i++) {
		LLVMAddAttribute(params[i], LLVMInRegAttribute);
	}
	g_free(fnname);
	g_hash_table_insert(ctx->struct_decoder_fns, lookup_name, fn);

	return fn;
}
