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
#include <string.h>
#include <assert.h>
#include <libIDL/IDL.h>

#include "muidl.h"


/* a single item, unpacked from the IDL format. */
struct comp_item
{
	IDL_tree type;
	const char *name;
	int bits_each, dim;
};


static GHashTable *packed_cache = NULL;


static GPtrArray *unpack_idl_fields(IDL_tree stype)
{
	assert(IDL_NODE_TYPE(stype) == IDLN_TYPE_STRUCT);

	GPtrArray *items = g_ptr_array_new();
	for(IDL_tree m_cur = IDL_TYPE_STRUCT(stype).member_list;
		m_cur != NULL;
		m_cur = IDL_LIST(m_cur).next)
	{
		IDL_tree member = IDL_LIST(m_cur).data,
			mtype = get_type_spec(IDL_MEMBER(member).type_spec);
		for(IDL_tree d_cur = IDL_MEMBER(member).dcls;
			d_cur != NULL;
			d_cur = IDL_LIST(d_cur).next)
		{
			IDL_tree dcl = IDL_LIST(d_cur).data;
			struct comp_item *ci = g_new(struct comp_item, 1);
			ci->type = mtype;
			ci->bits_each = size_in_bits(mtype);
			if(IDL_NODE_TYPE(dcl) == IDLN_IDENT) {
				ci->dim = 1;
				ci->name = IDL_IDENT(dcl).str;
			} else if(IDL_NODE_TYPE(dcl) == IDLN_TYPE_ARRAY) {
				ci->dim = IDL_INTEGER(IDL_LIST(
					IDL_TYPE_ARRAY(dcl).size_list).data).value;
				ci->name = IDL_IDENT(IDL_TYPE_ARRAY(dcl).ident).str;
			} else {
				NOTDEFINED(dcl);
			}
			g_ptr_array_add(items, ci);
		}
	}
	return items;
}


static struct packed_item *new_packed_item(
	int word,
	int bit,
	const char *name)
{
	int namelen = strlen(name);
	struct packed_item *ret = g_malloc(sizeof(struct packed_item)
		+ sizeof(char) * (namelen + 1));
	ret->word = word;
	ret->bit = bit;
	memcpy(ret->name, name, namelen + 1);
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
		GList *link = g_list_first(items_per_size[sz]);
		struct comp_item *ci = link->data;
		const int nbits = ci->bits_each * ci->dim;
		items_per_size[sz] = g_list_delete_link(items_per_size[sz], link);

		/* try different destination words. */
		for(int wn=0; wn < num_words; wn++) {
			if(my_occ[wn] + nbits > BITS_PER_WORD) continue;
			struct packed_item *pi = new_packed_item(wn, my_occ[wn],
				ci->name);
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
			struct packed_item *pi = new_packed_item(num_words, 0, ci->name);
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
	const struct comp_item *const *ca = a, *const *cb = b;
	int asize = (*ca)->bits_each * (*ca)->dim,
		bsize = (*cb)->bits_each * (*cb)->dim;
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

	GPtrArray *items = unpack_idl_fields(stype);
	g_ptr_array_sort(items, &item_by_bitsize_cmp);

	/* packing of small (sub-word) items */
	GList *items_by_size[BITS_PER_WORD - 1];
	for(int i=0; i < (BITS_PER_WORD - 1); i++) {
		items_by_size[i] = NULL;
	}
	int num_small = 0;
	for(int i=0; i<items->len; i++) {
		struct comp_item *item = items->pdata[i];
		int bits = item->bits_each * item->dim;
		if(bits >= BITS_PER_WORD) break;
		items_by_size[bits] = g_list_prepend(items_by_size[bits], item);
		num_small++;
	}
	for(int i=0; i < (BITS_PER_WORD - 1); i++) {
		items_by_size[i] = g_list_reverse(items_by_size[i]);
	}
	GPtrArray *packed = g_ptr_array_new();
	int num_words = pack_items(packed, items_by_size, num_small, NULL, 0, 64);
	if(num_words >= 64) {
		fprintf(stderr, "structure `%s' can't be bit-packed\n", s_id);
		/* FIXME: return NULL, make callers handle NULL */
		abort();
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
	for(int i=0; i<items->len; i++) {
		struct comp_item *item = items->pdata[i];
		int bits = item->bits_each * item->dim;
		if(bits < BITS_PER_WORD) continue;
		g_ptr_array_add(packed,
			new_packed_item(num_words, 0, item->name));
		int words = (bits + BITS_PER_WORD - 1) / BITS_PER_WORD;
#if 0
		printf("%s: packing item `%s' of %d words (%d bits) as-is\n",
			__func__, item->name, words, bits);
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

	assert(packed->len == items->len);
	g_ptr_array_foreach(items, (GFunc)&g_free, NULL);
	g_ptr_array_free(items, TRUE);
	items = NULL;

	ret = g_malloc(sizeof(struct packed_format)
		+ sizeof(struct packed_item *) * packed->len);
	ret->num_words = num_words;
	ret->num_items = packed->len;
	memcpy(ret->items, &g_ptr_array_index(packed, 0),
		packed->len * sizeof(void *));
	g_ptr_array_free(packed, TRUE);
	g_hash_table_insert(packed_cache, g_strdup(s_id), ret);

	return ret;
}
