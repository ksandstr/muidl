/*
 * analyse.c -- verification and analysis of IDL tree inputs
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

#define _GNU_SOURCE

#include <glib.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>
#include <libIDL/IDL.h>

#include "muidl.h"


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


static bool op_label(struct method_info *inf, IDL_tree op)
{
	IDL_tree prop_node = IDL_OP_DCL(op).ident;

	unsigned long tagmask = NO_TAGMASK;
	if(!get_ul_property(&tagmask, prop_node, "TagMask")) return false;
	inf->tagmask = tagmask;

	unsigned long labelval = 0;
	if(!get_ul_property(&labelval, prop_node, "Label")) return false;
	inf->label = labelval;

	return true;
}


/* first stage of op parameter analysis: decode static register assignments,
 * fill in param_info structures and compute the highest fixed register number.
 *
 * returns false on failure.
 */
static bool op_params(
	struct method_info *inf,
	int *num_regs_p,
	int *max_fixreg_p,
	IDL_tree method)
{
	int num_regs = 0, max_fixreg = 0, ppos = 0;
	for(IDL_tree cur = IDL_OP_DCL(method).parameter_dcls;
		cur != NULL;
		cur = IDL_LIST(cur).next)
	{
		IDL_tree parm = IDL_LIST(cur).data,
			type = get_type_spec(IDL_PARAM_DCL(parm).param_type_spec),
			ident = IDL_PARAM_DCL(parm).simple_declarator;
		struct param_info *pinf = &inf->params[ppos++];
		pinf->name = IDL_IDENT(ident).str;
		pinf->type = type;
		pinf->param_dcl = parm;

		if(IDL_PARAM_DCL(parm).attr != IDL_PARAM_IN) inf->num_out_params++;

		if(is_value_type(type)) num_regs++;
		else {
			/* TODO: count and assign MRs for rigid and long types */
			NOTDEFINED(type);
		}

		unsigned long mr_n = 0;
		if(!get_ul_property(&mr_n, ident, "MR")) return false;
		if(mr_n > 0) {
			if(mr_n > 63) {
				fprintf(stderr, "%s: MR(%lu) too large\n", __FUNCTION__,
					mr_n);
				return false;
			}
			pinf->first_reg = mr_n;
			pinf->last_reg = mr_n;
			max_fixreg = MAX(max_fixreg, mr_n);
		} else {
			pinf->first_reg = 0;
			pinf->last_reg = 0;
		}
	}
	inf->num_params = ppos;
	*num_regs_p = num_regs;
	*max_fixreg_p = max_fixreg;
	return true;
}


/* TODO: get the number and max dimensions of the string buffers we'll
 * send/receive once long types are implemented.
 *
 * TODO: enforce zero-or-one NegativeReturn exception restriction
 */
struct method_info *analyse_op_dcl(
	struct print_ctx *pr,
	IDL_tree method)
{
	IDL_tree param_list = IDL_OP_DCL(method).parameter_dcls;
	const int num_params = IDL_list_length(param_list);
	struct method_info *inf = g_malloc(sizeof(struct method_info)
		+ sizeof(struct param_info) * num_params);
	inf->node = method;
	inf->num_out_params = 0;

	if(!op_label(inf, method)) goto fail;
	int num_regs = 0, max_fixreg = 0;
	if(!op_params(inf, &num_regs, &max_fixreg, method)) goto fail;

	/* second pass: verify that fixed registers are referenced at most once,
	 * and assign other registers around them.
	 */
	int taken_len = MAX(num_regs, max_fixreg);
	/* (one more to permit one-origin addressing.) */
	bool *taken = alloca(sizeof(bool) * (taken_len + 1));
	for(int i=1; i <= taken_len; i++) taken[i] = false;
	int next_reg = 1, r_used = 0;
	for(int i=0; i < inf->num_params; i++) {
		struct param_info *pinf = &inf->params[i];
		if(pinf->first_reg > 0) {
			assert(pinf->last_reg > 0);
			assert(pinf->last_reg >= pinf->first_reg);
			for(int r = pinf->first_reg; r <= pinf->last_reg; r++) {
				if(taken[r]) {
					fprintf(stderr, "%s: MR%d fixed multiple times\n",
						__FUNCTION__, r);
					goto fail;
				}
				taken[r] = true;
				r_used++;
			}
		} else if(is_value_type(pinf->type)) {
			/* single-word types. */
			int r;
			do {
				r = next_reg++;
				assert(r <= taken_len);
			} while(taken[r]);
			taken[r] = true;
			r_used++;
			pinf->first_reg = r;
			pinf->last_reg = r;
		} else {
			NOTDEFINED(pinf->type);
		}
	}

	return inf;

fail:
	g_free(inf);
	return NULL;
}
