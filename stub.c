/*
 * stub.c -- build stubs for an IDL interface
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
#include <libIDL/IDL.h>

#include "muidl.h"


static void build_ipc_stub(struct llvm_ctx *ctx, IDL_tree opdcl)
{
}


gboolean iter_build_stubs(IDL_tree_func_data *tf, void *ud)
{
	struct llvm_ctx *ctx = ud;
	switch(IDL_NODE_TYPE(tf->tree)) {
		case IDLN_LIST:
		case IDLN_MODULE:
		case IDLN_SRCFILE:
			return TRUE;

		default: return FALSE;

		case IDLN_INTERFACE: {
			for(IDL_tree cur = IDL_INTERFACE(tf->tree).body;
				cur != NULL;
				cur = IDL_LIST(cur).next)
			{
				IDL_tree item = IDL_LIST(cur).data;
				if(IDL_NODE_TYPE(item) == IDLN_OP_DCL) {
					build_ipc_stub(ctx, tf->tree);
				}
			}
			return FALSE;
		}
	}
}
