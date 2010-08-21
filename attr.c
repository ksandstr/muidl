/*
 * attr.c -- functions for working with IDL attributes
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

#include <stdbool.h>
#include <assert.h>
#include <libIDL/IDL.h>

#include "muidl.h"


int op_timeout_kind(IDL_tree opdcl)
{
	assert(IDL_NODE_TYPE(opdcl) == IDLN_OP_DCL);
	int ret = 0;
	IDL_tree propnode = IDL_OP_DCL(opdcl).ident;
	if(IDL_tree_property_get(propnode, "StubTimeouts") != NULL) {
		ret |= TIMEOUT_SEND | TIMEOUT_RECV;
	} else {
		if(IDL_tree_property_get(propnode, "StubSendTimeout") != NULL) {
			ret |= TIMEOUT_SEND;
		}
		if(IDL_tree_property_get(propnode, "StubRecvTimeout") != NULL) {
			ret |= TIMEOUT_RECV;
		}
	}
	return ret;
}


bool has_pager_target(IDL_ns ns, IDL_tree op_dcl)
{
	assert(IDL_NODE_TYPE(op_dcl) == IDLN_OP_DCL);
	IDL_tree prop_node = IDL_OP_DCL(op_dcl).ident;
	const char *pt = IDL_tree_property_get(prop_node, "PagerTarget");
	return pt != NULL;
}


bool is_negs_exn(IDL_tree exn)
{
	/* FIXME: once libIDL supports properties on exceptions, use
	 * those to recognize this sort of thing.
	 */
	const char *rid = IDL_IDENT_REPO_ID(IDL_EXCEPT_DCL(exn).ident);
	return strcmp(rid, "IDL:Posix/Errno:1.0") == 0;
}
