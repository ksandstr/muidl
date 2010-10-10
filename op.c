/*
 * op.c -- functions dealing with IDL_OP_DCL nodes
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


IDL_tree find_exn(IDL_tree op, bool (*pred)(IDL_tree exn))
{
	IDL_LIST_FOREACH(cur, IDL_OP_DCL(op).raises_expr) {
		IDL_tree e = IDL_get_parent_node(IDL_LIST(cur).data,
			IDLN_EXCEPT_DCL, NULL);
		if((*pred)(e)) return e;
	}
	return NULL;
}


bool has_pager_target(IDL_tree op_dcl)
{
	assert(IDL_NODE_TYPE(op_dcl) == IDLN_OP_DCL);
	IDL_tree prop_node = IDL_OP_DCL(op_dcl).ident;
	const char *pt = IDL_tree_property_get(prop_node, "PagerTarget");
	return pt != NULL;
}


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


bool has_mapped_param(IDL_tree opdcl)
{
	assert(IDL_NODE_TYPE(opdcl) == IDLN_OP_DCL);
	IDL_LIST_FOREACH(cur, IDL_OP_DCL(opdcl).parameter_dcls) {
		IDL_tree p = IDL_LIST(cur).data,
			ptype = get_type_spec(IDL_PARAM_DCL(p).param_type_spec);
		if(IS_MAPPING_TYPE(ptype)) return true;
		IDL_tree ident = IDL_PARAM_DCL(p).simple_declarator;
		if(IDL_NODE_TYPE(ident) != IDLN_IDENT) {
			ident = IDL_TYPE_ARRAY(ident).ident;
		}
		if(IS_MAPGRANT_TYPE(ptype) && has_map_property(ident)) {
			return true;
		}
	}
	return false;
}
