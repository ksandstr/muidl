/*
 * attr.c -- functions for working with IDL attributes
 * Copyright 2010, 2011  Kalle A. Sandström <ksandstr@iki.fi>
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

#include "defs.h"


bool is_negs_exn(IDL_tree exn)
{
	assert(IDL_NODE_TYPE(exn) == IDLN_EXCEPT_DCL);
	/* FIXME: once libIDL supports properties on exceptions, use
	 * those to recognize this sort of thing.
	 */
	const char *rid = IDL_IDENT_REPO_ID(IDL_EXCEPT_DCL(exn).ident);
	return strcmp(rid, "IDL:Posix/Errno:1.0") == 0;
}


bool is_noreply_exn(IDL_tree exn)
{
	assert(IDL_NODE_TYPE(exn) == IDLN_EXCEPT_DCL);
	/* FIXME: same as in is_negs_exn() */
	const char *rid = IDL_IDENT_REPO_ID(IDL_EXCEPT_DCL(exn).ident);
	return strcmp(rid, "IDL:muidl/NoReply:1.0") == 0;
}


bool is_complex_exn(IDL_tree exn) {
	return !is_negs_exn(exn) && !is_noreply_exn(exn);
}


bool is_packed(IDL_tree struct_type)
{
	assert(IDL_NODE_TYPE(struct_type) == IDLN_TYPE_STRUCT);
	IDL_tree prop_node = IDL_TYPE_STRUCT(struct_type).ident;
	return IDL_tree_property_get(prop_node, "Packed") != NULL;
}
