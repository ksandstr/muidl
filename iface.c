/*
 * iface.c -- support functions for libIDL interface trees
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

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <assert.h>
#include <glib.h>
#include <libIDL/IDL.h>
#include <ccan/str/str.h>
#include <ccan/strset/strset.h>
#include <ccan/strmap/strmap.h>

#include "defs.h"


static bool collect_methods(
	GList **methods_p,
	struct strset *ifaces_seen,
	IDL_ns ns,
	IDL_tree iface)
{
	/* depth-first recursion so that operation overlap errors etc. are caught
	 * as close to the inheritance graph's root (i.e. the interfaces we're
	 * going to use) as possible.
	 *
	 * to contrast: interface inheritance is idempotent, so multiple
	 * overlapping inheritance of interfaces is A-OK.
	 */
	IDL_LIST_FOREACH(cur, IDL_INTERFACE(iface).inheritance_spec) {
		IDL_tree inh_id = IDL_LIST(cur).data;
		char *inh_repoid = IDL_IDENT_REPO_ID(inh_id);
		if(strset_get(ifaces_seen, inh_repoid) != NULL) continue;

		IDL_tree inh_iface_id = IDL_ns_resolve_this_scope_ident(ns,
			IDL_IDENT_TO_NS(inh_id), inh_id);
		if(inh_iface_id == NULL) {
			fprintf(stderr, "can't find inherited interface `%s'\n",
				inh_repoid);
			return false;
		}

		assert(IDL_NODE_TYPE(inh_iface_id) == IDLN_GENTREE);
		inh_iface_id = IDL_GENTREE(inh_iface_id).data;
		assert(IDL_NODE_TYPE(inh_iface_id) == IDLN_IDENT);
#if 0
		printf("%s: recursing to `%s'\n", __FUNCTION__,
			IDL_IDENT_REPO_ID(inh_iface_id));
#endif
		IDL_tree inh_iface = IDL_NODE_UP(inh_iface_id);
		assert(IDL_NODE_TYPE(inh_iface) == IDLN_INTERFACE);

		bool ok = strset_add(ifaces_seen, inh_repoid);
		assert(ok || errno != EEXIST);
		if(!collect_methods(methods_p, ifaces_seen, ns, inh_iface)) {
			return false;
		}
	}

	/* then the actual methods. */
	IDL_LIST_FOREACH(cur, IDL_INTERFACE(iface).body) {
		IDL_tree op = IDL_LIST(cur).data;
		if(IDL_NODE_TYPE(op) != IDLN_OP_DCL) continue;
		*methods_p = g_list_prepend(*methods_p, op);
	}

	return true;
}


GList *all_methods_of_iface(IDL_ns ns, IDL_tree iface)
{
	GList *methods = NULL;
	struct strset ifaces_seen;
	strset_init(&ifaces_seen);
	bool ok = strset_add(&ifaces_seen, IFACE_REPO_ID(iface));
	assert(ok || errno != EEXIST);
	collect_methods(&methods, &ifaces_seen, ns, iface);
	strset_clear(&ifaces_seen);
	return g_list_reverse(methods);
}


static int exns_by_repoid_cmp(gconstpointer a, gconstpointer b) {
	return strcmp(EXN_REPO_ID((IDL_tree)a), EXN_REPO_ID((IDL_tree)b));
}


static bool build_value_glist(const char *key, IDL_tree value, GList **list) {
	*list = g_list_prepend(*list, value);
	return true;
}


GList *iface_exns_sorted(IDL_ns ns, IDL_tree iface)
{
	STRMAP(IDL_tree) ex_repo_ids;
	strmap_init(&ex_repo_ids);

	GList *methods = all_methods_of_iface(ns, iface);
	GLIST_FOREACH(cur, methods) {
		IDL_tree op = cur->data;
		IDL_LIST_FOREACH(r_cur, IDL_OP_DCL(op).raises_expr) {
			IDL_tree exn_id = IDL_LIST(r_cur).data;
			const char *rid = IDL_IDENT(exn_id).repo_id;
			if(strmap_get(&ex_repo_ids, rid) != NULL) continue;

			/* exn_id actually refers to the IDL_EXCEPT_DCL's ident node.
			 * the actual exception is an immediate parent.
			 */
			IDL_tree exn = IDL_get_parent_node(exn_id, IDLN_EXCEPT_DCL,
				NULL);
			assert(exn != NULL);
			assert(streq(rid, IDL_IDENT(IDL_EXCEPT_DCL(exn).ident).repo_id));
			bool ok = strmap_add(&ex_repo_ids, rid, exn);
			assert(ok || errno != EEXIST);
		}
	}
	g_list_free(methods);

	GList *ret = NULL;
	strmap_iterate(&ex_repo_ids, &build_value_glist, &ret);
	strmap_clear(&ex_repo_ids);

	return g_list_sort(ret, &exns_by_repoid_cmp);
}
