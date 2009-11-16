/*
 * util.c -- utilities for µidl
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

#include "muidl.h"


void list_dispose(GList *list)
{
	g_list_foreach(list, (GFunc)&g_free, NULL);
	g_list_free(list);
}


void free_message_info(struct message_info *inf)
{
	for(int i=0; i<inf->num_untyped; i++) {
		g_free(inf->untyped[i]);
	}
	g_free(inf->untyped);

	for(int i=0; i<inf->num_inline_seq; i++) g_free(inf->seq[i]);
	g_free(inf->seq);

	g_free(inf);
}

