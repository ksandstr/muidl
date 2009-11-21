/*
 * gen-stubs.c -- generic stub generation for the L4.X2 API
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

#include <glib.h>
#include <stdio.h>
#include <assert.h>
#include <libIDL/IDL.h>

#include "muidl.h"


void print_stubs_file(struct print_ctx *pr)
{
	print_file_heading(pr);

	static const char *hdrfiles[] = {
		"stdint.h", "stdbool.h", "kernel/types.h", "l4/types.h",
	};
	print_headers(pr, hdrfiles, G_N_ELEMENTS(hdrfiles));

	code_f(pr, "/* FIXME: output stub routines */");
}
