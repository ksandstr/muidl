/*
 * l4x2.h -- L4.X2 related definitions and exports from l4x2.c
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

#ifndef SEEN_L4X2_H
#define SEEN_L4X2_H


/* offsets for use as second parameter to UTCB_ADDR_VAL(), wrapped in
 * CONST_INT().
 */
#define MR_OFFSET(n) (n)
#define BR_OFFSET(n) (-(n) - 64)
#define TCR_XFER_TIMEOUTS (-8)
#define TCR_ERROR_CODE (-9)
#define TCR_PAGER (-12)


/* TODO: prototypes for things exported by l4x2.c */

#endif
