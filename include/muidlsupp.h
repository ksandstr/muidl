/*
 * include/kernel/muidlsupp.h -- support interface for µIDL
 * Copyright 2009, 2010  Kalle A. Sandström <ksandstr@iki.fi>
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

#ifndef SEEN_KERNEL_MUIDLSUPP_H
#define SEEN_KERNEL_MUIDLSUPP_H

#include <kernel/types.h>


/* if one of the low 3 bits is nonzero, this is a L4 IPC errorcode. otherwise,
 * it's one of muidl's (where we can use wordlen-4 bits for status, and a
 * single flag bit).
 */
#define MUIDL_IS_L4_ERROR(word) (((L4_Word_t)(word) & 0xe) != 0)
#define MUIDL_STATUS_DATA(word) (((L4_Word_t)(word) & ~UINTMAX_C(0xf)) >> 4)
#define MUIDL_STATUS_FLAG(word) ((L4_Word_t)(word) & 1)
#define MUIDL_STATUS(data, flag) ((L4_Word_t)(data) << 4 | ((flag) & 1))


/* constants returned by the dispatcher for non-L4 error conditions. status
 * data gives error code, flag should be ignored for now.
 */
#define MUIDL_UNKNOWN_LABEL MUIDL_STATUS(1, 0)


/* functions that manipulate a per-thread pointer variable. no destructors. */
extern void muidl_supp_alloc_context(unsigned int length);
extern void *muidl_supp_get_context(void);


#endif
