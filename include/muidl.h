/*
 * muidl.h -- support definitions for µIDL
 * Copyright 2009, 2010, 2013  Kalle A. Sandström <ksandstr@iki.fi>
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

#ifndef SEEN_MUIDL_H
#define SEEN_MUIDL_H

#include <stdint.h>


/* status returned by dispatch function. 0 for "OK".
 *
 * if one of the low 4 bits is nonzero, this is a L4 IPC errorcode from the
 * dispatch loop's receive phase. mostly useful for telling e.g. buffer
 * overruns apart from IPC aborts, such as used for exiting the loop.
 *
 * when the low 4 bits are zero, it's a µIDL status code. these are defined
 * below.
 */
#define MUIDL_IS_L4_ERROR(word) (((uintptr_t)(word) & 0xf) != 0)


/* constants returned by the dispatcher for non-L4 error conditions. */
#define MUIDL_UNKNOWN_LABEL (1 << 4)


/* the section below is for L4.X2 programs (µIDL consumers) only. */

#ifndef IN_MUIDL_IMPL
#include <l4/types.h>
#include <l4/message.h>

/* per-thread context & accessors. */

struct muidl_context
{
	L4_ThreadId_t last_sender;
	L4_MsgTag_t last_tag;
	L4_MsgTag_t exn_tag;
};


/* no destructors. the pointer should remain valid and constant within the
 * call to the dispatch function. implementations for these two should be
 * provided by the application or its runtime.
 */
extern void muidl_supp_alloc_context(size_t length);
extern void *muidl_supp_get_context(void);


static inline L4_ThreadId_t muidl_get_sender(void) {
	return ((struct muidl_context *)muidl_supp_get_context())->last_sender;
}

static inline L4_MsgTag_t muidl_get_tag() {
	return ((struct muidl_context *)muidl_supp_get_context())->last_tag;
}

#endif	/* !IN_MUIDL_IMPL */

#endif
