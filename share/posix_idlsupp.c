
/* support bits for services that muidl generates, using POSIX interfaces.
 *
 * this file is licensed under the GNU Lesser General Public License
 * version 3, or at your option any later version of the GNU Lesser
 * General Public License published by the Free Software Foundation.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <threads.h>


static tss_t key;
static once_flag init_once = ONCE_FLAG_INIT;


static void create_key(void) {
	int n = tss_create(&key, &free);
	if(n != thrd_success) abort();
}


void *muidl_supp_get_context(void) {
	call_once(&init_once, &create_key);
	return tss_get(key);
}


void muidl_supp_alloc_context(unsigned int length)
{
	call_once(&init_once, &create_key);

	void *ctx = tss_get(key);
	if(ctx != NULL) free(ctx);

	if(length < 64) length = 64;
	ctx = malloc(length);
	if(ctx == NULL) abort();
	memset(ctx, '\0', length);
	tss_set(key, ctx);
}
