

#include <stdlib.h>
#include <l4/types.h>

#include "second-defs.h"
#include "first-defs.h"


int main(void)
{
	__first_op01(L4_nilthread, NULL, 0);
	__second_op11(L4_nilthread, NULL, 0);
	__second_op12(L4_nilthread, NULL, 0);
	return 0;
}
