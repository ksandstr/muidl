
#include <stddef.h>
#include <l4/types.h>

#include "struct-passing-defs.h"


int main(void)
{
	__t_op01(L4_nilthread, NULL, 1);
	__t_op02(L4_nilthread, NULL, 2);
	__t_op03(L4_nilthread, NULL, 3);
	__t_op11(L4_nilthread, NULL, 4);
	__t_op12(L4_nilthread, NULL, 5);
	__t_op13(L4_nilthread, NULL, 6);
	__t_op21(L4_nilthread, NULL, 4);
	__t_op22(L4_nilthread, NULL, 5);
	__t_op23(L4_nilthread, NULL, 6);
	return 0;
}
