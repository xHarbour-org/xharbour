/****************************************************************************
 *                                                                          *
 * File    : fegetexceptflag.c                                              *
 *                                                                          *
 * Purpose : fegetexceptflag function -- win32 version.                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-03-17  Changed return type from void to int; see TC1.       *
 *           01-09-03  Rewritten.                                           *
 *                                                                          *
 ****************************************************************************/

#include <fenv.h>

/* store selected exception sticky bits */
int __cdecl (fegetexceptflag)(fexcept_t *flagp, int except)
{
    if ((except &= FE_ALL_EXCEPT) != 0)
    {
        /* try to get one or more exception sticky bits */
        __asm {
            mov eax,[flagp]
            fstsw word [eax]
        }
    }

    *flagp &= except;
    return 0;  /* success; new in TC1 */
}
