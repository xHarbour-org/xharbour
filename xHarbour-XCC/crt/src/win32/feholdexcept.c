/****************************************************************************
 *                                                                          *
 * File    : feholdexcept.c                                                 *
 *                                                                          *
 * Purpose : feholdexcept function -- win32 version.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           00-12-12  Bugfix: first working version.                       *
 *           01-09-03  Rewritten.                                           *
 *                                                                          *
 ****************************************************************************/

#include <fenv.h>

/* store and clear floating-point environment */
int __cdecl (feholdexcept)(fenv_t *envp)
{
    __asm {
        mov eax,[envp]
        fstenv [eax]
        fclex
    }

    return 0;
}

