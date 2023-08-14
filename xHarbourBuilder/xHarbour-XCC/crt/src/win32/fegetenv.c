/****************************************************************************
 *                                                                          *
 * File    : fegetenv.c                                                     *
 *                                                                          *
 * Purpose : fegetenv function -- win32 version.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           00-12-12  Bugfix: first working version.                       *
 *           01-03-17  Changed return type from void to int; see TC1.       *
 *           01-09-03  Rewritten.                                           *
 *                                                                          *
 ****************************************************************************/

#include <fenv.h>

/* store floating-point environment */
int __cdecl (fegetenv)(fenv_t *envp)
{
    __asm {
        mov eax,[envp]
        fstenv [eax]
        fldcw word [eax]
    }

    return 0;  /* success; new in TC1 */
}
