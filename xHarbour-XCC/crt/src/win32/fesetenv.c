/****************************************************************************
 *                                                                          *
 * File    : fesetenv.c                                                     *
 *                                                                          *
 * Purpose : fesetenv function -- win32 version.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-03-17  Changed return type from void to int; see TC1.       *
 *           01-09-03  Rewritten.                                           *
 *                                                                          *
 ****************************************************************************/

#include <fenv.h>

/* load floating-point environment */
int __cdecl (fesetenv)(const fenv_t *envp)
{
    __asm {
        mov eax,[envp]
        fldenv [eax]
    }

    return 0;  /* success; new in TC1 */
}
