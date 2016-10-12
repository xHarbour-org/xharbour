/****************************************************************************
 *                                                                          *
 * File    : feraiseexcept.c                                                *
 *                                                                          *
 * Purpose : feraiseexcept function -- win32 version.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-03-17  Changed return type from void to int; see TC1.       *
 *           01-09-03  Rewritten.                                           *
 *                                                                          *
 ****************************************************************************/

#include <fenv.h>

const fenv_t __dfl_fenv = {0x027f};  /* 53-bit precision */
fenv_t __fenv = {0};  /* the software FPP registers */

int __cdecl (feraiseexcept)(int except)
{
    /* report floating-point exception */
    if ((except &= FE_ALL_EXCEPT) != 0)
    {
        /* try to get one or more exception sticky bits */
        fenv_t env;

        fegetenv(&env);
        env.status |= except;
        fesetenv(&env);
    }

    return 0;
}

