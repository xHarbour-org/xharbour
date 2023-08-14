/****************************************************************************
 *                                                                          *
 * File    : feclearexcept.c                                                *
 *                                                                          *
 * Purpose : feclearexcept function -- win32 version.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-03-17  Changed return type from void to int; see TC1.       *
 *           01-09-03  Rewritten.                                           *
 *                                                                          *
 ****************************************************************************/

#include <fenv.h>

/* clear exception sticky bits */
int __cdecl (feclearexcept)(int except)
{
    if ((except &= FE_ALL_EXCEPT) != 0)
    {
        /* try to get one or more exception sticky bits */
        fenv_t env;

        fegetenv(&env);
        env.status &= ~except;
        fesetenv(&env);
    }

    return 0;
}
