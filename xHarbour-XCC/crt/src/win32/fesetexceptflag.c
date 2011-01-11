/****************************************************************************
 *                                                                          *
 * File    : fesetexceptflag.c                                              *
 *                                                                          *
 * Purpose : fesetexceptflag function -- win32 version.                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-03-17  Changed return type from void to int; see TC1.       *
 *           01-09-03  Rewritten.                                           *
 *                                                                          *
 ****************************************************************************/

#include <fenv.h>

/* load selected exception sticky bits */
int __cdecl (fesetexceptflag)(const fexcept_t *flagp, int except)
{
    if ((except &= FE_ALL_EXCEPT) != 0)
    {
        /* try to get one or more exception sticky bits */
        fenv_t env;

        fegetenv(&env);
        env.status |= except;
        fesetenv(&env);
    }

    return 0;  /* success; new in TC1 */
}
