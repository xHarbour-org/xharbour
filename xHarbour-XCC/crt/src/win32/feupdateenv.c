/****************************************************************************
 *                                                                          *
 * File    : feupdateenv.c                                                  *
 *                                                                          *
 * Purpose : feupdateenv function -- win32 version.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-03-17  Changed return type from void to int; see TC1.       *
 *           01-09-03  Rewritten.                                           *
 *                                                                          *
 ****************************************************************************/

#include <fenv.h>

/* merge in stored floating-point environment */
int __cdecl (feupdateenv)(const fenv_t *envp)
{
    int except = fetestexcept(FE_ALL_EXCEPT);

    fesetenv(envp);
    feraiseexcept(except);

    return 0;
}
