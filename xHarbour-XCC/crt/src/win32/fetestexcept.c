/****************************************************************************
 *                                                                          *
 * File    : fetestexcept.c                                                 *
 *                                                                          *
 * Purpose : fetestexcept function -- win32 version.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-03  Rewritten.                                           *
 *                                                                          *
 ****************************************************************************/

#include <fenv.h>

/* test exception sticky bits */
int __cdecl (fetestexcept)(int except)
{
    fexcept_t status;

    fegetexceptflag(&status, except);
    return status;
}
