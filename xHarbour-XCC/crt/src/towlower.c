/****************************************************************************
 *                                                                          *
 * File    : towlower.c                                                     *
 *                                                                          *
 * Purpose : towlower function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wctype.h>

/* convert to lower case wide character */
wint_t __cdecl (towlower)(wint_t wc)
{
    return __towctrans(wc, 1);
}

