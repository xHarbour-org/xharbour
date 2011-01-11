/****************************************************************************
 *                                                                          *
 * File    : towupper.c                                                     *
 *                                                                          *
 * Purpose : towupper function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wctype.h>

/* convert to upper case wide character */
wint_t __cdecl (towupper)(wint_t wc)
{
    return __towctrans(wc, 2);
}

