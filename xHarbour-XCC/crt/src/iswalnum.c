/****************************************************************************
 *                                                                          *
 * File    : iswalnum.c                                                     *
 *                                                                          *
 * Purpose : iswalnum function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wctype.h>

/* test for alphanumeric wide character */
int __cdecl (iswalnum)(wint_t wc)
{
    return __iswctype(wc, 1);
}

