/****************************************************************************
 *                                                                          *
 * File    : iswalpha.c                                                     *
 *                                                                          *
 * Purpose : iswalpha function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wctype.h>

/* test for alphabetic wide character */
int __cdecl (iswalpha)(wint_t wc)
{
    return __iswctype(wc, 2);
}

