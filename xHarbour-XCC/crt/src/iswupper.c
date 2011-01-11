/****************************************************************************
 *                                                                          *
 * File    : iswupper.c                                                     *
 *                                                                          *
 * Purpose : iswupper function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wctype.h>

/* test for upper space wide character */
int __cdecl (iswupper)(wint_t wc)
{
    return __iswctype(wc, 10);
}

