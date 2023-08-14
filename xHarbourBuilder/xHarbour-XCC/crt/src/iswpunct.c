/****************************************************************************
 *                                                                          *
 * File    : iswpunct.c                                                     *
 *                                                                          *
 * Purpose : iswpunct function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wctype.h>

/* test for punctutation wide character */
int __cdecl (iswpunct)(wint_t wc)
{
    return __iswctype(wc, 8);
}

