/****************************************************************************
 *                                                                          *
 * File    : iswdigit.c                                                     *
 *                                                                          *
 * Purpose : iswdigit function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wctype.h>

/* test for digit wide character */
int __cdecl (iswdigit)(wint_t wc)
{
    return __iswctype(wc, 4);
}

