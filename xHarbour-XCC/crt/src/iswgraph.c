/****************************************************************************
 *                                                                          *
 * File    : iswgraph.c                                                     *
 *                                                                          *
 * Purpose : iswgraph function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wctype.h>

/* test for graphic wide character */
int __cdecl (iswgraph)(wint_t wc)
{
    return __iswctype(wc, 5);
}

