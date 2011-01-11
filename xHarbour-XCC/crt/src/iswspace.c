/****************************************************************************
 *                                                                          *
 * File    : iswspace.c                                                     *
 *                                                                          *
 * Purpose : iswspace function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wctype.h>

/* test for space wide character */
int __cdecl (iswspace)(wint_t wc)
{
    return __iswctype(wc, 9);
}

