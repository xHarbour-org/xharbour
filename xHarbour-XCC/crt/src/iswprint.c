/****************************************************************************
 *                                                                          *
 * File    : iswprint.c                                                     *
 *                                                                          *
 * Purpose : iswprint function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wctype.h>

/* test for printable wide character */
int __cdecl (iswprint)(wint_t wc)
{
    return __iswctype(wc, 7);
}

