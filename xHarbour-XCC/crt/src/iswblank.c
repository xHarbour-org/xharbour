/****************************************************************************
 *                                                                          *
 * File    : iswblank.c                                                     *
 *                                                                          *
 * Purpose : iswblank function [new C99].                                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wctype.h>

/* test for space or horizontal tab wide character */
int __cdecl (iswblank)(wint_t wc)
{
    return __iswctype(wc, 12);
}

