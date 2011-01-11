/****************************************************************************
 *                                                                          *
 * File    : iswxdigit.c                                                    *
 *                                                                          *
 * Purpose : iswxdigit function [new C99?].                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wctype.h>

/* test for hexadecimal wide character */
int __cdecl (iswxdigit)(wint_t wc)
{
    return __iswctype(wc, 11);
}

