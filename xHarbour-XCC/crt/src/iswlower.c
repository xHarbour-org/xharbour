/****************************************************************************
 *                                                                          *
 * File    : iswlower.c                                                     *
 *                                                                          *
 * Purpose : iswlower function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wctype.h>

/* test for lower case wide character */
int __cdecl (iswlower)(wint_t wc)
{
    return __iswctype(wc, 6);
}

