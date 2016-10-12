/****************************************************************************
 *                                                                          *
 * File    : iswcntrl.c                                                     *
 *                                                                          *
 * Purpose : iswcntrl function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wctype.h>

/* test for control wide character */
int __cdecl (iswcntrl)(wint_t wc)
{
    return __iswctype(wc, 3);
}

