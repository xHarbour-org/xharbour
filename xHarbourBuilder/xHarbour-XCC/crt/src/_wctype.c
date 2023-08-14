/****************************************************************************
 *                                                                          *
 * File    : _wctype.c                                                      *
 *                                                                          *
 * Purpose : iswctype function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-05  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include "xwctype.h"

/* classify wide character */
int __cdecl __iswctype(wint_t wc, wctype_t desc)
{
    const wchar_t *p = (const wchar_t *)__wctypetab[0].name + __wctypetab[desc].off;
    const wchar_t *pe = (const wchar_t *)__wctypetab[0].name + __wctypetab[desc + 1].off;

    for (; p < pe; p += 3)
    {
        if (p[0] <= wc && wc <= p[1])
            return (int)p[2];
    }

    return 0;
}

