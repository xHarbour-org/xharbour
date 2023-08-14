/****************************************************************************
 *                                                                          *
 * File    : _towctrans.c                                                   *
 *                                                                          *
 * Purpose : __towctrans function.                                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-10  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <wctype.h>
#include "xwctype.h"

/* translate wide character */
wint_t __cdecl __towctrans(wint_t wc, wctrans_t off)
{
    const wchar_t *p = (const wchar_t *)__wctranstab[0].name + __wctranstab[off].off;
    const wchar_t *pe = (const wchar_t *)__wctranstab[0].name + __wctranstab[off + 1].off;

    for (; p < pe; p += 3)
    {
        if (p[0] <= wc && wc <= p[1])
            return wc + (wint_t)p[2];
    }

    return wc;
}

