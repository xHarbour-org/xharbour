/****************************************************************************
 *                                                                          *
 * File    : mbstowcs.c                                                     *
 *                                                                          *
 * Purpose : mbstowcs function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-03  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <limits.h>
#include <stdlib.h>
#include "xwchar.h"

/* translate multibyte string to wide char string */
size_t __cdecl (mbstowcs)(wchar_t * restrict wcs, const char * restrict s, size_t n)
{
    mbstate_t mbst = {0};
    size_t nw;
    wchar_t wc;

    for (nw = 0; nw < n; ++nw)
    {
        /* make another wide character */
        int i = __mbtowc(&wc, s, INT_MAX, &mbst);

        if (i < 0)
            return (size_t)-1;

        if (wcs != 0)
            wcs[nw] = wc;

        if (wc == L'\0')
            break;

        s += i;
    }

    return nw;
}

