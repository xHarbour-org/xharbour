/****************************************************************************
 *                                                                          *
 * File    : wcsrtomb.c                                                     *
 *                                                                          *
 * Purpose : wcsrtomb function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <limits.h>
#include <string.h>
#include <stdlib.h>
#include "xwchar.h"

/* translate wide char string to multibyte string */
size_t __cdecl (wcsrtombs)(char * restrict s, const wchar_t ** restrict pwcs, size_t n, mbstate_t * restrict pst)
{
    static mbstate_t mbst = {0};
    char buf[MB_LEN_MAX];
    int i;
    size_t nc = 0;
    const wchar_t *wcs = *pwcs;

    if (pst == 0)
        pst = &mbst;

    if (s == 0)
    {
        for (;; nc += i, ++wcs)
        {
            /* translate but don't store */
            if ((i = __wctomb(buf, *wcs, pst)) <= 0)
                return (size_t)-1;
            else if (i > 0 && buf[i - 1] == '\0')
                return nc + i - 1;
        }
    }

    for (; n > 0; nc += i, ++wcs, s += i, n -= i)
    {
        /* translate and store */
        char *t;
        mbstate_t mbstsave;

        if (n < MB_CUR_MAX)
            t = buf, mbstsave = *pst;
        else
            t = s;

        if ((i = __wctomb(t, *wcs, pst)) <= 0)
        {
            /* encountered invalid sequence */
            nc = (size_t)-1;
            break;
        }

        if (s == t)
            ;
        else if (n < i)
        {
            /* won't all fit */
            *pst = mbstsave;
            break;
        }
        else
        {
            memcpy(s, buf, i);
        }

        if (s[i - 1] == '\0')
        {
            /* encountered terminating null */
            *pwcs = 0;
            return nc + i - 1;
        }
    }

    *pwcs = wcs;
    return nc;
}

