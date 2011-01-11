/****************************************************************************
 *                                                                          *
 * File    : wcstombs.c                                                     *
 *                                                                          *
 * Purpose : wcstombs function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-02  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <limits.h>
#include <string.h>
#include <stdlib.h>
#include "xwchar.h"

/* translate wide char string to multibyte string */
size_t __cdecl (wcstombs)(char * restrict s, const wchar_t * restrict wcs, size_t n)
{
    mbstate_t mbst = {0};
    size_t nc;
    int i;

    for (nc = 0; nc < n; ++wcs)
    {
        /* translate another wide character */
        if (n - nc >= MB_CUR_MAX && s != 0)
        {
            /* copy directly */
            if ((i = __wctomb(s + nc, *wcs, &mbst)) < 0)
                return (size_t)-1;

            nc += i;

            if (i > 0 && s[nc - 1] == '\0')
                return nc - 1;
        }
        else
        {
            /* copy into local buffer */
            char buf[MB_LEN_MAX];

            if ((i = __wctomb(buf, *wcs, &mbst)) < 0)
                return (size_t)-1;
            else if (i <= n - nc)
            {
                /* will all fit, copy and continue */
                if (s != 0)
                    memcpy(s + nc, buf, i);

                nc += i;

                if (0 < i && buf[i - 1] == '\0')
                    return nc - 1;
            }
            else
            {
                /* won't all fit, copy partial and quit */
                if (s != 0)
                    memcpy(s + nc, buf, n - nc);
                return n;
            }
        }
    }

    return nc;
}

