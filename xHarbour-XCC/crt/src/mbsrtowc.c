/****************************************************************************
 *                                                                          *
 * File    : mbsrtowc.c                                                     *
 *                                                                          *
 * Purpose : mbsrtowc function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <limits.h>
#include "xwchar.h"

/* translate multibyte string to wide, restartably */
size_t __cdecl (mbsrtowcs)(wchar_t * restrict wcs, const char ** restrict ps, size_t n, mbstate_t * restrict pst)
{
    static mbstate_t mbst = {0};
    const char *s = *ps;
    size_t nwc = 0;
    int i;

    if (pst == 0)
        pst = &mbst;

    if (wcs == 0)
    {
        for (;; ++nwc, s += i)
        {
            /* translate but don't store */
            wchar_t wc;

            if ((i = __mbtowc(&wc, s, INT_MAX, pst)) < 0)
                return (size_t)-1;
            else if (i == 0 && wc == L'\0')
                return nwc;
        }
    }

    for (; n > 0; ++nwc, s += i, ++wcs, --n)
    {
        /* translate and store */
        if ((i = __mbtowc(wcs, s, INT_MAX, pst)) < 0)
        {
            /* encountered invalid sequence */
            nwc = (size_t)-1;
            break;
        }
        else if (i == 0 && *wcs == L'\0')
        {
            /* encountered terminating null */
            s = 0;
            break;
        }
    }

    *ps = s;
    return nwc;
}

