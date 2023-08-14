/****************************************************************************
 *                                                                          *
 * File    : _wcsftime.c                                                    *
 *                                                                          *
 * Purpose : __wcsftime function.                                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-10  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>
#include <string.h>
#include "xtime.h"
#include "xwchar.h"

#define NSTACK  3  /* depth of format nesting stack */

/* format and widen time information */
size_t __wcsftime(wchar_t *buf, size_t bufsize, const char *fmt, size_t len, const struct tm *t)
{
    const char *fmtsav[NSTACK];
    size_t lensav[NSTACK];
    size_t nstack = 0;
    wchar_t *ibuf = buf;
    mbstate_t mbst = {0};

    while (len > 0 || nstack > 0)
    {
        /* parse format string */
        int n;
        wchar_t wc = L'\0';

        if (len == 0)
            fmt = fmtsav[--nstack], len = lensav[nstack];

        if ((n = __mbtowc(&wc, fmt, len, &mbst)) <= 0)
            n = (*fmt == '\0') ? 0 : 1;  /* bad parse, eat one char */

        fmt += n, len -= n;

        if (wc == L'\0')
            ;  /* discard any trailer */
        else if (bufsize == 0)
            return 0;  /* not enough room */
        else if (wc != L'%' || len == 0)
            *buf++ = wc, --bufsize;
        else
        {
            /* do the conversion */
            char ac[20];
            char qual = (*fmt == 'E' || *fmt == 'O') ? *fmt++ : '\0';
            int m;
            const char *p;

            p = __gentime(t, &__times, qual, *fmt, &m, ac);

            if (qual != '\0')
                --len;

            ++fmt, --len;

            if (m > 0)
            {
                /* parse conversion string */
                mbstate_t mbst2 = {0};

                for (; 0 < m; p += n, m -= n)
                {
                    if ((n = __mbtowc(&wc, p, m, &mbst2)) <= 0)
                        break;
                    else if (bufsize == 0)
                        return 0;
                    else
                        *buf++ = wc, --bufsize;
                }
            }
            else if (len == 0 || nstack >= NSTACK)
            {
                fmt = p, len = -m;  /* tail recursion or stack full */
            }
            else
            {
                /* add leftover format to stack */
                fmtsav[nstack] = fmt, fmt = p;
                lensav[nstack++] = len, len = -m;
            }
        }
    }

    return buf - ibuf;
}

