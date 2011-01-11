/****************************************************************************
 *                                                                          *
 * File    : _strftime.c                                                    *
 *                                                                          *
 * Purpose : __strftime function.                                           *
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

/* format time information */
size_t __strftime(char *buf, size_t bufsize, const char *fmt, const struct tm *t, const __timeinfo *tin)
{
    const char *fmtsav[NSTACK];
    size_t lensav[NSTACK];
    size_t nstack = 0;
    size_t len = strlen(fmt);
    const char *ibuf = buf;
    mbstate_t mbst = {0};

    while (len > 0 || nstack > 0)
    {
        /* parse format string */
        int n;
        wchar_t wc;

        if (len == 0)
            fmt = fmtsav[--nstack], len = lensav[nstack];

        if ((n = __mbtowc(&wc, fmt, len, &mbst)) <= 0)
            n = *fmt == '\0' ? 0 : 1;  /* bad parse, eat one char */

        fmt += n, len -= n;

        if (wc != L'%' || len == 0)
        {
            /* deliver literal format chars */
            if (bufsize < n)
                return 0;

            memcpy(buf, fmt - n, n);
            buf += n, bufsize -= n;
        }
        else
        {
            /* do the conversion */
            char ac[20];
            char qual = (*fmt == 'E' || *fmt == 'O') ? *fmt++ : '\0';
            int m;
            const char *p = __gentime(t, tin, qual, *fmt, &m, ac);

            if (qual != '\0')
                --len;

            ++fmt, --len;

            if (m > 0)
            {
                /* deliver converted chars */
                if (bufsize < m)
                    return 0;

                memcpy(buf, p, m);
                buf += m, bufsize -= m;
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

    if (bufsize == 0)
        return 0;

    *buf = '\0';
    return buf - (char *)ibuf;
}

