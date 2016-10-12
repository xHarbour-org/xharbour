/****************************************************************************
 *                                                                          *
 * File    : wcsftime.c                                                     *
 *                                                                          *
 * Purpose : wcsftime function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xtime.h"
#include "xwchar.h"

/* format wide time information */
size_t __cdecl (wcsftime)(wchar_t * restrict buf, size_t bufsize, const wchar_t * restrict fmt, const struct tm * restrict t)
{
    const wchar_t *ibuf = buf;
    int ch;

    while (bufsize > 0 && fmt[0] != L'\0')
    {
        if (fmt[0] != L'%' || (ch = wctob(fmt[1])) <= 0)
        {
            *buf++ = *fmt++, --bufsize;
        }
        else
        {
            /* process a conversion specifier */
            char nfmt[3];
            int n = 2;

            nfmt[0] = '%', nfmt[1] = ch, fmt += 2;

            if ((ch == 'E' || ch == 'O') && fmt[0] != L'\0')
                nfmt[n++] = wctob(*fmt++);

            if ((n = __wcsftime(buf, bufsize, nfmt, n, t)) < 0)
                return 0;

            buf += n, bufsize -= n;
        }
    }

    if (bufsize == 0)
        return 0;

    *buf = L'\0';
    return buf - (wchar_t *)ibuf;
}

