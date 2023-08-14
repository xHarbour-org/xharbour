/****************************************************************************
 *                                                                          *
 * File    : wprintf.c                                                      *
 *                                                                          *
 * Purpose : wprintf function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xwstdio.h"

/* write to wide file */
static void *prout(void *arg, const wchar_t *buf, size_t n)
{
    FILE *str = (FILE *)arg;

    for (; n > 0; --n, ++buf)
    {
        if (fputwc(*buf, str) == WEOF)
            return 0;
    }
    return str;
}

/* print formatted to wide stdout */
int __cdecl (wprintf)(const wchar_t * restrict fmt, ...)
{
    int ans;
    va_list ap;

    va_start(ap, fmt);
    ans = __wprintf(&prout, stdout, fmt, ap);
    va_end(ap);

    return ans;
}

