/****************************************************************************
 *                                                                          *
 * File    : wscanf.c                                                       *
 *                                                                          *
 * Purpose : wscanf function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xwstdio.h"

/* get or put a wide character */
static wint_t scin(void *str, wint_t ch, int getfl)
{
    return (getfl) ? fgetwc((FILE *)str) : ungetwc(ch, (FILE *)str);
}

/* read formatted from wide stdin */
int __cdecl (wscanf)(const wchar_t * restrict fmt, ...)
{
    int ans;
    va_list ap;

    va_start(ap, fmt);
    ans = __wscanf(&scin, stdin, fmt, ap);
    va_end(ap);

    return ans;
}

