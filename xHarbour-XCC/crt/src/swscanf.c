/****************************************************************************
 *                                                                          *
 * File    : swscanf.c                                                      *
 *                                                                          *
 * Purpose : swscanf function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xwstdio.h"

/* get or put a wide character */
static wint_t scin(void *str, wint_t ch, int getfl)
{
    wchar_t *s = *(wchar_t **)str;

    if (!getfl)
    {
        /* back up a wchar_t */
        *(wchar_t **)str = s - 1;
        return ch;
    }
    else if (*s == L'\0')
    {
        return WEOF;
    }
    else
    {
        /* deliver a wchar_t */
        *(wchar_t **)str = s + 1;
        return *s;
    }
}

/* read formatted from wide string */
int __cdecl (swscanf)(const wchar_t * restrict buf, const wchar_t * restrict fmt, ...)
{
    int ans;
    va_list ap;

    va_start(ap, fmt);
    ans = __wscanf(&scin, (void **)&buf, fmt, ap);
    va_end(ap);

    return ans;
}

