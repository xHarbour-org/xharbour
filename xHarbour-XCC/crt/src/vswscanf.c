/****************************************************************************
 *                                                                          *
 * File    : vswscanf.c                                                     *
 *                                                                          *
 * Purpose : vswscanf function [new C99].                                   *
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

/* read formatted from wide string to arg list */
int __cdecl (vswscanf)(const wchar_t * restrict buf, const wchar_t * restrict fmt, va_list ap)
{
    return __wscanf(&scin, (void **)&buf, fmt, ap);
}

