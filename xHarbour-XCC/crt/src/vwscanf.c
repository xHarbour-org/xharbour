/****************************************************************************
 *                                                                          *
 * File    : vwscanf.c                                                      *
 *                                                                          *
 * Purpose : vwscanf function [new C99].                                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-02  Added _Lockfileatomic() and _Unlockfileatomic().     *
 *                                                                          *
 ****************************************************************************/

#include "xwstdio.h"

/* get or put a wide character */
static wint_t scin(void *str, wint_t ch, int getfl)
{
    return (getfl) ? fgetwc((FILE *)str) : ungetwc(ch, (FILE *)str);
}

/* read formatted from wide stdin to arg list */
int __cdecl (vwscanf)(const wchar_t * restrict fmt, va_list ap)
{
    int ans;

    _Lockfileatomic(stdin);
    ans = __wscanf(&scin, stdin, fmt, ap);
    _Unlockfileatomic(stdin);

    return ans;
}

