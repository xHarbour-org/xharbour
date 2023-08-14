/****************************************************************************
 *                                                                          *
 * File    : vfwscanf.c                                                     *
 *                                                                          *
 * Purpose : vfwscanf function [new C99].                                   *
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

/* read formatted from wide stream from arg list */
int __cdecl (vfwscanf)(FILE * restrict str, const wchar_t * restrict fmt, va_list ap)
{
    int ans;

    _Lockfileatomic(str);
    ans = __wscanf(&scin, str, fmt, ap);
    _Unlockfileatomic(str);

    return ans;
}

