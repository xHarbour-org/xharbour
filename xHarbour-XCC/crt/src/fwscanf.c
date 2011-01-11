/****************************************************************************
 *                                                                          *
 * File    : fwscanf.c                                                      *
 *                                                                          *
 * Purpose : fwscanf function.                                              *
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

/* read formatted from wide stream */
int __cdecl (fwscanf)(FILE * restrict str, const wchar_t * restrict fmt, ...)
{
    int ans;
    va_list ap;

    va_start(ap, fmt);
    _Lockfileatomic(str);
    ans = __wscanf(&scin, str, fmt, ap);
    _Unlockfileatomic(str);
    va_end(ap);

    return ans;
}

