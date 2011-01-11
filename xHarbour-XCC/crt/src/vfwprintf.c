/****************************************************************************
 *                                                                          *
 * File    : vfwprintf.c                                                    *
 *                                                                          *
 * Purpose : vfwprintf function [new C99?].                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-02  Added _Lockfileatomic() and _Unlockfileatomic().     *
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

/* print formatted to wide stream from arg list */
int __cdecl (vfwprintf)(FILE * restrict str, const wchar_t * restrict fmt, va_list ap)
{
    int ans;

    _Lockfileatomic(str);
    ans = __wprintf(&prout, str, fmt, ap);
    _Unlockfileatomic(str);

    return ans;
}

