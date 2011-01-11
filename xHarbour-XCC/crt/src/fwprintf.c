/****************************************************************************
 *                                                                          *
 * File    : fwprintf.c                                                     *
 *                                                                          *
 * Purpose : fwprintf function.                                             *
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

/* print formatted to wide stream */
int __cdecl (fwprintf)(FILE * restrict str, const wchar_t * restrict fmt, ...)
{
    int ans;
    va_list ap;

    va_start(ap, fmt);
    _Lockfileatomic(str);
    ans = __wprintf(&prout, str, fmt, ap);
    _Unlockfileatomic(str);
    va_end(ap);

    return ans;
}

