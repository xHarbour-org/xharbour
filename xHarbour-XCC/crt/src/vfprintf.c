/****************************************************************************
 *                                                                          *
 * File    : vfprintf.c                                                     *
 *                                                                          *
 * Purpose : vfprintf function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-02  Added _Lockfileatomic() and _Unlockfileatomic().     *
 *                                                                          *
 ****************************************************************************/

#include "xstdio.h"

/* write to file */
static void *prout(void *str, const char *buf, size_t n)
{
    return (fwrite(buf, 1, n, (FILE *)str) == n) ? str : 0;
}

/* print formatted to stream from arg list */
int __cdecl (vfprintf)(FILE * restrict str, const char * restrict fmt, va_list ap)
{
    int ans;

    _Lockfileatomic(str);
    ans = __printf(&prout, str, fmt, ap);
    _Unlockfileatomic(str);

    return ans;
}

