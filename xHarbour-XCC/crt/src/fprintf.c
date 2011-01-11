/****************************************************************************
 *                                                                          *
 * File    : fprintf.c                                                      *
 *                                                                          *
 * Purpose : fprintf function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-02  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include "xstdio.h"

/* write to file */
static void *prout(void *str, const char *buf, size_t n)
{
    return (fwrite(buf, 1, n, (FILE *)str) == n) ? str : 0;
}

/* print formatted to stream */
int __cdecl (fprintf)(FILE * restrict str, const char * restrict fmt, ...)
{
    int ans;
    va_list ap;

    va_start(ap, fmt);
    _Lockfileatomic(stdout);
    ans = __printf(&prout, str, fmt, ap);
    _Unlockfileatomic(stdout);
    va_end(ap);

    return ans;
}

