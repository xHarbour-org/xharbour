/****************************************************************************
 *                                                                          *
 * File    : sprintf.c                                                      *
 *                                                                          *
 * Purpose : sprintf function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <string.h>
#include "xstdio.h"

/* write to string */
static void *prout(void *s, const char *buf, size_t n)
{
    return (char *)memcpy(s, buf, n) + n;
}

/* print formatted to string */
int __cdecl (sprintf)(char * restrict s, const char * restrict fmt, ...)
{
    int ans;
    va_list ap;

    va_start(ap, fmt);
    ans = __printf(&prout, s, fmt, ap);
    if (ans >= 0) s[ans] = '\0';
    va_end(ap);

    return ans;
}

