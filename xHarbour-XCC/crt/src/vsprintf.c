/****************************************************************************
 *                                                                          *
 * File    : vsprintf.c                                                     *
 *                                                                          *
 * Purpose : vsprintf function.                                             *
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

/* print formatted to string from arg list */
int __cdecl (vsprintf)(char * restrict s, const char * restrict fmt, va_list ap)
{
    int ans = __printf(&prout, s, fmt, ap);
    if (ans >= 0) s[ans] = '\0';

    return ans;
}

