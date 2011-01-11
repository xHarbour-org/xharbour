/****************************************************************************
 *                                                                          *
 * File    : vswprintf.c                                                    *
 *                                                                          *
 * Purpose : vswprintf function [new C99?].                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xwstdio.h"

struct args {
    wchar_t *s;
    size_t max;
};

/* write to wide string */
static void *prout(void *pa, const wchar_t *buf, size_t n)
{
    struct args *p = (struct args *)pa;

    if (p->max <= n)
    {
        /* deliver shortend string */
        wmemcpy(p->s, buf, p->max);
        return 0;
    }
    else
    {
        /* deliver full string */
        wmemcpy(p->s, buf, n);
        p->s += n;
        p->max -= n;
        return pa;
    }
}

/* print formatted to wide string from arg list */
int __cdecl (vswprintf)(wchar_t * restrict s, size_t max, const wchar_t * restrict fmt, va_list ap)
{
    int ans;
    struct args x;

    x.s = s;
    x.max = max;
    ans = __wprintf(&prout, &x, fmt, ap);
    if (ans >= 0) s[ans] = L'\0';

    return ans;
}

