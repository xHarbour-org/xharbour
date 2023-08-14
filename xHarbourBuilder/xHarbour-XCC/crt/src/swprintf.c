/****************************************************************************
 *                                                                          *
 * File    : swprintf.c                                                     *
 *                                                                          *
 * Purpose : swprintf function.                                             *
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
static void *prout(void *pa, const wchar_t *buf, size_t request)
{
    struct args *p = (struct args *)pa;
    if (p->max < request)
    {
        /* deliver short string */
        wmemcpy(p->s, buf, p->max);
        return 0;
    }
    else
    {
        /* deliver full string */
        wmemcpy(p->s, buf, request);
        p->s += request;
        p->max -= request;
        return pa;
    }
}

/* print formatted to wide string */
int __cdecl (swprintf)(wchar_t * restrict s, size_t max, const wchar_t * restrict fmt, ...)
{
    int ans;
    va_list ap;
    struct args x;

    va_start(ap, fmt);
    x.s = s;
    x.max = max;
    ans = __wprintf(&prout, &x, fmt, ap);
    if (ans >= 0) s[ans] = L'\0';
    va_end(ap);

    return ans;
}

