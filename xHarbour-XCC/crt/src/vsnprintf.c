/****************************************************************************
 *                                                                          *
 * File    : vsnprint.c                                                     *
 *                                                                          *
 * Purpose : vsnprintf function [new C99].                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-05  Rewritten.                                            *
 *                                                                          *
 ****************************************************************************/

#include <string.h>
#include "xstdio.h"

struct args {
    char *s;
    size_t max;
};

/* write to string */
static void *prout(void *pa, const char *buf, size_t n)
{
    struct args *p = (struct args *)pa;

    if (p->max < n)
        n = p->max;   /* write only what fits in string */
    memcpy(p->s, buf, n);
    p->s += n;
    p->max -= n;

    return pa;
}

/* print formatted to string from arg list with limit */
int __cdecl (vsnprintf)(char * restrict s, size_t max, const char * restrict fmt, va_list ap)
{
    int ans;
    struct args x;

    if (max == 0)
    {
        /* write nothing */
        x.s = (char *)&x.max;  /* place to junk terminating nul */
        x.max = 0;
    }
    else
    {
        /* set up buffer */
        x.s = s;
        x.max = --max;
    }
    ans = __printf(&prout, &x, fmt, ap);
    *x.s = '\0';

    return ans;
}

