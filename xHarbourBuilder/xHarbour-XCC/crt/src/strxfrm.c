/****************************************************************************
 *                                                                          *
 * File    : strxfrm.c                                                      *
 *                                                                          *
 * Purpose : strxfrm function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-05  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include "xstrxfrm.h"

/* transform s2[] to s1[] by locale-dependent rule */
size_t __cdecl (strxfrm)(char * restrict s1, const char * restrict s2, size_t n)
{
    static const __xfrm initial = {0};
    __xfrm xstate = initial;
    size_t nx = 0;

    xstate.sin = xstate.sbegin = (const unsigned char *)s2;

    while (nx < n)
    {
        /* translate and deliver */
        size_t i = __cstrxfrm((char *)s1, n - nx, &xstate);

        s1 += i, nx += i;

        if (i > 0 && s1[-1] == '\0')
            return nx - 1;
    }

    for (;;)
    {
        /* translate and count */
        char buf[32];
        size_t i = __cstrxfrm(buf, sizeof(buf), &xstate);

        nx += i;

        if (i > 0 && buf[i - 1] == '\0')
            return nx - 1;
    }
}

