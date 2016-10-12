/****************************************************************************
 *                                                                          *
 * File    : wcsxfrm.c                                                      *
 *                                                                          *
 * Purpose : wcsxfrm function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xwcsxfrm.h"

/* transform s2[] to s1[] by locale-dependent rule */
size_t __cdecl (wcsxfrm)(wchar_t * restrict s1, const wchar_t * restrict s2, size_t n)
{
    size_t nx = 0;
    const wchar_t *s = s2;
    mbstate_t mbst = {0};

    while (nx < n)
    {
        /* translate and deliver */
        size_t i = __wcsxfrm(s1, &s, n - nx, &mbst);

        s1 += i, nx += i;

        if (i > 0 && s1[-1] == L'\0')
            return nx - 1;
        else if (nx < n && *s == L'\0')
            s = s2;  /* rescan */
    }

    for (;;)
    {
        /* translate and count */
        wchar_t buf[32];
        size_t i = __wcsxfrm(buf, &s, sizeof(buf) / sizeof(wchar_t), &mbst);

        nx += i;

        if (i > 0 && buf[i - 1] == L'\0')
            return nx - 1;
        else if (i < sizeof(buf) / sizeof(wchar_t) && *s == L'\0')
            s = s2;  /* rescan */
    }
}

