/****************************************************************************
 *                                                                          *
 * File    : wmemmove.c                                                     *
 *                                                                          *
 * Purpose : wmemmove function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wchar.h>

/* copy wchar_t s2[n] to s1[n] safely */
wchar_t * __cdecl (wmemmove)(wchar_t *s1, const wchar_t *s2, size_t n)
{
    wchar_t *su1 = s1;

    if (s2 < su1 && su1 < s2 + n)
    {
        for (su1 += n, s2 += n; n > 0; --n)
            *--su1 = *--s2;  /*copy backwards */
    }
    else
    {
        for (; n > 0; --n)
            *su1++ = *s2++;  /* copy forwards */
    }

    return s1;
}

