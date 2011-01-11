/****************************************************************************
 *                                                                          *
 * File    : wcsncpy.c                                                      *
 *                                                                          *
 * Purpose : wcsncpy function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wchar.h>

/* copy wchar_t s2[max n] to s1[n] */
wchar_t * __cdecl (wcsncpy)(wchar_t * restrict s1, const wchar_t * restrict s2, size_t n)
{
    wchar_t *s;

    /* copy at most n wchar_ts from s2[] */
    for (s = s1; n > 0 && *s2 != L'\0'; --n)
        *s++ = *s2++;
    for (; n > 0; --n)
        *s++ = L'\0';

    return s1;
}

