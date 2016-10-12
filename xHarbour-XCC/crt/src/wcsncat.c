/****************************************************************************
 *                                                                          *
 * File    : wcsncat.c                                                      *
 *                                                                          *
 * Purpose : wcsncat function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wchar.h>

/* copy wchar_t s2[max n] to end of s1[] */
wchar_t * __cdecl (wcsncat)(wchar_t * restrict s1, const wchar_t * restrict s2, size_t n)
{
    wchar_t *s;

    /* find end of s1[] */
    for (s = s1; *s != L'\0'; ++s)
        ;

    /* copy at most n wchar_ts from s2[] */
    for (; n > 0 && *s2 != L'\0'; --n)
        *s++ = *s2++;

    *s = L'\0';

    return s1;
}

