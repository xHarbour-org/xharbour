/****************************************************************************
 *                                                                          *
 * File    : wmemchr.c                                                      *
 *                                                                          *
 * Purpose : wmemchr function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wchar.h>

/* find first occurrence of c in wchar_t s[n] */
wchar_t * __cdecl (wmemchr)(const wchar_t *s, wchar_t c, size_t n)
{
    for (; n > 0; ++s, --n)
    {
        if (*s == c)
            return (wchar_t *)s;
    }

    return 0;
}

