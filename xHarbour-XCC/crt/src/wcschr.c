/****************************************************************************
 *                                                                          *
 * File    : wcschr.c                                                       *
 *                                                                          *
 * Purpose : wcschr function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wchar.h>

/* find first occurrence of c in wchar_t s[] */
wchar_t * __cdecl (wcschr)(const wchar_t *s, wchar_t c)
{
    for (; *s != c; ++s)
        if (*s == L'\0') return 0;

    return (wchar_t *)s;
}

