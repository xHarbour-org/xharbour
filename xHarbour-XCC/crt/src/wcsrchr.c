/****************************************************************************
 *                                                                          *
 * File    : wcsrchr.c                                                      *
 *                                                                          *
 * Purpose : wcsrchr function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wchar.h>

/* find last occurrence of c in wchar_t s[] */
wchar_t * __cdecl (wcsrchr)(const wchar_t *s, wchar_t c)
{
    const wchar_t *sc;

    for (sc = 0;; ++s)
    {
        /* check another wchar_t */
        if (*s == c)
            sc = s;

        if (*s == L'\0')
            return (wchar_t *)sc;
    }
}

