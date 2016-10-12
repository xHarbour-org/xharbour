/****************************************************************************
 *                                                                          *
 * File    : wcslen.c                                                       *
 *                                                                          *
 * Purpose : wcslen function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wchar.h>

/* find length of wchar_t s[] */
size_t __cdecl (wcslen)(const wchar_t *s)
{
    const wchar_t *sc;

    for (sc = s; *sc != L'\0'; ++sc)
        ;

    return sc - s;
}

