/****************************************************************************
 *                                                                          *
 * File    : wcsicmp.c                                                      *
 *                                                                          *
 * Purpose : _wcsicmp functions.                                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wchar.h>
#include <wctype.h>

/* case-insensitive compare wchar_t s1[], s2[] */
int __cdecl (_wcsicmp)(const wchar_t *s1, const wchar_t *s2)
{
    wint_t c1, c2;

    do
    {
        c1 = towlower((wint_t)*s1), s1++;
        c2 = towlower((wint_t)*s2), s2++;
    } while (c1 && c1 == c2);

    /* return (c1 < c2) ? -1 : (c1 > c2) ? +1 : 0; */
    return c1 - c2;
}

