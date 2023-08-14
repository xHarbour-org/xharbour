/****************************************************************************
 *                                                                          *
 * File    : wcsnicmp.c                                                     *
 *                                                                          *
 * Purpose : _wcsnicmp functions.                                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wchar.h>
#include <wctype.h>

/* case-insensitive compare unsigned char s1[max n], s2[max n] */
int __cdecl (_wcsnicmp)(const wchar_t *s1, const wchar_t *s2, size_t n)
{
    for (; n > 0; s1++, s2++, n--)
    {
        wint_t c1 = towlower((wint_t)*s1);
        wint_t c2 = towlower((wint_t)*s2);

        if (c1 != c2)
            return (c1 < c2) ? -1 : +1;
        else if (c1 == '\0')
            return 0;
    }

    return 0;
}

