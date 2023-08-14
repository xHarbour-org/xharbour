/****************************************************************************
 *                                                                          *
 * File    : wcsncmp.c                                                      *
 *                                                                          *
 * Purpose : wcsncmp function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wchar.h>

/* compare wchar_t s1[max n], s2[max n] */
int __cdecl (wcsncmp)(const wchar_t *s1, const wchar_t *s2, size_t n)
{
    for (; n > 0; ++s1, ++s2, --n)
    {
        if (*s1 != *s2)
            return (*(wchar_t *)s1 < *(wchar_t *)s2) ? -1 : +1;
        else if (*s1 == L'\0')
            return 0;
    }

    return 0;
}

