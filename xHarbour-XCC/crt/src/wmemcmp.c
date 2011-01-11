/****************************************************************************
 *                                                                          *
 * File    : wmemcmp.c                                                      *
 *                                                                          *
 * Purpose : wmemcmp function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wchar.h>

/* compare wchar_t s1[n], s2[n] */
int __cdecl (wmemcmp)(const wchar_t *s1, const wchar_t *s2, size_t n)
{
    for (; n > 0; ++s1, ++s2, --n)
    {
        if (*s1 != *s2)
            return (*s1 < *s2) ? -1 : +1;
    }

    return 0;
}

