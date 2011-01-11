/****************************************************************************
 *                                                                          *
 * File    : wmemcpy.c                                                      *
 *                                                                          *
 * Purpose : wmemcpy function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wchar.h>

/* copy wchar_t s2[n] to s1[n] in any order */
wchar_t * __cdecl (wmemcpy)(wchar_t * restrict s1, const wchar_t * restrict s2, size_t n)
{
    wchar_t *su1 = s1;

    for (; n > 0; ++su1, ++s2, --n)
        *su1 = *s2;

    return s1;
}

