/****************************************************************************
 *                                                                          *
 * File    : wcscat.c                                                       *
 *                                                                          *
 * Purpose : wcscat function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wchar.h>

/* copy wchar_t s2[] to end of s1[] */
wchar_t * __cdecl (wcscat)(wchar_t * restrict s1, const wchar_t * restrict s2)
{
    wchar_t *s;

    /* find end of s1[] */
    for (s = s1; *s != L'\0'; ++s)
        ;
    /* copy s2[] to end */
    for (; (*s = *s2) != L'\0'; ++s, ++s2)
        ;

    return s1;
}

