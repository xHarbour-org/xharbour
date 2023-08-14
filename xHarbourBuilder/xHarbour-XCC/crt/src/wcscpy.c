/****************************************************************************
 *                                                                          *
 * File    : wcscpy.c                                                       *
 *                                                                          *
 * Purpose : wcscpy function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wchar.h>

/* copy wchar_t s2[] to s1[] */
wchar_t * __cdecl (wcscpy)(wchar_t * restrict s1, const wchar_t * restrict s2)
{
    wchar_t *s;

    for (s = s1; (*s++ = *s2++) != '\0'; )
        ;

    return s1;
}

