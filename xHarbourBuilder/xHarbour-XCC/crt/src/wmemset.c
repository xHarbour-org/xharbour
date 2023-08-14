/****************************************************************************
 *                                                                          *
 * File    : wmemset.c                                                      *
 *                                                                          *
 * Purpose : wmemset function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wchar.h>

/* store c throughout wchar_t s[n] */
wchar_t * __cdecl (wmemset)(wchar_t *s, wchar_t c, size_t n)
{
    wchar_t *su = s;

    for (; n > 0; ++su, --n)
        *su = c;

    return s;
}

