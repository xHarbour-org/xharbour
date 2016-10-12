/****************************************************************************
 *                                                                          *
 * File    : _wcstoll.c                                                     *
 *                                                                          *
 * Purpose : __wcstoll function [new C99].                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-09-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <errno.h>
#include <limits.h>
#include <wctype.h>
#include "xwchar.h"

/* convert wide string to long long, with checking */
long long (__wcstoll)(const wchar_t * s, wchar_t **endptr, int base)
{
    const wchar_t *sc;
    wchar_t *se, sign;
    unsigned long long x;

    if (endptr == 0)
        endptr = &se;

    for (sc = s; iswspace(*sc); ++sc)
        ;

    sign = (*sc == L'-' || *sc == L'+') ? *sc++ : L'+';

    x = __wcstoull(sc, endptr, base);

    if (sc == *endptr)
        *endptr = (wchar_t *)s;

    if (s == *endptr && x != 0 || sign == L'+' && x > LLONG_MAX || sign == L'-' && x > -(unsigned long long)LLONG_MIN)
    {
        /* overflow */
        errno = ERANGE;
        return (sign == L'-') ? LLONG_MIN : LLONG_MAX;
    }
    else
    {
        return (long long)(sign == L'-' ? -x : x);
    }
}

