/****************************************************************************
 *                                                                          *
 * File    : wcstol.c                                                       *
 *                                                                          *
 * Purpose : wcstol function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <errno.h>
#include <limits.h>
#include <wchar.h>
#include <wctype.h>

/* convert wide string to long, with checking */
long __cdecl (wcstol)(const wchar_t * restrict s, wchar_t ** restrict endptr, int base)
{
    const wchar_t *sc;
    wchar_t *se, sign;
    unsigned long x;

    if (endptr == 0)
        endptr = &se;

    for (sc = s; iswspace(*sc); ++sc)
        ;

    sign = (*sc == L'-' || *sc == L'+') ? *sc++ : L'+';

    x = __wcstoul(sc, endptr, base);

    if (sc == *endptr)
        *endptr = (wchar_t *)s;

    if (s == *endptr && x != 0 || sign == L'+' && x > LONG_MAX || sign == L'-' && x > -(unsigned long)LONG_MIN)
    {
        /* overflow */
        errno = ERANGE;
        return (sign == L'-') ? LONG_MIN : LONG_MAX;
    }
    else
    {
        return (long)(sign == L'-' ? -x : x);
    }
}

