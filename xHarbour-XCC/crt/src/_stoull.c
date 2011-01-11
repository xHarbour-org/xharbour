/****************************************************************************
 *                                                                          *
 * File    : _stoull.c                                                      *
 *                                                                          *
 * Purpose : __stoull function [new C99].                                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stddef.h>
#include <string.h>

#pragma optimize(none)

#if ULLONG_MAX != 18446744073709551615ULL
#error LONG LONGS TOO LARGE FOR __stoull
#endif

/* macros */
#define BASE_MAX  36  /* largest valid base */

/* static data */
static const char digits[] =    /* valid digits (36) */
    "0123456789abcdefghijklmnopqrstuvwxyz";

static const char ndigs[BASE_MAX+1] = {  /* 64-bits! */
    0, 0, 65, 41, 33, 28, 25, 23, 22, 21,
    20, 19, 18, 18, 17, 17, 17, 16, 16, 16,
    15, 15, 15, 15, 14, 14, 14, 14, 14, 14,
    14, 13, 13, 13, 13, 13, 13
};

/* convert string to unsigned long long, with checking */
unsigned long long __cdecl __stoull(const char * restrict s, char ** restrict endptr, int base)
{
    const char *sc, *sd;
    const char *s1, *s2;
    char dig, sign;
    ptrdiff_t n;
    unsigned long long x, y;

    for (sc = s; isspace(*sc); ++sc)
        ;

    sign = (*sc == '-' || *sc == '+') ? *sc++ : '+';

    if (base < 0 || base == 1 || base > BASE_MAX)
    {
        /* silly base */
        if (endptr != 0)
            *endptr = (char *)s;
        return 0;
    }
    else if (base > 0)
    {
        /* strip 0x or 0X */
        if (base == 16 && *sc == '0' && (sc[1] == 'x' || sc[1] == 'X'))
            sc += 2;
    }
    else if (*sc != '0')
        base = 10;
    else if (sc[1] == 'x' || sc[1] == 'X')
        base = 16, sc += 2;
    else
        base = 8;

    /* skip leading zeros */
    for (s1 = sc; *sc == '0'; ++sc)
        ;

    x = 0;
    for (s2 = sc; (sd = (char *)memchr(digits, tolower(*sc), base)) != 0; ++sc)
    {
        /* accumulate digits */
        y = x;
        dig = (char)(sd - digits);  /* for overflow checking */
        x = x * base + dig;
    }
    if (s1 == sc)
    {
        /* check string validity */
        if (endptr != 0)
            *endptr = (char *)s;
        return 0;
    }

    n = sc - s2 - ndigs[base];
    if (n < 0)
        ;
    else if (n > 0 || x < x - dig || (x - dig) / base != y)
    {
        /* overflow */
        errno = ERANGE;
        sc = s, x = ULLONG_MAX, sign = '+';
    }

    /* get final value */
    if (sign == '-')
        x = -x;

    if (endptr != 0)
        *endptr = (char *)sc;

    return x;
}

