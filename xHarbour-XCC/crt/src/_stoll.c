/****************************************************************************
 *                                                                          *
 * File    : _stoll.c                                                       *
 *                                                                          *
 * Purpose : __stoll function [new C99].                                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-09-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdlib.h>
#include "xmath.h"

unsigned long long __cdecl __stoull(const char *, char **, int);

/* convert string to long long, with checking */
long long __cdecl (__stoll)(const char * s, char ** endptr, int base)
{
    const char *sc;
    char *se, sign;
    unsigned long long x;

    if (endptr == 0)
        endptr = &se;

    for (sc = s; isspace(*sc); ++sc)
        ;

    sign = (*sc == '-' || *sc == '+') ? *sc++ : '+';

    x = __stoull(sc, endptr, base);

    if (sc == *endptr)
        *endptr = (char *)s;

    if (s == *endptr && x != 0 || sign == '+' && x > LLONG_MAX || sign == '-' && x > -(unsigned long long)LLONG_MIN)
    {
        /* overflow */
        errno = ERANGE;
        return (sign == '-') ? LLONG_MIN : LLONG_MAX;
    }
    else
    {
        return (long long)(sign == '-' ? -x : x);
    }
}

