/****************************************************************************
 *                                                                          *
 * File    : strtol.c                                                       *
 *                                                                          *
 * Purpose : strtol function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdlib.h>

/* convert string to long, with checking */
long __cdecl (strtol)(const char * restrict s, char ** restrict endptr, int base)
{
    const char *sc;
    char *se, sign;
    unsigned long x;

    if (endptr == 0)
        endptr = &se;

    for (sc = s; isspace(*sc); ++sc)
        ;

    sign = (*sc == '-' || *sc == '+') ? *sc++ : '+';

    x = __stoul(sc, endptr, base);

    if (sc == *endptr)
        *endptr = (char *)s;

    if (s == *endptr && x != 0 || sign == '+' && x > LONG_MAX || sign == '-' && x > -(unsigned long)LONG_MIN)
    {
        /* overflow */
        errno = ERANGE;
        return (sign == '-') ? LONG_MIN : LONG_MAX;
    }
    else
    {
        return (long)((sign == '-') ? -x : x);
    }
}

