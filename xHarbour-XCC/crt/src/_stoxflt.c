/****************************************************************************
 *                                                                          *
 * File    : _stoxflt.c                                                     *
 *                                                                          *
 * Purpose : __stoxflt function [new C99].                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-09-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <ctype.h>
#include <locale.h>
#include <stdlib.h>
#include <string.h>
#include "xmath.h"

#define NDIG  7  /* hexadecimal digits per long element */

/* convert string to array of long plus exponent */
int __stoxflt(const char *s0, const char *s, char **endptr, long lo[], int maxsig)
{
    int nd, nsig, seen;
    long *pl = &lo[0];      /* lo[0] will hold exponent */
    const char *pd;
    static const char digits[] = "0123456789abcdefABCDEF";
    static const char vals[] = {  /* values of hex digits */
        0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 10, 11, 12, 13, 14, 15
    };

    maxsig *= NDIG;

    for (seen = 0; *s == '0'; ++s, seen = 1)
        ;   /* strip leading zeros */

    for (nsig = 0, lo[0] = 0; (pd = (char *)memchr(&digits[0], *s, 22)) != 0; ++s, seen = 1)
    {
        if (nsig < maxsig)
        {
            /* accumulate a digit */
            if (nsig % NDIG == 0)
                *++pl = 0;
            *pl = (*pl << 4) + vals[pd - digits];
            ++nsig;
        }
        else
        {
            /* just count the digit, and maybe round up */
            ++lo[0];
            if (nsig == maxsig && vals[pd - digits] >= 8)
                ++*pl, ++nsig;  /* round up */
        }
    }

    if (maxsig < nsig)
        nsig = maxsig;

    if (*s == localeconv()->decimal_point[0])
        ++s;

    if (nsig == 0)
    {
        for (; *s == '0'; ++s, seen = 1)
            --lo[0];  /* strip zeros after point */
    }

    for (; (pd = (char *)memchr(&digits[0], *s, 22)) != 0; ++s, seen = 1)
    {
        if (nsig < maxsig)
        {
            /* accumulate a fraction digit */
            if (nsig % NDIG == 0)
                *++pl = 0;
            *pl = *pl * 16 + vals[pd - digits];
            ++nsig, --lo[0];
        }
    }

    if ((nd = nsig % NDIG) != 0)
    {
        /* scale lsw to a full NDIG digits */
        nd = NDIG - nd;
        *pl <<= nd * 4;
        nsig += nd, lo[0] -= nd;
    }

    for (; NDIG <= nsig && *pl == 0; --pl)
        nsig -= NDIG, lo[0] += NDIG;  /* strip trailing zero longs */

    lo[0] <<= 2;  /* change hex exponent to binary exponent */
    if (seen && (*s == 'p' || *s == 'P'))
    {
        /* parse exponent */
        const char esign = (*++s == '+' || *s == '-') ? *s++ : '+';
        long lexp = 0;

        for (seen = 0; isdigit(*s); ++s, seen = 1)
        {
            if (lexp < 100000000)   /* else overflow */
                lexp = lexp * 10 + *s - '0';
        }

        if (esign == '-')
            lexp = -lexp;
        lo[0] += lexp;
    }

    if (endptr != 0)
        *endptr = (char *)(seen ? s : s0);

    return nsig / NDIG;
}

