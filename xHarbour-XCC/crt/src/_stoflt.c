/****************************************************************************
 *                                                                          *
 * File    : _stoflt.c                                                      *
 *                                                                          *
 * Purpose : __stoflt function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-09  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <ctype.h>
#include <locale.h>
#include <stdlib.h>
#include "xmath.h"

#define NDIG  9  /* decimal digits per long element */

/* convert string to array of long plus exponent */
int __stoflt(const char *s0, const char *s, char **endptr, long lo[], int maxsig)
{
    int nd, nsig, seen;
    long *pl = &lo[0];   /* lo[0] will hold exponent */

    maxsig *= NDIG;

    for (seen = 0; *s == '0'; ++s, seen = 1)
        ;  /* strip leading zeros */

    for (nsig = 0, lo[0] = 0; isdigit(*s); ++s, seen = 1)
    {
        if (nsig < maxsig)
        {
            /* accumulate a digit */
            if (nsig % NDIG == 0)
                *++pl = 0;
            *pl = *pl * 10 + *s - '0';
            ++nsig;
        }
        else
        {
            /* just count the digit, and maybe round up */
            ++lo[0];
            if (nsig == maxsig && '5' <= *s)
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

    for (; isdigit(*s); ++s, seen = 1)
    {
        if (nsig < maxsig)
        {
            /* accumulate a fraction digit */
            if (nsig % NDIG == 0)
                *++pl = 0;
            *pl = *pl * 10 + *s - '0';
            ++nsig, --lo[0];
        }
    }

    if ((nd = nsig % NDIG) != 0)
    {
        /* scale lsw to a full NDIG digits */
        static const long lfac[] = { 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000 };

        nd = NDIG - nd;
        if (*pl != 0)
            *pl *= lfac[nd - 1];
        nsig += nd, lo[0] -= nd;
    }

    for (; nsig >= NDIG && *pl == 0; --pl)
        nsig -= NDIG, lo[0] += NDIG;  /* strip trailing zero longs */

    if (seen && (*s == 'e' || *s == 'E'))
    {
        /* parse exponent */
        const char esign = (*++s == '+' || *s == '-') ? *s++ : '+';
        long lexp = 0;

        for (seen = 0; isdigit(*s); ++s, seen = 1)
        {
            if (lexp < 100000000)  /* else overflow */
                lexp = lexp * 10 + *s - '0';
        }

        if (esign == '-')
            lexp = -lexp;

        lo[0] += lexp;
    }

    if (endptr)
        *endptr = (char *)(seen ? s : s0);

    return nsig / NDIG;
}

