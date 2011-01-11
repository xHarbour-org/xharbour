/****************************************************************************
 *                                                                          *
 * File    : _wstoxflt.c                                                    *
 *                                                                          *
 * Purpose : __wstoxflt function [new C99].                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-09-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <ctype.h>
#include <locale.h>
#include <wchar.h>
#include <wctype.h>
#include "xmath.h"

#define NDIG  7  /* hexadecimal digits per long element */

/* convert string to array of long plus exponent */
int __wstoxflt(const wchar_t *s0, const wchar_t *s, wchar_t **endptr, long lo[], int maxsig)
{
    int nd, nsig, seen;
    long *pl = &lo[0];      /* lo[0] will hold exponent */
    const wchar_t *pd;
    static const wchar_t digits[] = {  /* hex digits in both cases */
        L'0', L'1', L'2', L'3',
        L'4', L'5', L'6', L'7',
        L'8', L'9', L'a', L'b',
        L'c', L'd', L'e', L'f',
        L'A', L'B', L'C', L'D',
        L'E', L'F', L'\0'
    };
    static const wchar_t vals[] = {  /* values of hex digits */
        0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 10, 11, 12, 13, 14, 15
    };

    maxsig *= NDIG;

    for (seen = 0; *s == L'0'; ++s, seen = 1)
        ;  /* strip leading zeros */

    for (nsig = 0, lo[0] = 0; (pd = (wchar_t *)wmemchr(&digits[0], *s, 22)) != 0; ++s, seen = 1)
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
            if (nsig == maxsig && 8 <= vals[pd - digits])
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

    for (; (pd = (wchar_t *)wmemchr(&digits[0], *s, 22)) != 0; ++s, seen = 1)
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

    lo[0] <<= 2;    /* change hex exponent to binary exponent */
    if (seen && (*s == L'p' || *s == L'P'))
    {
        /* parse exponent */
        const wchar_t esign = (*++s == L'+' || *s == L'-') ? *s++ : L'+';
        long lexp = 0;

            for (seen = 0; isdigit(*s); ++s, seen = 1)
            {
                if (lexp < 100000000)   /* else overflow */
                    lexp = lexp * 10 + *s - L'0';
            }

            if (esign == '-')
                lexp = -lexp;
            lo[0] += lexp;
    }

    if (endptr)
        *endptr = (wchar_t *)(seen ? s : s0);

    return nsig / NDIG;
}

