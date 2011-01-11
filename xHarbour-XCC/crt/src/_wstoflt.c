/****************************************************************************
 *                                                                          *
 * File    : _wstoflt.c                                                     *
 *                                                                          *
 * Purpose : __wstoflt function.                                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-10  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <locale.h>
#include <wchar.h>
#include <wctype.h>

#define NDIG  9  /* decimal digits per long element */

/* convert string to array of long plus exponent */
int __wstoflt(const wchar_t *s0, const wchar_t *s, wchar_t **endptr, long lo[], int maxsig)
{
    int nd, nsig, seen;
    long *pl = &lo[0];  /* lo[0] will hold exponent */

    maxsig *= NDIG;

    for (seen = 0; *s == L'0'; ++s, seen = 1)
        ;  /* strip leading zeros */

    for (nsig = 0, lo[0] = 0; iswdigit(*s); ++s, seen = 1)
    {
        if (nsig < maxsig)
        {
            /* accumulate a digit */
            if (nsig % NDIG == 0)
                *++pl = 0;
            *pl = *pl * 10 + *s - L'0';
            ++nsig;
        }
        else
        {
            /* just count the digit, and maybe round up */
            ++lo[0];
            if (nsig == maxsig && L'5' <= *s)
                ++*pl, ++nsig;  /* round up */
        }
    }

    if (maxsig < nsig)
        nsig = maxsig;

    if (*s == btowc(localeconv()->decimal_point[0]))
        ++s;

    if (nsig == 0)
    {
        for (; *s == L'0'; ++s, seen = 1)
            --lo[0];  /* strip zeros after point */
    }

    for (; iswdigit(*s); ++s, seen = 1)
    {
        if (nsig < maxsig)
        {
            /* accumulate a fraction digit */
            if (nsig % NDIG == 0)
                *++pl = 0;
            *pl = *pl * 10 + *s - L'0';
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

    for (; NDIG <= nsig && *pl == 0; --pl)
        nsig -= NDIG, lo[0] += NDIG;  /* strip trailing zero longs */

    if (seen && (*s == L'e' || *s == L'E'))
    {
        /* parse exponent */
        const wchar_t esign = (*++s == L'+' || *s == L'-') ? *s++ : L'+';
        long lexp = 0;

        for (seen = 0; iswdigit(*s); ++s, seen = 1)
        {
            if (lexp < 100000000)   /* else overflow */
                lexp = lexp * 10 + *s - L'0';
        }

        if (esign == L'-')
            lexp = -lexp;

        lo[0] += lexp;
    }

    if (endptr)
        *endptr = (wchar_t *)(seen ? s : s0);

    return nsig / NDIG;
}

