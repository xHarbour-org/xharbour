/****************************************************************************
 *                                                                          *
 * File    : _ldtob.c                                                       *
 *                                                                          *
 * Purpose : __ldtob function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-10  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <float.h>
#include <stdlib.h>
#include <string.h>
#include "xmath.h"
#include "xstdio.h"

/* macros */
#define MAXDIG  40  /* safe for 128-bit long double */
#define NDIG    8   /* decimal digits generated for each multiply */
#define NXDIG   7   /* hexadecimal digits generated for each multiply */

/* static data */
static const long double pows[] = {
    1e1L, 1e2L, 1e4L, 1e8L, 1e16L, 1e32L,
#if _LBIAS > 0x100  /* assume IEEE 754 8- or 10-byte */
    1e64L, 1e128L, 1e256L,
#if _DLONG  /* assume IEEE 754 10-byte */
    1e512L, 1e1024L, 1e2048L, 1e4096L,
#endif
#endif
};

/* convert long double to text */
void __ldtob(__prtinfo *px, char code)
{
    char ac[MAXDIG];
    char *p = ac;
    long double ldval = px->v.ld;
    short errx, nsig, xexp;

    if (code == 'a' || code == 'A')
        ;
    else if (px->prec < 0)
        px->prec = 6;
    else if (px->prec == 0 && (code == 'g' || code == 'G'))
        px->prec = 1;

    if ((errx = __fpunscalel(&xexp, &px->v.ld)) == FP_NAN)
    {
        /* x == NaN */
        memcpy(px->s, code == 'a' || code == 'e' || code == 'f' || code == 'g' ? "nan" : "NAN", px->n1 = 3);
        return;
    }
    else if (errx > 0)
    {
        /* x == INF */
        memcpy(px->s, code == 'a' || code == 'e' || code == 'f' || code == 'g' ? "inf" : "INF", px->n1 = 3);
        return;
    }

    if (code == 'a' || code == 'A')
    {
        /* put "0x" */
        *px->s++ = '0';
        *px->s++ = (code == 'a') ? 'x' : 'X';
        px->n0 +=2;
    }

    if (errx == 0)  /* x == 0 */
        nsig = 0, xexp = 0;
    else if (code == 'a' || code == 'A')
    {
        /* 0 < |x|, generate hex fraction, binary exponent */
        const char *digits = (code == 'a') ? "0123456789abcdef" : "0123456789ABCDEF";
        int gen;

        nsig = (px->prec < 0) ? MAXDIG - NXDIG : px->prec + 1;
        gen = nsig + 1;
        ldval = (ldval < 0) ? -px->v.ld : px->v.ld;
        xexp -= 4;  /* one leading nonzero hex digit */

        for (*p++ = 0x0; 0 < gen && 0 < ldval; p += NXDIG)
        {
            /* convert NXDIG at a time */
            int j;
            long lo;

            __fpscalel(&ldval, 4 * NXDIG);
            lo = (long)ldval;

            if ((gen -= NXDIG) > 0)
                ldval -= (long double)lo;

            for (p += NXDIG, j = NXDIG; 0 < lo && 0 <= --j; )
                *--p = (int)(lo & 0xf), lo >>= 4;

            while (--j >= 0)
                *--p = 0;
        }

        gen = p - &ac[1];
        p = &ac[1];

        if (gen < nsig)
            nsig = gen;

        if (nsig >= 0)
        {
            /* round and strip trailing zeros */
            const char drop = nsig < gen && 0x8 <= p[nsig] ? 0xf : 0x0;
            int n;

            for (n = nsig; p[--n] == drop; )
                --nsig;

            if (drop == 0xf)
                ++p[n];

            if (n < 0)
                --p, ++nsig, xexp += 4;

            for (n = nsig; 0 <= --n; )
                p[n] = digits[p[n]];
        }

        if (px->prec < 0)
            px->prec = nsig - 1;
    }
    else
    {
        /* 0 < |x|, generate decimal fraction and exponent */

        /* scale ldval to ~~10^(NDIG/2) */
        {
            int i, n;

            if (ldval < 0)
                ldval = -ldval;

            if ((xexp = xexp * 30103L / 100000L - NDIG / 2) < 0)
            {
                /* scale up */
                n = (-xexp + (NDIG / 2 - 1)) & ~(NDIG / 2 - 1);
                xexp = -n;
                for (i = 0; 0 < n; n >>= 1, ++i)
                    if (n & 1) ldval *= pows[i];
            }
            else if (xexp > 0)
            {
                /* scale down */
                long double factor = 1.0;

                xexp &= ~(NDIG / 2 - 1);
                for (n = xexp, i = 0; 0 < n; n >>= 1, ++i)
                    if (n & 1) factor *= pows[i];
                ldval /= factor;
            }
        }

        /* convert significant digits */
        {
            int gen = px->prec + (code == 'f' || code == 'F' ? xexp + 2 + NDIG : 2 + NDIG / 2);

            if (LDBL_DIG + NDIG / 2 < gen)
                gen = LDBL_DIG + NDIG / 2;

            for (*p++ = '0'; gen > 0 && ldval > 0; p += NDIG)
            {
                /* convert NDIG at a time */
                int j;
                long lo = (long)ldval;

                if ((gen -= NDIG) > 0)
                    ldval = (ldval - (long double)lo) * 1e8L;

                for (p += NDIG, j = NDIG; lo > 0 && --j >= 0; )
                {
                    /* convert NDIG digits */
                    ldiv_t qr = ldiv(lo, 10);
                    *--p = qr.rem + '0', lo = qr.quot;
                }

                while (--j >= 0)
                    *--p = '0';
            }

            gen = p - &ac[1];

            for (p = &ac[1], xexp += NDIG-1; *p == '0'; ++p)
                --gen, --xexp;  /* correct xexp */

            nsig = px->prec + (code == 'f' || code == 'F' ? xexp + 1 : code == 'e' || code == 'E' ? 1 : 0);
            if (gen < nsig)
                nsig = gen;
            if (nsig >= 0)
            {
                /* round and strip trailing zeros */
                const char drop = (nsig < gen && p[nsig] >= '5') ? '9' : '0';
                int n;

                for (n = nsig; p[--n] == drop; )
                    --nsig;

                if (drop == '9')
                    ++p[n];

                if (n < 0)
                    --p, ++nsig, ++xexp;
            }
        }
    }

    __genld(px, code, p, nsig, xexp);
}

