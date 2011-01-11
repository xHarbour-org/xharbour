/****************************************************************************
 *                                                                          *
 * File    : xxstod.h                                                       *
 *                                                                          *
 * Purpose : _[W]Sto[d|f|ld] common functionality.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-09-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

#define D16TO7  FLIT(268435456.0)       /* 16^7 */
#define D10TO9  FLIT(1e9)               /* 10^9 */
#define NLONG   ((FBITS + 27) / 28)     /* 7 * NLONG == max hexadecimal digits */

/*
FTYPE __stod(const CTYPE *s, CTYPE **endptr, long pten)
 */
{
    /* convert string to FTYPE, with checking */
    FTYPE x;
    long lo[NLONG + 1];
    const CTYPE *s0 = s;
    int code = CNAME(stopfx)(&s, endptr);
    const int neg = code & FL_NEG;

    if ((code &= ~FL_NEG) == FL_DEC)
    {
        /* parse decimal format */
        const int nlo = CNAME(stoflt)(s0, s, endptr, lo, NLONG);

        if (nlo == 0)
            x = 0;
        else
#if NLONG == 1
            x = lo[1];
#else /* NLONG == 1 */
        {
            /* combine longs, base 10^9 */
            int i;
            static const FTYPE fac[] = {
                /* scale factors */
                D10TO9,
                D10TO9 * D10TO9,
#if NLONG > 2
                D10TO9 * D10TO9 * D10TO9,
#if NLONG > 3
                D10TO9 * D10TO9 * D10TO9 * D10TO9,
#if NLONG > 4
                D10TO9 * D10TO9 * D10TO9 * D10TO9 * D10TO9,
#if NLONG > 5
#error NLONG TOO BIG
#endif /* NLONG > 5 */
#endif /* NLONG > 4 */
#endif /* NLONG > 3 */
#endif /* NLONG > 2 */
            };

            for (i = nlo, x = lo[nlo]; 0 < --i; )
            {
                if (lo[i] != 0)
                    x += fac[nlo - i - 1] * lo[i];
            }
        }
#endif /* NLONG == 1 */

        x = FNAME(fppow10)(x, pten + lo[0]);
    }
    else if (code == FL_HEX)
    {
        /* parse hexadecimal format */
        const int nlo = CNAME(stoxflt)(s0, s, endptr, lo, NLONG);

        if (nlo == 0)
            x = 0;
        else
#if NLONG == 1
            x = lo[1];
#else /* NLONG == 1 */
        {
            /* combine longs, base 16^7 */
            int i;
            static const FTYPE fac[] = {
                /* scale factors */
                D16TO7,
                D16TO7 * D16TO7,
#if NLONG > 2
                D16TO7 * D16TO7 * D16TO7,
#if NLONG > 3
                D16TO7 * D16TO7 * D16TO7 * D16TO7,
#if NLONG > 4
                D16TO7 * D16TO7 * D16TO7 * D16TO7 * D16TO7,
#if NLONG > 5
#error NLONG TOO BIG
#endif /* NLONG > 5 */
#endif /* NLONG > 4 */
#endif /* NLONG > 3 */
#endif /* NLONG > 2 */
            };

            for (i = nlo, x = lo[nlo]; --i > 0; )
            {
                if (lo[i] != 0)
                    x += fac[nlo - i - 1] * lo[i];
            }
        }
#endif /* NLONG == 1 */

        FNAME(fpscale)(&x, lo[0]);
        x = FNAME(fppow10)(x, pten);
    }
    else if (code == FL_INF)
        x = FCONST(inf);
    else if (code == FL_NAN)
        x = FCONST(nan);
    else
        x = 0;  /* code == FL_ERR */

    return (neg != 0) ? -x : x;
}

