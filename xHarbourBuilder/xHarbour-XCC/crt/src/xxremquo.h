/****************************************************************************
 *                                                                          *
 * File    : xxremquo.h                                                     *
 *                                                                          *
 * Purpose : Common remquo[fl] functionality.                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-30  Created                                              *
 *           03-06-27  Added call to __matherr().                           *
 *                                                                          *
 ****************************************************************************/

#include <limits.h>
#include <fenv.h>
#include "xmath.h"

/* compute remainder of x/y, quotient to *pquo */
FTYPE __cdecl FFUN(remquo)(FTYPE x, FTYPE y, int *pquo)
{
    const short errx = FNAME(fptest)(&x);
    const short erry = FNAME(fptest)(&y);

    if (pquo != 0)
        *pquo = 0;

    if (errx >= 0 || erry >= 0)
    {
        /* x or y is 0, INF, or NAN */
        if (errx == FP_NAN)
            return x;
        else if (erry == FP_NAN)
            return y;
        else if (errx == FP_INFINITE || erry == 0)
        {
            /* INF/y or x/0 */
#ifdef _USE_MATHERR
            {
                struct _exception e = { _DOMAIN, FERR(remquo) };
                if (_matherr(&e)) return e.retval;
            }
#endif
            __feraise(FE_INVALID);
            return FCONST(nan);
        }
        else
            return x;  /* 0/nonzero or finite/INF */
    }
    else
    {
        /* finite/finite */
        FTYPE t;
        short n, neg, ychar;
        unsigned long intpart;

        if (y < FLIT(0.0))
            y = -y;
        if (x < FLIT(0.0))
            x = -x, neg = 1;
        else
            neg = 0;

        for (t = y, FNAME(fpunscale)(&ychar, &t), n = 0, intpart = 0;;)
        {
            /* subtract |y| until |x|<|y| */
            short xchar;

            t = x;
            if (n < 0 || FNAME(fpunscale)(&xchar, &t) == 0 || (n = xchar - ychar) < 0)
                break;
            for (; n >= 0; --n)
            {
                /* try to subtract |y|*2^n */
                t = y, FNAME(fpscale)(&t, n);
                if (t <= x)
                {
                    x -= t;
                    intpart += 1 << n;
                    break;
                }
            }
        }

        if (FLIT(0.0) < x)
        {
            /* correct for |y| <= 2*|x| */
            t = FLIT(0.5) * y;
            if (t < x && FLIT(0.0) < t || t == x && (intpart & 1) != 0)
                x -= y, ++intpart;
        }

        if (pquo != 0)
        {
            /* store integer part */
            int ans = (int)(intpart & INT_MAX);

            *pquo = (neg) ? -ans : ans;
        }

        return (neg) ? -x : x;
    }
}

