/****************************************************************************
 *                                                                          *
 * File    : xxfmod.h                                                       *
 *                                                                          *
 * Purpose : Common fmod[fl] functionality.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *           03-06-27  Added call to __matherr().                           *
 *                                                                          *
 ****************************************************************************/

#include <fenv.h>
#include "xmath.h"

/* compute fmod(x, y) */
FTYPE __cdecl (FFUN(fmod))(FTYPE x, FTYPE y)
{
    const short errx = FNAME(fptest)(&x);
    const short erry = FNAME(fptest)(&y);

    if (errx >= 0 || erry >= 0)
    {
        /* x or y is 0, INF, or NAN */
        if (errx == FP_NAN)
            return x;
        else if (erry == FP_NAN)
            return y;
        else if (errx == FP_INFINITE || erry == 0)
        {
            /* fmod(FP_INFINITE, y) or fmod(x, 0) */
#ifdef _USE_MATHERR
            {
                struct _exception e = { _DOMAIN, FERR(fmod) };
                if (_matherr(&e)) return e.retval;
            }
#endif
            __feraise(FE_INVALID);
            return FCONST(nan);
        }
        else
            return x;  /* fmod(0, nonzero) or fmod(finite, INF) */
    }
    else
    {
        /* fmod(finite, finite) */
        FTYPE t;
        short n, neg, ychar;

        if (y < FLIT(0.0))
            y = -y;
        if (x < FLIT(0.0))
            x = -x, neg = 1;
        else
            neg = 0;

        for (t = y, FNAME(fpunscale)(&ychar, &t), n = 0; ; )
        {
            /* subtract |y| until |x|<|y| */
            short xchar;

            t = x;
            if (n < 0 || FNAME(fpunscale)(&xchar, &t) == 0 || (n = xchar - ychar) < 0)
                return (neg) ? -x : x;

            for (; 0 <= n; --n)
            {
                /* try to subtract |y|*2^n */
                t = y, FNAME(fpscale)(&t, n);
                if (t <= x)
                {
                    x -= t;
                    break;
                }
            }
        }
    }
}

