/****************************************************************************
 *                                                                          *
 * File    : xxtgamma.h                                                     *
 *                                                                          *
 * Purpose : Common tgamma[fl] functionality.                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-30  Created                                              *
 *           03-06-27  Added call to __matherr().                           *
 *                                                                          *
 ****************************************************************************/

#include <fenv.h>
#include "xmath.h"

FTYPE (FNAME(lgamma))(FTYPE x);
FTYPE (FNAME(tgamma))(FTYPE x);

extern FTYPE FNAME(gamma_big);

static const FTYPE pi = FLIT(3.14159265358979323846264338327950287);

/* compute gamma(x) */
FTYPE __cdecl (FFUN(tgamma))(FTYPE x)
{
    FTYPE y = x;

    switch (FNAME(fpint)(&y, 0))
    {
        /* test for special codes */
        case FP_NAN:
            return x;

        case FP_INFINITE:
            if (!FISNEG(x))
                return x;
            else
            {
                /* -INF is invalid */
#ifdef _USE_MATHERR
                {
                    struct _exception e = { _DOMAIN, FERR(tgamma) };
                    if (_matherr(&e)) return e.retval;
                }
#endif
                __feraise(FE_INVALID);
                return FCONST(nan);
            }

        case 0:
            if (x == FLIT(0.0))
            {
                /* gamma(0) is +/-INF */
#ifdef _USE_MATHERR
                {
                    struct _exception e = { _OVERFLOW, FERR(tgamma) };
                    if (_matherr(&e)) return e.retval;
                }
#endif
                __feraise(FE_DIVBYZERO);
                return FISNEG(x) ? -FCONST(inf) : FCONST(inf);
            }
            else if (x < FLIT(0.0))
            {
                /* x a negative integer, undefined */
#ifdef _USE_MATHERR
                {
                    struct _exception e = { _DOMAIN, FERR(tgamma) };
                    if (_matherr(&e)) return e.retval;
                }
#endif
                __feraise(FE_INVALID);
                return FCONST(nan);
            }

        default:
            /* result finite, may fall through */
            if (x < -FNAME(gamma_big))
            {
                /* large negative, use reflection relation */
                FTYPE z = x;

                y = x - y;
                FNAME(fpint)(&z, -1);
                if (x - z < FLIT(-1.0))
                    y = -y;  /* floor(x) is odd negative integer */
                z = pi / (-x * FNAME(sin)(pi * y, 0));
                x = -FNAME(lgamma)(-x);
                FNAME(exp)(&x, z, 0);
                return z;
            }
            else if (x <= FNAME(gamma_big))
            {
                return FNAME(tgamma)(x);
            }
            else
            {
                /* large positive, exponentiate lgamma(x) */
                x = FNAME(lgamma)(x);
                FNAME(exp)(&x, FLIT(1.0), 0);
                return x;
            }
    }
}

