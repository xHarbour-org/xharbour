/****************************************************************************
 *                                                                          *
 * File    : xxlgamma.h                                                     *
 *                                                                          *
 * Purpose : Common lgamma[fl] functionality.                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *           03-06-27  Added call to __matherr().                           *
 *                                                                          *
 ****************************************************************************/

#include <fenv.h>
#include "xmath.h"

FTYPE (FNAME(lgamma))(FTYPE x);
FTYPE (FNAME(tgamma))(FTYPE x);

extern FTYPE FNAME(gamma_big);

static const FTYPE pi = FLIT(3.14159265358979323846264338327950287);

/* compute ln(gamma(x)) */
FTYPE __cdecl (FFUN(lgamma))(FTYPE x)
{
    FTYPE y = x;

    switch (FNAME(fpint)(&y, 0))
    {
        /* test for special codes */
        case FP_NAN:
            return x;

        case FP_INFINITE:
            return FCONST(nan);

        case 0:
            if (x <= FLIT(0.0))
            {
                /* x is zero or a negative integer, huge */
#ifdef _USE_MATHERR
                {
                    struct _exception e = { _OVERFLOW, FERR(lgamma) };
                    if (_matherr(&e)) return e.retval;
                }
#endif
                __feraise(FE_DIVBYZERO);
                return FCONST(inf);
            }

        default:
            /* result finite, may fall through */
            if (x < -FNAME(gamma_big))
                return FNAME(log)(pi / (-x * FNAME(sin)(pi * (y - x), 0)), 0) - FNAME(lgamma)(-x);
            else if (x <= FNAME(gamma_big))
                return FNAME(log)(FNAME(tgamma)(x), 0);
            else
                return FNAME(lgamma)(x);
    }
}

