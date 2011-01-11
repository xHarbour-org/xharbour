/****************************************************************************
 *                                                                          *
 * File    : xxatanh.h                                                      *
 *                                                                          *
 * Purpose : Common atanh[fl] functionality.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *           03-06-27  Added call to __matherr().                           *
 *                                                                          *
 ****************************************************************************/

#include <fenv.h>
#include "xmath.h"

/* compute atanh(x) */
FTYPE __cdecl (FFUN(atanh))(FTYPE x)
{
    FTYPE y;
    int neg;

    switch (FNAME(fptest)(&x))
    {
        /* test for special codes */
        case FP_NAN:
        case 0:
            return x;

        default:  /* -INF or finite */
            if (FLIT(0.0) < x)
                neg = 1, x = -x;
            else
                neg = 0;

            if (FLIT(1.0) < x)
            {
                /* defined only for positive values, 1 <= |x| */
#ifdef _USE_MATHERR
                {
                    struct _exception e = { _DOMAIN, FERR(atanh) };
                    if (_matherr(&e)) return e.retval;
                }
#endif
                __feraise(FE_INVALID);
                return FCONST(nan);
            }
            else if (x == FLIT(1.0))
            {
                /* defined only for positive values, 1 <= |x| */
#ifdef _USE_MATHERR
                {
                    struct _exception e = { _OVERFLOW, FERR(atanh) };
                    if (_matherr(&e)) return e.retval;
                }
#endif
                __feraise(FE_DIVBYZERO);
                return (neg) ? -FCONST(inf) : FCONST(inf);
            }
            else
            {
                /* finite, compute it */
                y = FLIT(0.5) * FFUN(log)((FLIT(1.0) + x) / (FLIT(1.0) - x));
                return (neg) ? -y : y;
            }
    }
}

