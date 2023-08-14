/****************************************************************************
 *                                                                          *
 * File    : xxacosh.h                                                      *
 *                                                                          *
 * Purpose : Common acosh[fl] functionality.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *           03-06-27  Added call to __matherr().                           *
 *                                                                          *
 ****************************************************************************/

#include <fenv.h>
#include "xmath.h"

/* compute acosh(x) */
FTYPE __cdecl (FFUN(acosh))(FTYPE x)
{
    switch (FNAME(fptest)(&x))
    {
        /* test for special codes */
        case FP_NAN:
            return x;

        case FP_INFINITE:
            if (!FISNEG(x))
                return x;

        case 0:
        default:  /* -INF or finite */
            if (x < FLIT(1.0))
            {
                /* defined only for positive values, 1 <= |x| */
#ifdef _USE_MATHERR
                {
                    struct _exception e = { _OVERFLOW, FERR(acosh) };
                    if (_matherr(&e)) return e.retval;
                }
#endif
                __feraise(FE_DIVBYZERO);
                return (FCONST(nan));
            }
            else if (x == FLIT(1.0))
                return FLIT(0.0);
            else
                return (FFUN(log)(x + FFUN(sqrt)((x - FLIT(1.0)) * (x + FLIT(1.0)))));
    }
}

