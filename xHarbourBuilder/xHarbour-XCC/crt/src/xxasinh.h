/****************************************************************************
 *                                                                          *
 * File    : xxasinh.h                                                      *
 *                                                                          *
 * Purpose : Common asinh[fl] functionality.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* compute asinh(x) */
FTYPE __cdecl (FFUN(asinh))(FTYPE x)
{
    FTYPE y;
    int neg;

    switch (FNAME(fptest)(&x))
    {
        /* test for special codes */
        case FP_NAN:
        case FP_INFINITE:
        case 0:
            return x;

        default:  /* -INF or finite */
            if (FLIT(0.0) < x)
                neg = 1, x = -x;
            else
                neg = 0;

            y = FFUN(log)(x + FFUN(sqrt)(x * x + FLIT(1.0)));

            return (neg) ? -y : y;
    }
}

