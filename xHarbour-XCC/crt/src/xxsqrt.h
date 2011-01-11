/****************************************************************************
 *                                                                          *
 * File    : xxsqrt.h                                                       *
 *                                                                          *
 * Purpose : Common sqrt[fl] functionality.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-30  Created                                              *
 *           03-06-27  Added call to __matherr().                           *
 *                                                                          *
 ****************************************************************************/

#include <fenv.h>
#include "xmath.h"

/* compute x^(1/2) */
FTYPE __cdecl (FFUN(sqrt))(FTYPE x)
{
    short xexp;
    FTYPE y;

    switch (FNAME(fpunscale)(&xexp, &x))
    {
        /* test for special codes */
        case FP_NAN:
            return x;

        case 0:
            return FLIT(0.0);

        case FP_INFINITE:
            if (!FISNEG(x))
                return x;  /* INF */

        default:  /* -INF or finite */
            if (FISNEG(x))
            {
                /* sqrt undefined for reals */
#ifdef _USE_MATHERR
                {
                    struct _exception e = { _DOMAIN, FERR(sqrt) };
                    if (_matherr(&e)) return e.retval;
                }
#endif
                __feraise(FE_INVALID);
                return FCONST(nan);
            }

            if ((unsigned int)xexp & 1)
                x *= FLIT(2.0), --xexp;

            y = (FLIT(-0.09977) * x + FLIT(0.71035)) * x + FLIT(0.38660);  /* 6 bits */
            y += x / y;
            y = FLIT(0.25) * y + x / y;  /* 27 bits */

#if FBITS <= 27
#elif FBITS <= 56
            y = FLIT(0.5) * (y + x / y);  /* 56 bits */
#elif FBITS <= 113
            y += x / y;
            y = FLIT(0.25) * y + x / y;  /* 113 bits */
#else
#error sqrt has insufficient precision
#endif

            FNAME(fpscale)(&y, xexp / 2);
            return y;
    }
}

