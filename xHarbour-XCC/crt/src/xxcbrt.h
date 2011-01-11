/****************************************************************************
 *                                                                          *
 * File    : xxcbrt.h                                                       *
 *                                                                          *
 * Purpose : Common cbrt[fl] functionality.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* compute x^(1/3) */
FTYPE __cdecl (FFUN(cbrt))(FTYPE x)
{
    short xexp;
    FTYPE y;
    int fix;

    switch (FNAME(fpunscale)(&xexp, &x))
    {
        /* test for special codes */
        case FP_NAN:
        case 0:
        case FP_INFINITE:
            return x;

        default:  /* -INF or finite */
            for (fix = 0; (xexp / 3) * 3 != xexp; ++xexp)
                --fix;
            if (fix < 0)
                FNAME(fpscale)(&x, fix);
            if (!FISNEG(x))
                fix = 0;
            else
                fix = 1, x = -x;

            y = ((FLIT(0.24379) * x + FLIT(0.95807)) * x + FLIT(0.07892)) / (x + FLIT(0.27962));  /* 10 bits */
            y = FLIT(0.5) * (y + FLIT(1.5) * x / (y * y + FLIT(0.5) * x / y)); /* 30 bits */
#if   FBITS <= 30
#elif FBITS <= 92
            y = FLIT(0.5) * (y + FLIT(1.5) * x / (y * y + FLIT(0.5) * x / y)); /* 92 bits */
#elif FBITS <= 278
            y = FLIT(0.5) * (y + FLIT(1.5) * x / (y * y + FLIT(0.5) * x / y)); /* 92 bits */
            y = FLIT(0.5) * (y + FLIT(1.5) * x / (y * y + FLIT(0.5) * x / y)); /* 278 bits */
#else
#error cbrt has insufficient precision
#endif

            if (fix)
                y = -y;
            FNAME(fpscale)(&y, xexp / 3);
            return y;
    }
}

