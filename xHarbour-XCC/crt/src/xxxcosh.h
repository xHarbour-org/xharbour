/****************************************************************************
 *                                                                          *
 * File    : xxxcosh.h                                                      *
 *                                                                          *
 * Purpose : Common _[FL]Cosh functionality.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-30  Created                                              *
 *           03-06-27  Added call to __matherr().                           *
 *                                                                          *
 ****************************************************************************/

#include <fenv.h>
#include "xmath.h"

/* compute y * cosh(x), |y| <= 1 */
FTYPE __cdecl FNAME(cosh)(FTYPE x, FTYPE y)
{
    switch (FNAME(fptest)(&x))
    {
        /* test for special codes */
        case FP_NAN:
        case FP_INFINITE:
            return x;

        case 0:
            return y;

        default:  /* finite */
            if (y == FLIT(0.0))
                return y;

            if (x < FLIT(0.0))
                x = -x;

            if (x < FNAME(xbig))
            {
                /* worth adding in exp(-x) */
                FNAME(exp)(&x, FLIT(1.0), -1);
                return y * (x + FLIT(0.25) / x);
            }
            else
            {
                /* x large, compute y*exp(x)/2 */
                switch (FNAME(exp)(&x, y, -1))
                {
                    /* report over/underflow */
                    case 0:
#ifdef _USE_MATHERR
                        {
                            struct _exception e = { _UNDERFLOW, FERR(cosh) };
                            if (_matherr(&e)) return e.retval;
                        }
#endif
                        __feraise(FE_UNDERFLOW);
                        break;

                    case FP_INFINITE:
#ifdef _USE_MATHERR
                        {
                            struct _exception e = { _OVERFLOW, FERR(cosh) };
                            if (_matherr(&e)) return e.retval;
                        }
#endif
                        __feraise(FE_OVERFLOW);
                        break;
                }
                return x;
            }
    }
}

