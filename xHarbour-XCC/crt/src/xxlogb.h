/****************************************************************************
 *                                                                          *
 * File    : xxlogb.h                                                       *
 *                                                                          *
 * Purpose : Common logb[fl] functionality.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *           03-06-27  Added call to __matherr().                           *
 *                                                                          *
 ****************************************************************************/

#include <float.h>
#include <fenv.h>
#include "xmath.h"

#if FLT_RADIX != 2
#error correct only for FLT_RADIX == 2
#endif

/* compute logb(x) */
FTYPE __cdecl FFUN(logb)(FTYPE x)
{
    short xexp;

    switch (FNAME(fpunscale)(&xexp, &x))
    {
        /* test for special codes */
        case FP_NAN:
            return x;

        case 0:
#ifdef _USE_MATHERR
            {
                struct _exception e = { _OVERFLOW, FERR(logb) };
                if (_matherr(&e)) return e.retval;
            }
#endif
            __feraise(FE_DIVBYZERO);
            return -FCONST(inf);

        case FP_INFINITE:
            return (FCONST(inf));  /* INF */

        default:  /* finite */
            return (FTYPE)--xexp;
    }
}

