/****************************************************************************
 *                                                                          *
 * File    : xxscalbln.h                                                    *
 *                                                                          *
 * Purpose : Common scalbln[fl] functionality.                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-30  Created                                              *
 *           03-06-27  Added call to __matherr().                           *
 *                                                                          *
 ****************************************************************************/

#include <float.h>
#include <fenv.h>
#include "xmath.h"

#if FLT_RADIX != 2
#error correct only for FLT_RADIX == 2
#endif

/* scale x by 2^xexp */
FTYPE __cdecl FFUN(scalbln)(FTYPE x, long xexp)
{
    if (xexp != 0 && FNAME(fptest)(&x) < 0)
    {
        switch (FNAME(fpscale)(&x, xexp))
        {
            /* report over/underflow */
            case 0:
#ifdef _USE_MATHERR
                {
                    struct _exception e = { _UNDERFLOW, FERR(scalbln) };
                    if (_matherr(&e)) return e.retval;
                }
#endif
                __feraise(FE_UNDERFLOW);
                break;

            case FP_INFINITE:
#ifdef _USE_MATHERR
                {
                    struct _exception e = { _OVERFLOW, FERR(scalbln) };
                    if (_matherr(&e)) return e.retval;
                }
#endif
                __feraise(FE_OVERFLOW);
                break;
        }
    }

    return x;
}

