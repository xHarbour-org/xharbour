/****************************************************************************
 *                                                                          *
 * File    : xxldexp.h                                                      *
 *                                                                          *
 * Purpose : Common ldexp[fl] functionality.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *           03-06-27  Added call to __matherr().                           *
 *                                                                          *
 ****************************************************************************/

#include <fenv.h>
#include "xmath.h"

/* compute ldexp(x, xexp) */
FTYPE __cdecl (FFUN(ldexp))(FTYPE x, int xexp)
{
    if (xexp != 0 && FNAME(fptest)(&x) < 0)
    {
        switch (FNAME(fpscale)(&x, xexp))
        {
            /* report over/underflow */
            case 0:
#ifdef _USE_MATHERR
                {
                    struct _exception e = { _UNDERFLOW, FERR(ldexp) };
                    if (_matherr(&e)) return e.retval;
                }
#endif
                __feraise(FE_UNDERFLOW);
                break;

            case FP_INFINITE:
#ifdef _USE_MATHERR
                {
                    struct _exception e = { _OVERFLOW, FERR(ldexp) };
                    if (_matherr(&e)) return e.retval;
                }
#endif
                __feraise(FE_OVERFLOW);
                break;
        }
    }

    return x;
}

