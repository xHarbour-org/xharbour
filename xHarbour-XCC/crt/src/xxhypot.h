/****************************************************************************
 *                                                                          *
 * File    : xxhypot.h                                                      *
 *                                                                          *
 * Purpose : Common hypot[fl] functionality.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *           03-06-27  Added call to __matherr().                           *
 *                                                                          *
 ****************************************************************************/

#include <fenv.h>
#include "xmath.h"

/* compute hypotenuse */
FTYPE __cdecl (FFUN(hypot))(FTYPE x, FTYPE y)
{
    int zexp;
    FTYPE z = FNAME(hypot)(x, y, &zexp);

    if (zexp != 0)
    {
        switch (FNAME(fpscale)(&z, zexp))
        {
            /* report over/underflow */
            case 0:
#ifdef _USE_MATHERR
                {
                    struct _exception e = { _UNDERFLOW, FERR(hypot) };
                    if (_matherr(&e)) return e.retval;
                }
#endif
                __feraise(FE_UNDERFLOW);
                break;

            case FP_INFINITE:
#ifdef _USE_MATHERR
                {
                    struct _exception e = { _OVERFLOW, FERR(hypot) };
                    if (_matherr(&e)) return e.retval;
                }
#endif
                __feraise(FE_OVERFLOW);
                break;
        }
    }

    return z;
}

