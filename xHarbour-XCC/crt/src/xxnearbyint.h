/****************************************************************************
 *                                                                          *
 * File    : xxnearbyint.h                                                  *
 *                                                                          *
 * Purpose : Common nearbyint[fl] functionality.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <fenv.h>
#include "xmath.h"

/* round x according to current mode, don't raise inexact */
FTYPE __cdecl FFUN(nearbyint)(FTYPE x)
{
    switch (FNAME(fptest)(&x))
    {
        /* test for special codes */
        case FP_NAN:
        case FP_INFINITE:
        case 0:
            return x;

        default:  /* finite fraction */
            switch (fegetround())
            {
                /* round according to current mode */
                case FE_DOWNWARD:
                    if (x < FLIT(0.0))
                        x -= FLIT(1.0);
                    break;

                case FE_TONEAREST:
                    if (x < FLIT(0.0))
                        x -= FLIT(0.5);
                    else
                        x += FLIT(0.5);
                    break;

                case FE_TOWARDZERO:
                    break;

                case FE_UPWARD:
                    if (FLIT(0.0) < x)
                        x += FLIT(1.0);
                    break;
            }

            FNAME(fpint)(&x, 0);
            return x;
    }
}

