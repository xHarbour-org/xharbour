/****************************************************************************
 *                                                                          *
 * File    : xxatan.h                                                       *
 *                                                                          *
 * Purpose : Common atan[fl] functionality.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* compute atan(x) */
FTYPE __cdecl (FFUN(atan))(FTYPE x)
{
    static const FTYPE piby2 = FLIT(1.5707963267948966192313216916397515);
    unsigned short hex;

    switch (FNAME(fptest)(&x))
    {
        /* test for special codes */
        case FP_NAN:
        case 0:
            return x;

        case FP_INFINITE:
            return FISNEG(x) ? -piby2 : piby2;

        default:  /* finite */
            if (x < FLIT(0.0))
                x = -x, hex = 0x8;
            else
                hex = 0x0;

            if (FLIT(1.0) < x)
                x = FLIT(1.0) / x, hex ^= 0x2;

            return FNAME(atan)(x, hex);
    }
}

