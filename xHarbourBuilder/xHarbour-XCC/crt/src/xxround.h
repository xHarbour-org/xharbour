/****************************************************************************
 *                                                                          *
 * File    : xxround.h                                                      *
 *                                                                          *
 * Purpose : Common round[fl] functionality.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-30  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* round x to nearest */
FTYPE __cdecl FFUN(round)(FTYPE x)
{
    switch (FNAME(fptest)(&x))
    {
        /* test for special codes */
        case FP_NAN:
        case FP_INFINITE:
        case 0:
            return x;

        default:  /* finite fraction */
            if (x < FLIT(0.0))
                x -= FLIT(0.5);
            else
                x += FLIT(0.5);

            FNAME(fpint)(&x, 0);
            return x;
    }
}

