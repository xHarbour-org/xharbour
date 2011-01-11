/****************************************************************************
 *                                                                          *
 * File    : xxfabs.h                                                       *
 *                                                                          *
 * Purpose : Common fabs[fl] functionality.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* compute |x| */
FTYPE __cdecl (FFUN(fabs))(FTYPE x)
{
    switch (FNAME(fptest)(&x))
    {
        /* test for special codes */
        case FP_NAN:
            return x;

        case FP_INFINITE:
            return FCONST(inf);

        case 0:
            return FLIT(0.0);

        default:  /* finite */
            return (x < FLIT(0.0)) ? -x : x;
    }
}

