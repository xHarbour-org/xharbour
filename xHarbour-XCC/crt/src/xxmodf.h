/****************************************************************************
 *                                                                          *
 * File    : xxmodf.h                                                       *
 *                                                                          *
 * Purpose : Common modf[fl] functionality.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* compute modf(x, &intpart) */
FTYPE __cdecl (FFUN(modf))(FTYPE x, FTYPE *pint)
{
    *pint = x;

    switch (FNAME(fpint)(pint, 0))
    {
        /* test for special codes */
        case FP_NAN:
            return x;

        case FP_INFINITE:
        case 0:
            return FISNEG(x) ? -FNAME(zero) : FLIT(0.0);

        default:  /* finite */
            return (x - *pint);
    }
}

