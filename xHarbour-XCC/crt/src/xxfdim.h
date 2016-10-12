/****************************************************************************
 *                                                                          *
 * File    : xxfdim.h                                                       *
 *                                                                          *
 * Purpose : Common fdim[fl] functionality.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* compute larger of x-y and zero */
FTYPE __cdecl (FFUN(fdim))(FTYPE x, FTYPE y)
{
    if (FNAME(fptest)(&x) == FP_NAN)
        return x;
    else if (FNAME(fptest)(&y) == FP_NAN)
        return y;
    else if (x < y)
        return FLIT(0.0);
    else
        return x - y;
}

