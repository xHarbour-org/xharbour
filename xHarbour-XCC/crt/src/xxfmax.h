/****************************************************************************
 *                                                                          *
 * File    : xxfmax.h                                                       *
 *                                                                          *
 * Purpose : Common fmax[fl] functionality.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* compute larger of x and y */
FTYPE __cdecl (FFUN(fmax))(FTYPE x, FTYPE y)
{
    if (FNAME(fptest)(&x) == FP_NAN)
        return y;
    else if (FNAME(fptest)(&y) == FP_NAN)
        return x;
    else if (x < y)
        return y;
    else
        return x;
}

