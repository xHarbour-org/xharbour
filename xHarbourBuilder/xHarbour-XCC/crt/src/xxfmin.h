/****************************************************************************
 *                                                                          *
 * File    : xxfmin.h                                                       *
 *                                                                          *
 * Purpose : Common fmin[fl] functionality.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* compute larger of x and y */
FTYPE __cdecl (FFUN(fmin))(FTYPE x, FTYPE y)
{
    if (FNAME(fptest)(&x) == FP_NAN)
        return y;
    else if (FNAME(fptest)(&y) == FP_NAN)
        return x;
    else if (x < y)
        return x;
    else
        return y;
}

