/****************************************************************************
 *                                                                          *
 * File    : xxxfpcomp.h                                                    *
 *                                                                          *
 * Purpose : Common _[FL]Fpcomp functionality [new C99].                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-09-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* compare x and y */
int __cdecl FNAME(fpcomp)(FTYPE x, FTYPE y)
{
    if (FNAME(fptest)(&x) == FP_NAN || FNAME(fptest)(&y) == FP_NAN)
        return 0;
    else if (x < y)
        return _FP_LT;
    else if (y < x)
        return _FP_GT;
    else
        return _FP_EQ;
}

