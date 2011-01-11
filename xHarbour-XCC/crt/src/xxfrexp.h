/****************************************************************************
 *                                                                          *
 * File    : xxfrexp.h                                                      *
 *                                                                          *
 * Purpose : Common frexp[fl] functionality.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* compute frexp(x, &i) */
FTYPE __cdecl (FFUN(frexp))(FTYPE x, int *pexp)
{
    short binexp;

    FNAME(fpunscale)(&binexp, &x);
    *pexp = binexp;
    return x;
}

