/****************************************************************************
 *                                                                          *
 * File    : xxceil.h                                                       *
 *                                                                          *
 * Purpose : Common ceil[fl] functionality.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* compute ceil(x) */
FTYPE __cdecl (FFUN(ceil))(FTYPE x)
{
    FTYPE y = x;

    return (FNAME(fpint)(&y, 0) < 0 && FLIT(0.0) < x) ? y + FLIT(1.0) : y;
}

