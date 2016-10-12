/****************************************************************************
 *                                                                          *
 * File    : xxfloor.h                                                      *
 *                                                                          *
 * Purpose : Common floor[fl] functionality.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* compute floor(x) */
FTYPE __cdecl (FFUN(floor))(FTYPE x)
{
    FTYPE y = x;

    return (FNAME(fpint)(&y, 0) < 0 && x < FLIT(0.0)) ? y - FLIT(1.0) : y;
}

