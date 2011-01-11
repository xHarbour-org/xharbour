/****************************************************************************
 *                                                                          *
 * File    : xxxpoly.h                                                      *
 *                                                                          *
 * Purpose : Common _[FL]Poly functionality.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-09-02  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* compute polynomial */
FTYPE FNAME(poly)(FTYPE x, const FTYPE *tab, int n)
{
    FTYPE y;

    for (y = *tab; 0 <= --n; )
        y = y * x + *++tab;

    return y;
}

