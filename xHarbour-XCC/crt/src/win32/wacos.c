/****************************************************************************
 *                                                                          *
 * File    : wacos.c                                                        *
 *                                                                          *
 * Purpose : acos function.                                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-06-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

extern double _Ieee754_acos(double);

/* compute acos(x) */
double __cdecl (acos)(double x)
{
    double z;

    z = _Ieee754_acos(x);
    if (!isnan(x) && fabs(x) > 1.0)
    {
        errno = EDOM;
        return __nan._D;
    }

    return z;
}

