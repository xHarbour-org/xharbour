/****************************************************************************
 *                                                                          *
 * File    : wacosl.c                                                       *
 *                                                                          *
 * Purpose : acosf function.                                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-06-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

extern long double _Ieee754_acosl(long double);

/* compute acosl(x) */
long double __cdecl (acosl)(long double x)
{
    long double z;

    z = _Ieee754_acosl(x);
    if (!isnan(x) && fabsl(x) > 1.0)
    {
        errno = EDOM;
        return __nanl._L;
    }

    return z;
}

