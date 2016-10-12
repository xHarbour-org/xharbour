/****************************************************************************
 *                                                                          *
 * File    : wacosf.c                                                       *
 *                                                                          *
 * Purpose : acosf function.                                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-06-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

extern float _Ieee754_acosf(float);

/* compute acosf(x) */
float __cdecl (acosf)(float x)
{
    float z;

    z = _Ieee754_acosf(x);
    if (!isnan(x) && fabsf(x) > 1.0)
    {
        errno = EDOM;
        return __nanf._F;
    }

    return z;
}

