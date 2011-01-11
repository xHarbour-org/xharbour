/****************************************************************************
 *                                                                          *
 * File    : _fpunscalef.c                                                  *
 *                                                                          *
 * Purpose : __fpunscalef function -- IEEE 754 version.                     *
 *                                                                          *
 * Comment : Combination of __fptestf() and frexp().                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* separate *px to 1/2 <= |frac| < 1 and 2^*pex */
short __fpunscalef(short *pex, float *px)
{
    unsigned short *ps = (unsigned short *)px;
    short xchar = (ps[_F0] & _FMASK) >> _FOFF;

    if (xchar == _FMAX)
    {
        /* NaN or INF */
        *pex = 0;
        return ((ps[_F0] & _FFRAC) != 0 || ps[_F1] != 0) ? FP_NAN : FP_INFINITE;
    }
    else if (xchar > 0 || (xchar = __fpnormf(ps)) <= 0)
    {
        /* finite, reduce to [1/2, 1) */
        ps[_F0] = ps[_F0] & ~_FMASK | _FBIAS << _FOFF;
        *pex = xchar - _FBIAS;
        return FP_NORMAL;
    }
    else
    {
        /* zero */
        *pex = 0;
        return 0;
    }
}

