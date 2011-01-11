/****************************************************************************
 *                                                                          *
 * File    : _fpunscale.c                                                   *
 *                                                                          *
 * Purpose : __fpunscale function -- IEEE 754 version.                      *
 *                                                                          *
 * Comment : Combination of __fptest() and frexp().                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* separate *px to 1/2 <= |frac| < 1 and 2^*pex */
short __fpunscale(short *pex, double *px)
{
    unsigned short *ps = (unsigned short *)px;
    short xchar = (ps[_D0] & _DMASK) >> _DOFF;

    if (xchar == _DMAX)
    {
        /* NaN or INF */
        *pex = 0;
        return ((ps[_D0] & _DFRAC) != 0 || ps[_D1] != 0 || ps[_D2] != 0 || ps[_D3] != 0) ? FP_NAN : FP_INFINITE;
    }
    else if (xchar > 0 || (xchar = __fpnorm(ps)) <= 0)
    {
        /* finite, reduce to [1/2, 1) */
        ps[_D0] = ps[_D0] & ~_DMASK | _DBIAS << _DOFF;
        *pex = xchar - _DBIAS;
        return FP_NORMAL;
    }
    else
    {
        /* zero */
        *pex = 0;
        return 0;
    }
}

