/****************************************************************************
 *                                                                          *
 * File    : _fpunscalel.c                                                  *
 *                                                                          *
 * Purpose : __fpunscalel function -- IEEE 754 version.                     *
 *                                                                          *
 * Comment : Combination of __fptestl() and frexp().                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-10  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* separate *px to 1/2 <= |frac| < 1 and 2^*pex -- 64-bit */
short (__fpunscalel)(short *pex, long double *px)
{
    return __fpunscale(pex, (double *)px);
}

#if 0
/* separate *px to 1/2 <= |frac| < 1 and 2^*pex -- 80-bit */
short (__fpunscalel)(short *pex, long double *px)
{
    unsigned short *ps = (unsigned short *)px;
    short xchar = ps[_L0] & _LMASK;

    if (xchar == _LMAX)
    {
        /* NaN or INF */
        *pex = 0;
        return ((ps[_L1] & 0x7fff) != 0 || ps[_L2] != 0 || ps[_L3] != 0 || ps[_L4] != 0) ? FP_NAN : FP_INFINITE;
    }
    else if (ps[_L1] != 0 || ps[_L2] != 0 || ps[_L3] != 0 || ps[_L4] != 0)
    {
        /* finite, reduce to [1/2, 1) */
        xchar += _LDnorm(ps);
        ps[_L0] = ps[_L0] & _LSIGN | _LBIAS;
        *pex = xchar - _LBIAS;
        return FP_NORMAL;
    }
    else
    {
        /* zero */
        *pex = 0;
        return 0;
    }
}
#endif
