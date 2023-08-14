/****************************************************************************
 *                                                                          *
 * File    : _fpscalef.c                                                    *
 *                                                                          *
 * Purpose : __fpscalef function -- IEEE 754 version.                       *
 *                                                                          *
 * Comment : The operation is essentially the reverse of _Dnorm.            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* scale *px by 2^xexp with checking */
short __fpscalef(float *px, long lexp)
{
    unsigned short *ps = (unsigned short *)px;
    short xchar = (short)((ps[_F0] & _FMASK) >> _FOFF);

    if (xchar == _FMAX)
        return ((ps[_F0] & _FFRAC) != 0 || ps[_F1] != 0) ? FP_NAN : FP_INFINITE;
    else if (xchar == 0 && (xchar = __fpnormf(ps)) > 0)
        return 0;

    lexp += xchar;
    if (lexp >= _FMAX)
    {
        /* overflow, return +/-INF */
        *px = (ps[_F0] & _FSIGN) ? -__inff._Float : __inff._Float;
        return FP_INFINITE;
    }
    else if (lexp > 0)
    {
        /* finite result, repack */
        ps[_F0] = ps[_F0] & ~_FMASK | (short)lexp << _FOFF;
        return FP_NORMAL;
    }
    else
    {
        /* denormalized, scale */
        unsigned short sign = ps[_F0] & _FSIGN;

        ps[_F0] = (unsigned short)(1<<_FOFF | ps[_F0] & _FFRAC);
        if (--lexp < -(16+_FOFF))
        {
            /* underflow, return +/-0 */
            ps[_F0] = sign, ps[_F1] = 0;
            return 0;
        }
        else
        {
            /* nonzero, align fraction */
            short xexp = (short)lexp;
            if (xexp <= -16)
                ps[_F1] = ps[_F0], ps[_F0] = 0, xexp += 16;
            if ((xexp = -xexp) != 0)
            {
                /* scale by bits */
                ps[_F1] = ps[_F1] >> xexp | ps[_F0] << (16 - xexp);
                ps[_F0] >>= xexp;
            }
            ps[_F0] |= sign;
            return FP_NORMAL;
        }
    }
}

