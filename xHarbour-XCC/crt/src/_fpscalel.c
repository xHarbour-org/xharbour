/****************************************************************************
 *                                                                          *
 * File    : _fpscalel.c                                                    *
 *                                                                          *
 * Purpose : __fpscalel function -- IEEE 754 version.                       *
 *                                                                          *
 * Comment : The operation is essentially the reverse of _Dnorm.            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-10  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* scale *px by 2^lexp with checking -- 64-bit */
short __fpscalel(long double *px, long lexp)
{
    return __fpscale((double *)px, lexp);
}

#if 0
/* scale *px by 2^lexp with checking -- 80-bit */
short _LDscale(long double *px, long lexp)
{
    unsigned short *ps = (unsigned short *)px;
    short xchar = ps[_L0] & _LMASK;

    if (xchar == _LMAX)
        return ((ps[_L1] & 0x7fff) != 0 || ps[_L2] != 0 || ps[_L3] != 0 || ps[_L4] != 0) ? FP_NAN : FP_INFINITE;
    else if (xchar == 0 && ps[_L1] == 0 && ps[_L2] == 0 && ps[_L3] == 0 && ps[_L4] == 0)
        return 0;

    lexp += xchar + _LDnorm(ps);
    if (lexp >= _LMAX)
    {
        /* overflow, return +/-INF */
        *px = ps[_L0] & _LSIGN ? -__infl._Long_double : __infl._Long_double;
        return FP_INFINITE;
    }
    else if (lexp >= 0)
    {
        /* finite result, repack */
        ps[_L0] = ps[_L0] & _LSIGN | (short)lexp;
        return FP_NORMAL;
    }
    else
    {
        /* denormalized, scale */
        ps[_L0] &= _LSIGN;
        if (lexp <= -64)
        {
            /* underflow, return +/-0 */
            ps[_L1] = 0, ps[_L2] = 0;
            ps[_L3] = 0, ps[_L4] = 0;
            return 0;
        }
        else
        {
            /* nonzero, align fraction */
            short xexp;

            for (xexp = lexp; xexp <= -16; xexp += 16)
            {
                /* scale by words */
                ps[_L4] = ps[_L3], ps[_L3] = ps[_L2];
                ps[_L2] = ps[_L1], ps[_L1] = 0;
            }

            if ((xexp = -xexp) != 0)
            {
                /* scale by bits */
                ps[_L4] = ps[_L4] >> xexp | ps[_L3] << 16 - xexp;
                ps[_L3] = ps[_L3] >> xexp | ps[_L2] << 16 - xexp;
                ps[_L2] = ps[_L2] >> xexp | ps[_L1] << 16 - xexp;
                ps[_L1] >>= xexp;
            }
            return FP_NORMAL;
        }
    }
}
#endif

