/****************************************************************************
 *                                                                          *
 * File    : _fpscale.c                                                     *
 *                                                                          *
 * Purpose : __fpscale function -- IEEE 754 version.                        *
 *                                                                          *
 * Comment : The operation is essentially the reverse of _Dnorm.            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* scale *px by 2^xexp with checking */
short __fpscale(double *px, long lexp)
{
    unsigned short *ps = (unsigned short *)px;
    short xchar = (short)((ps[_D0] & _DMASK) >> _DOFF);

    if (xchar == _DMAX)
        return ((ps[_D0] & _DFRAC) != 0 || ps[_D1] != 0 || ps[_D2] != 0 || ps[_D3] != 0) ? FP_NAN : FP_INFINITE;
    else if (xchar == 0 && (xchar = __fpnorm(ps)) > 0)
        return 0;

    lexp += xchar;
    if (lexp >= _DMAX)
    {
        /* overflow, return +/-INF */
        *px = (ps[_D0] & _DSIGN) ? -__inf._Double : __inf._Double;
        return FP_INFINITE;
    }
    else if (lexp > 0)
    {
        /* finite result, repack */
        ps[_D0] = ps[_D0] & ~_DMASK | (short)lexp << _DOFF;
        return FP_NORMAL;
    }
    else
    {
        /* denormalized, scale */
        unsigned short sign = ps[_D0] & _DSIGN;

        ps[_D0] = (unsigned short)(1<<_DOFF | ps[_D0] & _DFRAC);
        if (--lexp < -(48+_DOFF))
        {
            /* underflow, return +/-0 */
            ps[_D0] = sign, ps[_D1] = 0;
            ps[_D2] = 0, ps[_D3] = 0;
            return 0;
        }
        else
        {
            /* nonzero, align fraction */
            short xexp;

            for (xexp = (short)lexp; xexp <= -16; xexp += 16)
            {
                /* scale by words */
                ps[_D3] = ps[_D2], ps[_D2] = ps[_D1];
                ps[_D1] = ps[_D0], ps[_D0] = 0;
            }
            if ((xexp = -xexp) != 0)
            {
                /* scale by bits */
                ps[_D3] = ps[_D3] >> xexp | ps[_D2] << (16 - xexp);
                ps[_D2] = ps[_D2] >> xexp | ps[_D1] << (16 - xexp);
                ps[_D1] = ps[_D1] >> xexp | ps[_D0] << (16 - xexp);
                ps[_D0] >>= xexp;
            }
            ps[_D0] |= sign;
            return FP_NORMAL;
        }
    }
}

