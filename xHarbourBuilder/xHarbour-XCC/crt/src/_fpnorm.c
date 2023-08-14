/****************************************************************************
 *                                                                          *
 * File    : _fpnorm.c                                                      *
 *                                                                          *
 * Purpose : __fpnorm function -- IEEE 754 version.                         *
 *                                                                          *
 * Comment : In the IEEE 754 representation, a normalized value has a       *
 *           non-zero characteristic and an implicit fraction bit to        *
 *           the left of the most-significant fraction bit that is          *
 *           represented. Gradual underflow is signaled by a zero           *
 *           characteristic and a nonzero fraction with no implicit         *
 *           leading bit. Both these forms must be converted to a           *
 *           normalized fraction in the range [0.5, 1.0), accompanied       *
 *           by the appropriate binary exponent.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* normalize double fraction */
short __fpnorm(unsigned short *ps)
{
    unsigned short sign = ps[_D0] & _DSIGN;
    short xchar;

    xchar = 1;
    if ((ps[_D0] &= _DFRAC) != 0 || ps[_D1] || ps[_D2] || ps[_D3])
    {
        /* nonzero, scale */
        for (; ps[_D0] == 0; xchar -= 16)
        {
            /* shift left by 16 */
            ps[_D0] = ps[_D1], ps[_D1] = ps[_D2];
            ps[_D2] = ps[_D3], ps[_D3] = 0;
        }
        for (; ps[_D0] < 1<<_DOFF; --xchar)
        {
            /* shift left by 1 */
            ps[_D0] = ps[_D0] << 1 | ps[_D1] >> 15;
            ps[_D1] = ps[_D1] << 1 | ps[_D2] >> 15;
            ps[_D2] = ps[_D2] << 1 | ps[_D3] >> 15;
            ps[_D3] <<= 1;
        }
        for (; ps[_D0] >= 1<<_DOFF+1; ++xchar)
        {
            /* shift right by 1 */
            ps[_D3] = ps[_D3] >> 1 | ps[_D2] << 15;
            ps[_D2] = ps[_D2] >> 1 | ps[_D1] << 15;
            ps[_D1] = ps[_D1] >> 1 | ps[_D0] << 15;
            ps[_D0] >>= 1;
        }
        ps[_D0] &= _DFRAC;
    }
    ps[_D0] |= sign;
    return xchar;
}

