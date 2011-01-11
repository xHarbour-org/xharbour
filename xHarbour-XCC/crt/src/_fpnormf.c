/****************************************************************************
 *                                                                          *
 * File    : _fpnormf.c                                                     *
 *                                                                          *
 * Purpose : __fpnormf function -- IEEE 754 version.                        *
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

/* normalize float fraction */
short __fpnormf(unsigned short *ps)
{
    unsigned short sign = ps[_F0] & _FSIGN;
    short xchar;

    xchar = 1;
    if ((ps[_F0] &= _FFRAC) != 0 || ps[_F1])
    {
        /* nonzero, scale */
        if (ps[_F0] == 0)
            ps[_F0] = ps[_F1], ps[_F1] = 0, xchar -= 16;

        for (; ps[_F0] < 1<<_FOFF; --xchar)
        {
            /* shift left by 1 */
            ps[_F0] = ps[_F0] << 1 | ps[_F1] >> 15;
            ps[_F1] <<= 1;
        }
        for (; ps[_F0] >= 1<<(_FOFF+1); ++xchar)
        {
            /* shift right by 1 */
            ps[_F1] = ps[_F1] >> 1 | ps[_F0] << 15;
            ps[_F0] >>= 1;
        }
        ps[_F0] &= _FFRAC;
    }
    ps[_F0] |= sign;
    return xchar;
}

