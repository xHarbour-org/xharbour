/****************************************************************************
 *                                                                          *
 * File    : _fpnorml.c                                                     *
 *                                                                          *
 * Purpose : __fpnorml function -- IEEE 754 version.                        *
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
 *           01-09-10  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* not needed -- 64-bit */

#if 0
/* normalize long double fraction -- 80-bit */
short __fpnorml(unsigned short *ps)
{
    short xchar;
    unsigned short sign = ps[_L0];

    xchar = 0;
    for (ps[_L0] = 0; ps[_L0] == 0 && ps[_L1] < 0x100; xchar -= 16)
    {
        /* shift left by 16 */
        ps[_L0] = ps[_L1];
        ps[_L1] = ps[_L2], ps[_L2] = ps[_L3];
        ps[_L3] = ps[_L4], ps[_L4] = 0;
    }

    if (ps[_L0] == 0)
    {
        for (; ps[_L1] < (1U << _LOFF); --xchar)
        {
            /* shift left by 1 */
            ps[_L1] = ps[_L1] << 1 | ps[_L2] >> 15;
            ps[_L2] = ps[_L2] << 1 | ps[_L3] >> 15;
            ps[_L3] = ps[_L3] << 1 | ps[_L4] >> 15;
            ps[_L4] <<= 1;
        }
    }

    for (; ps[_L0] != 0; ++xchar)
    {
        /* shift right by 1 */
        ps[_L4] = ps[_L4] >> 1 | ps[_L3] << 15;
        ps[_L3] = ps[_L3] >> 1 | ps[_L2] << 15;
        ps[_L2] = ps[_L2] >> 1 | ps[_L1] << 15;
        ps[_L1] = ps[_L1] >> 1 | ps[_L0] << 15;
        ps[_L0] >>= 1;
    }

    ps[_L0] = sign;
    return xchar;
}
#endif

