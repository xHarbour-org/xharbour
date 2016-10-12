/****************************************************************************
 *                                                                          *
 * File    : xxxhypot.h                                                     *
 *                                                                          *
 * Purpose : Common _[FL]Hypot functionality [new C99].                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-09-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* compute hypotenuse with scale factor */
FTYPE FNAME(hypot)(FTYPE x, FTYPE y, int *pexp)
{
    short errx = FNAME(fptest)(&x);
    short erry = FNAME(fptest)(&y);

    *pexp = 0;

    if (errx == FP_INFINITE || erry == FP_INFINITE)
        return FCONST(inf);
    else if (errx == FP_NAN)
        return x;
    else if (erry == FP_NAN)
        return y;
    else
    {
        /* x and y are finite */
        FTYPE z;

        if (x < FLIT(0.0))
            x = -x;
        if (y < FLIT(0.0))
            y = -y;

        if (x < y)
        {
            /* ensure that |y| < |x| */
            FTYPE tmp = x;
            x = y;
            y = tmp;
        }

        if (x == FLIT(0.0))
            return (FLIT(0.0));  /* |(0, 0)| == 0 */

        if (FLIT(1.0) <= x)
        {
            /* scale down */
            *pexp = 2;
            x = FLIT(0.25) * x;
            y = FLIT(0.25) * y;
        }
        else
        {
            /* scale up */
            *pexp = -2;
            x = FLIT(4.0) * x;
            y = FLIT(4.0) * y;
        }

        z = x - y;
        if (z == x)
            ;  /* y unimportant */
        else if (y < z)
        {
            /* use simple approximation */
            const FTYPE qv = x / y;
            z = x + y / (qv + FFUN(sqrt)(qv * qv + FLIT(1.0)));
        }
        else
        {
            /* use 1 1/2 precision to preserve bits */
            static const FTYPE root2 = FLIT(1.4142135623730950488016887242096982);
            static const FTYPE oneplusroot2high = FLIT(2.4142);
            static const FTYPE oneplusroot2low = FLIT(0.0000135623730950488016887242096980785698);
            const FTYPE qv = z / y;
            const FTYPE rv = (qv + FLIT(2.0)) * qv;
            const FTYPE sv = rv / (root2 + FFUN(sqrt)(rv + FLIT(2.0))) + oneplusroot2low + qv + oneplusroot2high;
            z = x + y / sv;
        }

        return z;
    }
}

