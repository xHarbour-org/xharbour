/****************************************************************************
 *                                                                          *
 * File    : nexttoward.c                                                   *
 *                                                                          *
 * Purpose : nexttoward function (IEEE version) [new C99].                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *           03-06-27  Added call to __matherr().                           *
 *                                                                          *
 ****************************************************************************/

#include <fenv.h>
#include "xmath.h"

/* compute next value after x going toward y */
double __cdecl (nexttoward)(double x, long double y)
{
    unsigned short *const px = (unsigned short *)&x;

    if (__fptest(&x) > 0 || __fptestl(&y) == FP_NAN || x == y)
        ;
    else if (x < y && x > 0.0 || y < x && x < 0.0)
    {
        /* increment magnitude of x */
        if ((++px[_D3] & 0xffff) == 0
            && (++px[_D2] & 0xffff) == 0
            && (++px[_D1] & 0xffff) == 0
            && (++px[_D0] & ~_DSIGN) == _DMAX << _DOFF)
        {
            /* raise overflow and inexact */
#ifdef _USE_MATHERR
            {
                struct _exception e = { _OVERFLOW, "nexttoward" };
                if (_matherr(&e)) return e.retval;
            }
#endif
            __feraise(FE_OVERFLOW);
            __feraise(FE_INEXACT);
        }
    }
    else
    {
        /* decrement magnitude of x */
        if (x == 0.0)
        {
            /* change zero to tiny of opposite sign */
            px[_D3] = 1;
            px[_D0] ^= _DSIGN;
        }
        else if ((--px[_D3] & 0xffff) == 0xffff
            && (--px[_D2] & 0xffff) == 0xffff
            && (--px[_D1] & 0xffff) == 0xffff)
            --px[_D0];

        if ((px[_D0] & _DMASK) == 0)
        {
            /* zero or denormalized, raise underflow and inexact */
#ifdef _USE_MATHERR
            {
                struct _exception e = { _UNDERFLOW, "nexttoward" };
                if (_matherr(&e)) return e.retval;
            }
#endif
            __feraise(FE_UNDERFLOW);
            __feraise(FE_INEXACT);
        }
    }

    return x;
}

