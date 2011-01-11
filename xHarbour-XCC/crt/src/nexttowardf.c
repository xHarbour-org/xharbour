/****************************************************************************
 *                                                                          *
 * File    : nexttowardf.c                                                  *
 *                                                                          *
 * Purpose : nexttowardf function (IEEE version) [new C99].                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *           03-06-27  Added call to __matherr().                           *
 *                                                                          *
 ****************************************************************************/

#include <fenv.h>
#include "xmath.h"

/* compute next value after x going toward y */
float __cdecl (nexttowardf)(float x, long double y)
{
    unsigned short *const px = (unsigned short *)&x;

    if (__fptestf(&x) > 0 || __fptestl(&y) == FP_NAN || x == y)
        ;
    else if (x < y && x > 0.0F || y < x && x < 0.0F)
    {
        /* increment magnitude of x */
        if ((++px[_F1] & 0xffff) == 0 && (++px[_F0] & ~_FSIGN) == _FMAX << _FOFF)
        {
            /* raise overflow and inexact */
#ifdef _USE_MATHERR
            {
                struct _exception e = { _OVERFLOW, "nexttowardf" };
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
        if (x == 0.0F)
        {
            /* change zero to tiny of opposite sign */
            px[_F1] = 1;
            px[_F0] ^= _FSIGN;
        }
        else if ((--px[_F1] & 0xffff) == 0xffff)
            --px[_F0];

        if ((px[_F0] & _FMASK) == 0)
        {
            /* zero or denormalized, raise underflow and inexact */
#ifdef _USE_MATHERR
            {
                struct _exception e = { _UNDERFLOW, "nexttowardf" };
                if (_matherr(&e)) return e.retval;
            }
#endif
            __feraise(FE_UNDERFLOW);
            __feraise(FE_INEXACT);
        }
    }

    return x;
}
