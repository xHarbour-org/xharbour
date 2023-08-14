/****************************************************************************
 *                                                                          *
 * File    : nexttowardl.c                                                  *
 *                                                                          *
 * Purpose : nexttowardl function (IEEE version) [new C99].                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           03-06-27  Added call to __matherr().                           *
 *                                                                          *
 ****************************************************************************/

#include <fenv.h>
#include "xmath.h"

/* compute next value after x going toward y */
long double __cdecl (nexttowardl)(long double x, long double y)
{
    unsigned short *const px = (unsigned short *)&x;

    if (__fptestl(&x) > 0 || __fptestl(&y) == FP_NAN || x == y)
        ;
    else if (x < y && x > 0.0L || y < x && x < 0.0L)
    {
        /* increment magnitude of x */
        /* assume IEEE 754 8 byte */
        if ((++px[_D3] & 0xffff) == 0
            && (++px[_D2] & 0xffff) == 0
            && (++px[_D1] & 0xffff) == 0
            && (++px[_D0] & ~_DSIGN) == _DMAX << _DOFF)
        {
            /* raise overflow and inexact */
#ifdef _USE_MATHERR
            {
                struct _exception e = { _OVERFLOW, "nexttowardl" };
                if (_matherr(&e)) return e.retval;
            }
#endif
            __feraise(FE_OVERFLOW);
            __feraise(FE_INEXACT);
        }
        /* assume IEEE 754 10 byte */
//      if ((++px[_L4] & 0xffff) == 0
//          && (++px[_L3] & 0xffff) == 0
//          && (++px[_L2] & 0xffff) == 0
//          && (++px[_L1] & 0xffff) == 0)
//          if ((++px[_L0] & _LMASK) < _LMAX)
//              px[_L1] = 0x8000;  /* replace leading bit */
//          else
//          {
//              /* raise overflow and inexact */
//              __feraise(FE_OVERFLOW);
//              __feraise(FE_INEXACT);
//          }
    }
    else
    {
        /* decrement magnitude of x */
        /* assume IEEE 754 8 byte */
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
        /* assume IEEE 754 10 byte */
//      if (x == 0.0)
//      {
//          /* change zero to tiny of opposite sign */
//          px[_L4] = 1;
//          px[_L0] ^= _DSIGN;
//      }
//      else if ((--px[_L4] & 0xffff) == 0xffff
//          && (--px[_L3] & 0xffff) == 0xffff
//          && (--px[_L2] & 0xffff) == 0xffff)
//      {
//          /* decrement msw and renormalize as needed */
//          if (--px[_L1] < 0x8000 && (px[_L0] & _LMASK) != 0)
//              px[_L1] = 0xffff, --px[_L0];
//      }
        if ((px[_L0] & _LMASK) == 0)
        {
            /* zero or denormalized, raise underflow and inexact */
#ifdef _USE_MATHERR
            {
                struct _exception e = { _UNDERFLOW, "nexttowardl" };
                if (_matherr(&e)) return e.retval;
            }
#endif
            __feraise(FE_UNDERFLOW);
            __feraise(FE_INEXACT);
        }
    }

    return x;
}

