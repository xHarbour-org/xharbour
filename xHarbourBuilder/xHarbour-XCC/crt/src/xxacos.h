/****************************************************************************
 *                                                                          *
 * File    : xxacos.h                                                       *
 *                                                                          *
 * Purpose : Common acos[fl] functionality.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *           03-06-27  Added call to __matherr().                           *
 *                                                                          *
 ****************************************************************************/

#include <fenv.h>
#include "xmath.h"

/* compute acos(x) */
FTYPE __cdecl (FFUN(acos))(FTYPE x)
{
    static const FTYPE rthalf = FLIT(0.70710678118654752440084436210484905);
    unsigned short hex;

    switch (FNAME(fptest)(&x))
    {
        /* test for special codes */
        case FP_NAN:
            return x;

        default:  /* 0, FINITE, or INF */
            if (x < FLIT(0.0))
                x = -x, hex = 0x4;
            else
                hex = 0x2;

            if (x <= rthalf)
                return (FNAME(atan)(x / FFUN(sqrt)((FLIT(1.0) - x) * (FLIT(1.0) + x)), hex));
            else if (x <= FLIT(1.0))
                return (FNAME(atan)(FFUN(sqrt)((FLIT(1.0) - x) * (FLIT(1.0) + x)) / x, hex ^ 0x2));
            else
            {
                /* 1 < |x| */
#ifdef _USE_MATHERR
                {
                    struct _exception e = { _DOMAIN, FERR(acos) };
                    if (_matherr(&e)) return e.retval;
                }
#endif
                __feraise(FE_INVALID);
                return FCONST(nan);
            }
    }
}

