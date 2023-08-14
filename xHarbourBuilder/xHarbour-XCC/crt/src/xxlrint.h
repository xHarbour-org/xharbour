/****************************************************************************
 *                                                                          *
 * File    : xxlrint.h                                                      *
 *                                                                          *
 * Purpose : Common lrint[fl] functionality.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *           03-06-27  Added call to __matherr().                           *
 *                                                                          *
 ****************************************************************************/

#include <fenv.h>
#include <limits.h>
#include "xmath.h"

/* round x according to current mode, raising inexact */
long __cdecl FFUN(lrint)(FTYPE x)
{
    switch (FNAME(fptest)(&x))
    {
        /* test for special codes */
        case FP_NAN:
        case FP_INFINITE:
#ifdef _USE_MATHERR
            {
                struct _exception e = { _DOMAIN, FERR(lrint) };
                if (_matherr(&e)) return e.retval;
            }
#endif
            __feraise(FE_INVALID);

        case 0:
            return 0;

        default:  /* finite fraction */
#ifdef _USE_MATHERR
            {
                struct _exception e = { _PLOSS, FERR(lrint) };
                if (_matherr(&e)) return e.retval;
            }
#endif
            __feraise(FE_INEXACT);
            switch (fegetround())
            {
                /* round according to current mode */
                case FE_DOWNWARD:
                    if (x < FLIT(0.0))
                        x -= FLIT(1.0);
                    break;

                case FE_TONEAREST:
                    if (x < FLIT(0.0))
                        x -= FLIT(0.5);
                    else
                        x += FLIT(0.5);
                    break;

                case FE_TOWARDZERO:
                    break;

                case FE_UPWARD:
                    if (FLIT(0.0) < x)
                        x += FLIT(1.0);
                    break;
            }

            FNAME(fpint)(&x, 0);

            if (x < (FTYPE)LONG_MIN || (FTYPE)LONG_MAX < x)
            {
#ifdef _USE_MATHERR
                {
                    struct _exception e = { _DOMAIN, FERR(lrint) };
                    if (_matherr(&e)) return e.retval;
                }
#endif
                __feraise(FE_INVALID);
            }

            return (long)x;
    }
}

