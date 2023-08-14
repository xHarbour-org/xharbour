/****************************************************************************
 *                                                                          *
 * File    : xxllround.h                                                    *
 *                                                                          *
 * Purpose : Common llround[fl] functionality.                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *           03-06-27  Added call to __matherr().                           *
 *                                                                          *
 ****************************************************************************/

#include <limits.h>
#include <fenv.h>
#include "xmath.h"

/* round x to nearest */
long long __cdecl FFUN(llround)(FTYPE x)
{
    switch (FNAME(fptest)(&x))
    {
        /* test for special codes */
        case FP_NAN:
        case FP_INFINITE:
#ifdef _USE_MATHERR
            {
                struct _exception e = { _DOMAIN, FERR(llround) };
                if (_matherr(&e)) return e.retval;
            }
#endif
            __feraise(FE_INVALID);

        case 0:
            return 0;

        default:  /* finite fraction */
            if (x < FLIT(0.0))
                x -= FLIT(0.5);
            else
                x += FLIT(0.5);

            FNAME(fpint)(&x, 0);

            if (x < (FTYPE)LLONG_MIN || (FTYPE)LLONG_MAX < x)
            {
#ifdef _USE_MATHERR
                {
                    struct _exception e = { _DOMAIN, FERR(llround) };
                    if (_matherr(&e)) return e.retval;
                }
#endif
                __feraise(FE_INVALID);
            }

            return (long long)x;
    }
}

