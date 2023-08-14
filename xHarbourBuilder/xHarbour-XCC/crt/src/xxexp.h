/****************************************************************************
 *                                                                          *
 * File    : xxexp.h                                                        *
 *                                                                          *
 * Purpose : Common exp[fl] functionality.                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *           03-06-27  Added call to __matherr().                           *
 *                                                                          *
 ****************************************************************************/

#include <fenv.h>
#include "xmath.h"

/* compute e^x */
FTYPE __cdecl (FFUN(exp))(FTYPE x)
{
    switch (FNAME(fptest)(&x))
    {
        /* test for special codes */
        case FP_NAN:
            return x;

        case FP_INFINITE:
            return FISNEG(x) ? FLIT(0.0) : x;

        case 0:
            return FLIT(1.0);

        default:  /* finite */
            switch (FNAME(exp)(&x, FLIT(1.0), 0))
            {
                /* report over/underflow */
                case 0:
#ifdef _USE_MATHERR
                    {
                        struct _exception e = { _UNDERFLOW, FERR(exp) };
                        if (_matherr(&e)) return e.retval;
                    }
#endif
                    __feraise(FE_UNDERFLOW);
                    break;

                case FP_INFINITE:
#ifdef _USE_MATHERR
                    {
                        struct _exception e = { _OVERFLOW, FERR(exp) };
                        if (_matherr(&e)) return e.retval;
                    }
#endif
                    __feraise(FE_OVERFLOW);
            }
            return x;
    }
}

