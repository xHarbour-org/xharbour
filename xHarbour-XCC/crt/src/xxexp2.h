/****************************************************************************
 *                                                                          *
 * File    : xxexp2.h                                                       *
 *                                                                          *
 * Purpose : Common exp2[fl] functionality.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *           03-06-27  Added call to __matherr().                           *
 *                                                                          *
 ****************************************************************************/

#include <fenv.h>
#include "xmath.h"

static const FTYPE ln2 = FLIT(0.69314718055994530941723212145817658);

/* compute 2^x */
FTYPE __cdecl (FFUN(exp2))(FTYPE x)
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
            x *= ln2;
            switch (FNAME(exp)(&x, FLIT(1.0), 0))
            {
                /* report over/underflow */
                case 0:
#ifdef _USE_MATHERR
                    {
                        struct _exception e = { _UNDERFLOW, FERR(exp2) };
                        if (_matherr(&e)) return e.retval;
                    }
#endif
                    __feraise(FE_UNDERFLOW);
                    break;

                case FP_INFINITE:
#ifdef _USE_MATHERR
                    {
                        struct _exception e = { _OVERFLOW, FERR(exp2) };
                        if (_matherr(&e)) return e.retval;
                    }
#endif
                    __feraise(FE_OVERFLOW);
            }
            return x;
    }
}

