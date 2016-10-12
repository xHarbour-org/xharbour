/****************************************************************************
 *                                                                          *
 * File    : xxxdtent.h                                                     *
 *                                                                          *
 * Purpose : Common _[FL]Dtento functionality.                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-09-02  Created                                              *
 *           03-06-27  Added call to __matherr().                           *
 *                                                                          *
 ****************************************************************************/

#include <fenv.h>
#include "xmath.h"

/* static data */
#define NPOWS   (sizeof pows / sizeof pows[0]-1)

static const FTYPE pows[] = {
    FLIT(1e1),
    FLIT(1e2),
    FLIT(1e4),
    FLIT(1e8),
    FLIT(1e16),
    FLIT(1e32),
#if FMAXEXP >= 212
    FLIT(1e64),
#if FMAXEXP >= 425
    FLIT(1e128),
#if FMAXEXP >= 850
    FLIT(1e256),
#if FMAXEXP >= 1700
    FLIT(1e512),
#if FMAXEXP >= 3401
    FLIT(1e1024),
#if FMAXEXP >= 6803
    FLIT(1e2048),
#if FMAXEXP >= 13606
    FLIT(1e4096),
#endif /* FMAXEXP >= 13606 */
#endif /* FMAXEXP >= 6803 */
#endif /* FMAXEXP >= 3401 */
#endif /* FMAXEXP >= 1700 */
#endif /* FMAXEXP >= 850 */
#endif /* FMAXEXP >= 425 */
#endif /* FMAXEXP >= 212 */
};
static const size_t npows = NPOWS;

/* multiply y by *px with checking */
static short dmul(FTYPE *px, FTYPE y)
{
    short xexp;

    FNAME(fpunscale)(&xexp, px);
    *px *= y;
    return FNAME(fpscale)(px, xexp);
}

/* compute x * 10**n */
FTYPE FNAME(fppow10)(FTYPE x, long n)
{
    FTYPE factor;
    short errx;
    size_t i;

    if (n == 0 || x == FLIT(0.0))
        return x;

    factor = FLIT(1.0);
    if (n < 0)
    {
        /* scale down */
        unsigned long nu = -(unsigned long)n;

        for (i = 0; nu > 0 && i < npows; nu >>= 1, ++i)
            if (nu & 1) factor *= *(pows + i);

        errx = dmul(&x, FLIT(1.0) / factor);
        if (errx < 0 && nu > 0)
        {
            for (factor = FLIT(1.0) / *(pows + npows); nu > 0; --nu)
                if ((errx = dmul(&x, factor)) >= 0) break;
        }
    }
    else if (n > 0)
    {
        /* scale up */
        for (i = 0; n > 0 && i < npows; n >>= 1, ++i)
            if (n & 1) factor *= *(pows + i);

        errx = dmul(&x, factor);
            if (errx < 0 && n > 0)
            {
                for (factor = *(pows + npows); n > 0; --n)
                    if ((errx = dmul(&x, factor)) >= 0) break;
            }
    }

    if (errx == 0)
    {
#ifdef _USE_MATHERR
        {
            struct _exception e = { _UNDERFLOW, FERR(fppow10) };
            if (_matherr(&e)) return e.retval;
        }
#endif
        __feraise(FE_UNDERFLOW);
    }
    else if (errx == FP_INFINITE)
    {
#ifdef _USE_MATHERR
        {
            struct _exception e = { _OVERFLOW, FERR(fppow10) };
            if (_matherr(&e)) return e.retval;
        }
#endif
        __feraise(FE_OVERFLOW);
    }

    return x;
}

