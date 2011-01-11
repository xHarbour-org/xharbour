/****************************************************************************
 *                                                                          *
 * File    : xxpow.h                                                        *
 *                                                                          *
 * Purpose : Common pow[fl] functionality.                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *           03-06-27  Added call to __matherr().                           *
 *                                                                          *
 ****************************************************************************/

#include <fenv.h>
#include "xmath.h"

/* compute x^y */
FTYPE __cdecl (FFUN(pow))(FTYPE x, FTYPE y)
{
    FTYPE yi = y;
    FTYPE yx, z;
    short n, xexp, zexp;
    short neg = 0;
    short errx = FNAME(fpunscale)(&xexp, &x);
    const short erry = FNAME(fpint)(&yi, 0);

    static const short shuge = FMACRO(HUGE_EXP);
    static const FTYPE dhuge = FMACRO(HUGE_EXP);
    static const FTYPE ln2 = FLIT(0.69314718055994530941723212145817658);
    static const FTYPE rthalf = FLIT(0.70710678118654752440084436210484905);

    if (erry == 0 && y == FLIT(0.0) || errx < 0 && xexp == 1 && (x == FLIT(0.5) || erry == FP_INFINITE && x == FLIT(-0.5)))
        return FLIT(1.0);  /* x^0, 1^y, (-1)^-INF, (-1)^INF */
    else if (errx >= 0 || erry > 0)
    {
        /* x == 0, INF, NAN; y == INF, NAN */
        if (errx == FP_NAN)
            return x;  /* NAN^y */
        else if (erry == FP_NAN)
            return y;  /* x^NAN */
        else if (errx == FP_INFINITE)
        {
            if (!FISNEG(x))
                return FISNEG(y) ? FLIT(0.0) : FCONST(inf);  /* INF^y */
            else if (!FISNEG(y))
                return (erry == 0 && FNAME(fpint)(&yi, -1) < 0) ? -FCONST(inf) : FCONST(inf);  /* (-INF)^y, 0 < y */
            else
                return (erry == 0 && FNAME(fpint)(&yi, -1) < 0) ? FNAME(zero) : FLIT(0.0);     /* (-INF)^y, y < 0 */
        }
        else if (erry == FP_INFINITE)
        {
            if (!FISNEG(y))
                return (xexp <= 0) ? FLIT(0.0) : FCONST(inf);  /* x^INF */
            else
                return (xexp <= 0) ? FCONST(inf) : FLIT(0.0);  /* x^-INF */
        }
        else  /* x == 0 */
        {
            if (!FISNEG(y))
                return (erry == 0 && FNAME(fpint)(&yi, -1) < 0 && FISNEG(x)) ? -FNAME(zero) : FLIT(0.0);  /* 0^y, 0 < y */
            else
            {
                /* 0^y, y < 0: report zero divide */
#ifdef _USE_MATHERR
                {
                    struct _exception e = { _OVERFLOW, FERR(pow) };
                    if (_matherr(&e)) return e.retval;
                }
#endif
                __feraise(FE_DIVBYZERO);
                return (erry == 0 && FNAME(fpint)(&yi, -1) < 0 && FISNEG(x)) ? -FCONST(inf) : FCONST(inf);  /* 0^y, y < 0 */
            }
        }
    }
    else if (FISNEG(x) && erry < 0)
    {
        /* x^y, x < 0 && y not integer: report invalid */
#ifdef _USE_MATHERR
        {
            struct _exception e = { _DOMAIN, FERR(pow) };
            if (_matherr(&e)) return e.retval;
        }
#endif
        __feraise(FE_INVALID);
        return FCONST(nan);  /* x^y, x < 0 && y not integer */
    }

    if (FLIT(0.0) < x)
        neg = 0;
    else
    {
        /* x < 0 */
        x = -x;
        neg = FNAME(fpint)(&yi, -1);  /* negate result for y negative integer */
    }

    if (x < rthalf)
        x *= FLIT(2.0), --xexp;  /* sqrt(.5) <= x <= sqrt(2) */

    n = 0, yx = FLIT(0.0);

    if (y <= -dhuge)
        zexp = (xexp < 0) ? shuge : (xexp == 0) ? 0 : -shuge;
    else if (dhuge <= y)
        zexp = (xexp < 0) ? -shuge : (xexp == 0) ? 0 : shuge;
    else
    {
        /* y*log2(x) may be reasonable */
        FTYPE dexp = xexp;
        long zl = (long)(yx = y * dexp);

        if (zl != 0)
        {
            /* form yx = y*xexp-zl carefully */
            yx = y, FNAME(fpint)(&yx, 16);
            yx = (yx * dexp - zl) + (y - yx) * dexp;
        }
        yx *= ln2;
        zexp = zl <= -shuge ? -shuge : zl < shuge ? zl : shuge;
        if ((n = (short)y) < -FMACRO(SAFE_EXP) || FMACRO(SAFE_EXP) < n)
            n = 0;
    }

    z = FLIT(1.0);  /* compute xfrac^n * 2^yx * 2^zexp */
    if (x != FLIT(1.0))
    {
        /* z *= xfrac^n */
        if ((yi = y - n) != FLIT(0.0))
            yx += FFUN(log)(x) * yi;

        if (n < 0)
            n = -n;

        for (yi = x; ; yi *= yi)
        {
            /* scale by x^2^n */
            if ((n & 1) != 0)
                z *= yi;
            if ((n >>= 1) == 0)
                break;
        }

        if (y < FLIT(0.0))
            z = FLIT(1.0) / z;
    }

    if (FNAME(exp)(&yx, z, zexp) < 0)
        ;  /* finite result */
    else if (y < FLIT(0.0))
    {
        /* __exp overflow, y < 0: report underflow */
#ifdef _USE_MATHERR
        {
            struct _exception e = { _UNDERFLOW, FERR(pow) };
            if (_matherr(&e)) return e.retval;
        }
#endif
        __feraise(FE_UNDERFLOW);
        yx = FLIT(0.0);
    }
    else
    {
        /* __exp overflow, 0 < y: report overflow */
#ifdef _USE_MATHERR
        {
            struct _exception e = { _OVERFLOW, FERR(pow) };
            if (_matherr(&e)) return e.retval;
        }
#endif
        __feraise(FE_OVERFLOW);
        yx = FCONST(inf);
    }

    return (neg != 0) ? -yx : yx;
}

