/****************************************************************************
 *                                                                          *
 * File    : xxfma.h                                                        *
 *                                                                          *
 * Purpose : Common fma[fl] functionality.                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *           03-06-27  Added call to __matherr().                           *
 *                                                                          *
 ****************************************************************************/

#include <fenv.h>
#include "xmath.h"

/* compute x*y+z */
FTYPE __cdecl (FFUN(fma))(FTYPE x, FTYPE y, FTYPE z)
{
    FTYPE xp = x, yp = y, zp = z;
    short errx, erry, errz;
    short expx, expy, expz;

    if ((errx = FNAME(fpunscale)(&expx, &xp)) == FP_NAN)
        return x;
    else if ((erry = FNAME(fpunscale)(&expy, &yp)) == FP_NAN)
        return y;
    else if ((errz = FNAME(fpunscale)(&expz, &zp)) == FP_NAN)
        return z;
    else if (errx == FP_INFINITE || erry == FP_INFINITE)
    {
        if (errx != 0 && erry != 0 && (errz != FP_INFINITE || (FISNEG(x) ^ FISNEG(y)) == FISNEG(z)))
            return (x * y + z);
        else
        {
            /* 0*INF, INF*0, INF-INF are invalid */
#ifdef _USE_MATHERR
            {
                struct _exception e = { _DOMAIN, FERR(fma) };
                if (_matherr(&e)) return e.retval;
            }
#endif
            __feraise(FE_INVALID);
            return FCONST(nan);
        }
    }
    else if (errz == FP_INFINITE || errx == 0 || erry == 0 || errz == 0)
        return (x * y + z);  /* z==INF or at least one zero */
    else
    {
        /* all finite */
        long expw = (long)expx + expy;

        if (2 * FBITS < expw - expz)
            return x * y;  /* |x*y| >> |z| */
        else if (expw - expz < -2 * FBITS)
            return z;      /* |x*y| << |z| */
        else if ((FISNEG(x) ^ FISNEG(y) ^ FISNEG(z)) == 0)
            return x * y + z;  /* x*y same sign as z */
        else
        {
            /* compute (x0+x1)*(y0+y1)+z */
            x = xp;
            FNAME(fpint)(&x, FBITS / 2);
            xp -= x;
            y = yp;
            FNAME(fpint)(&y, FBITS / 2);
            yp -= y;
            FNAME(fpscale)(&zp, expz - expw);
            zp += x * y;
            zp += x * yp;
            zp += xp * y;
            zp += xp * yp;
            FNAME(fpscale)(&zp, expw);
            return zp;
        }
    }
}

