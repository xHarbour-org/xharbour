/****************************************************************************
 *                                                                          *
 * File    : xxxexp.h                                                       *
 *                                                                          *
 * Purpose : Common _[FL]Exp functionality.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-30  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* coefficients */
#if FBITS <= 33
#define CPOLY(x)    (c[0])
#define SPOLY(x)    (s[0] * x + s[1])

static const FTYPE c[] = {  /* 3rd-order, |x| < ln(2)/2 (minimax) */
    FLIT(0.0999748594),
};
static const FTYPE s[] = {
    FLIT(0.0083208258),
    FLIT(0.4999999992),
};

#elif FBITS <= 60
#define CPOLY(x)    (c[0] * x + c[1])
#define SPOLY(x)    ((s[0] * x + s[1]) * x + s[2])

static const FTYPE c[] = {  /* 5th-order, |x| < ln(2)/2 (minimax) */
    FLIT(0.00099173235263350450),
    FLIT(0.11110779924116564678),
};
static const FTYPE s[] = {
    FLIT(0.00003304120783105597),
    FLIT(0.01388723295391837963),
    FLIT(0.49999999999999998664),
};

#elif FBITS <= 74
#define CPOLY(x)    ((c[0] * x + c[1]) * x + c[2])
#define SPOLY(x)    ((s[0] * x + s[1]) * x + s[2])

static const FTYPE c[] = {  /* 6th-order, |x| < ln(2)/2 (minimax) */
    FLIT(0.0000015021009413506901594),
    FLIT(0.0012624415588135726592198),
    FLIT(0.1136346396655614546681762),
};
static const FTYPE s[] = {
    FLIT(0.0000631107933417128455181),
    FLIT(0.0151506531661140608604973),
    FLIT(0.4999999999999999999991708),
};

#elif FBITS <= 118
#define CPOLY(x)    (((c[0] * x + c[1]) * x + c[2]) * x + c[3])
#define SPOLY(x)    ((((s[0] * x + s[1]) * x + s[2]) * x + s[3]) * x + s[4])

static const FTYPE c[] = {  /* 9th-order, |x| < ln(2)/2 (102.5%) */
    FLIT(0.0000000050996601178111255913989690351278),
    FLIT(0.0000062839134587711380261342848362075402),
    FLIT(0.0017156346619451467305600244926131415489),
    FLIT(0.1176466115124820805029461151975277929451),
};
static const FTYPE s[] = {
    FLIT(0.0000000000566531262664884418901404512823),
    FLIT(0.0000002244096566956636845481353790434277),
    FLIT(0.0001225418512858200109905907769221302052),
    FLIT(0.0171566390895743735848063909321167993553),
    FLIT(0.4999999999999999999999999999999999603141),
};
#else
#error exp has insufficient precision
#endif

#if FBITS < 24
#error exp has too much precision
#endif

static const FTYPE c1 = (FTYPE)(5814539.0 / 8388608.0);
static const FTYPE c2 = FLIT(1.1730463525082348212145817656807550e-7);
static const FTYPE hugexp = FMACRO(HUGE_EXP);
static const FTYPE invln2 = FLIT(1.4426950408889634073599246810018922);

/* compute y*e^(*px)*2^eoff, (*px) finite, |y| not huge */
short FNAME(exp)(FTYPE *px, FTYPE y, short eoff)
{
    if (*px < -hugexp || y == FLIT(0.0))
    {
        /* certain underflow */
        *px = FLIT(0.0);
        return 0;
    }
    else if (hugexp < *px)
    {
        /* certain overflow */
        *px = FCONST(inf);
        return FP_INFINITE;
    }
    else
    {
        /* xexp won't overflow */
        FTYPE g = *px * invln2;
        short xexp = (short)(g + (g < FLIT(0.0) ? - FLIT(0.5) : FLIT(0.5)));

        g = xexp;
        g = (FTYPE)((*px - g * c1) - g * c2);
        if (-FCONST(eps) < g && g < FCONST(eps))
        {
            *px = y;
        }
        else
        {
            /* g*g worth computing */
            const FTYPE z = g * g;
            const FCOMP ch = z * CPOLY(z);
            const FCOMP sh = g * SPOLY(z);

            *px = (FLIT(1.0) + (ch + sh)) / (FLIT(1.0) + (ch - sh)) * y;
        }

        return FNAME(fpscale)(px, (long)xexp + eoff);
    }
}

