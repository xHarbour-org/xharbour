/****************************************************************************
 *                                                                          *
 * File    : xxtanh.h                                                       *
 *                                                                          *
 * Purpose : Common tanh[fl] functionality.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-30  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* coefficients */
#if FBITS <= 24
#define CPOLY(x)    (c[0])
#define SPOLY(x)    (s[0])

static const FTYPE c[] = {  /* 3rd-order, |x| < ln(2)/2 (100.0%) */
    FLIT( 0.3996812880),
};
static const FTYPE s[] = {
    FLIT( 0.0663519281),
};

#elif FBITS <= 57
#define CPOLY(x)    (c[0] * x + c[1])
#define SPOLY(x)    ((s[0] * x + s[1]) * x + s[2])

static const FTYPE c[] = {  /* 5th-order, |x| < ln(2)/2 (100.0%) */
    FLIT( 0.01750072318694301863),
    FLIT( 0.44846583775713999745),
};
static const FTYPE s[] = {
    FLIT(-0.00000638332552609522),
    FLIT( 0.00134544393444808061),
    FLIT( 0.11513250442380779058),
};

#elif FBITS <= 71
#define CPOLY(x)    ((c[0] * x + c[1]) * x + c[2])
#define SPOLY(x)    ((s[0] * x + s[1]) * x + s[2])

static const FTYPE c[] = {  /* 6th-order, |x| < ln(2)/2 (100.0%) */
    FLIT( 0.00020696020152007992945634),
    FLIT( 0.02330329902739970135468429),
    FLIT( 0.46152333099254421099513332),
};
static const FTYPE s[] = {
    FLIT( 0.0000073840231396924596636),
    FLIT( 0.0027955220298849456025345),
    FLIT( 0.1281899976592108777990220),
};

#elif FBITS <= 125
#define CPOLY(x)    ((((c[0] * x + c[1]) * x + c[2]) * x + c[3]) * x + c[4])
#define SPOLY(x)    ((((s[0] * x + s[1]) * x + s[2]) * x + s[3]) * x + s[4])

static const FTYPE c[] = {  /* 9th-order, |x| < ln(2)/2 (100.0%) */
    FLIT( 0.00000000479572627136582713642655232360907),
    FLIT( 0.00000327477333679019896114550152093706361),
    FLIT( 0.00055030065216154133882249297095438191102),
    FLIT( 0.03007356026311641196754358517430911119925),
    FLIT( 0.47618701728773560854179734623107874650052),
};
static const FTYPE s[] = {
    FLIT( 0.0000000000726306330285737396220950671593),
    FLIT( 0.0000001559084838092647986147092019374643),
    FLIT( 0.0000491289012335169012460234429776073008),
    FLIT( 0.0046778878338712091202778030972754230619),
    FLIT( 0.1428536839544022752084640128977454362166),
};
#else
#error tanh has insufficient precision
#endif

static const FTYPE ln2by2 = FLIT(0.34657359027997265470861606072908827);

/* compute tanh(x) */
FTYPE __cdecl (FFUN(tanh))(FTYPE x)
{
    short neg;

    switch (FNAME(fptest)(&x))
    {
        /* test for special codes */
        case FP_NAN:
            return x;

        case FP_INFINITE:
            return FISNEG(x) ? FLIT(-1.0) : FLIT(1.0);

        case 0:
            return x;

        default:  /* finite */
            if (x < FLIT(0.0))
                x = -x, neg = 1;
            else
                neg = 0;

            if (x < FCONST(rteps))
                ;  /* x tiny */
            else if (x < ln2by2)
            {
                /* |x| < ln(2)/2 */
                const FTYPE z = x * x;
                const FCOMP ch = FLIT(1.0) + z * CPOLY(z);

                x += x * z * SPOLY(z);
                x /= ch;
            }
            else if (FNAME(exp)(&x, FLIT(1.0), 0) < 0)
                x = FLIT(1.0) - FLIT(2.0) / (x * x + FLIT(1.0));
            else
                x = FLIT(1.0);  /* x large */

            return (neg) ? -x : x;
    }
}

