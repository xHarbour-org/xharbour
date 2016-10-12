/****************************************************************************
 *                                                                          *
 * File    : xxlog1p.h                                                      *
 *                                                                          *
 * Purpose : Common log1p[fl] functionality.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *           03-06-27  Added call to __matherr().                           *
 *                                                                          *
 ****************************************************************************/

#include <fenv.h>
#include "xmath.h"

/* coefficients */
#if FBITS <= 25
#define DPOLY(x)    (((c[0] * x + c[1]) * x + c[2]) * x + c[3])
#define NPOLY(x)    (((s[0] * x + s[1]) * x + s[2]) * x + s[3])

static const FTYPE s[] = {  /* 3/3, -0.5 <= x <= 0.5 */
    FLIT( 0.0082862580),
    FLIT( 0.3394238808),
    FLIT( 1.1457993413),
    FLIT( 0.8952856678),
};

static const FTYPE c[] = {
    FLIT( 0.1198195734),
    FLIT( 0.8377145063),
    FLIT( 1.5934420741),
    FLIT( 0.8952856868),
};
#elif FBITS <= 56
#define DPOLY(x)    FNAME(poly)(x, c, sizeof(c) / sizeof(c[0]) - 1)
#define NPOLY(x)    FNAME(poly)(x, s, sizeof(s) / sizeof(s[0]) - 1)

static const FTYPE s[] = {  /* 7/7, -0.5 <= x <= 0.5 */
    FLIT( 0.00001779850800829816),
    FLIT( 0.00432795669451011355),
    FLIT( 0.08104678960868492344),
    FLIT( 0.52821082542601385338),
    FLIT( 1.60020960630104334563),
    FLIT( 2.46131342307790913559),
    FLIT( 1.86585313126099358355),
    FLIT( 0.55353634798981805335),
};

static const FTYPE c[] = {
    FLIT( 0.00102660718632528855),
    FLIT( 0.02964409301906419136),
    FLIT( 0.27549714331834000100),
    FLIT( 1.18634284793942482413),
    FLIT( 2.69844257140117600850),
    FLIT( 3.34811195970925626786),
    FLIT( 2.14262130525590250687),
    FLIT( 0.55353634798981804783),
};
#elif FBITS <= 64
#define DPOLY(x)    FNAME(poly)(x, c, sizeof(c) / sizeof(c[0]) - 1)
#define NPOLY(x)    FNAME(poly)(x, s, sizeof(s) / sizeof(s[0]) - 1)

static const FTYPE s[] = {  /* 8/8, -0.5 <= x <= 0.5 */
    FLIT( 0.0000038225734455018310945),
    FLIT( 0.0012416028282768833235456),
    FLIT( 0.0302881804937452567026993),
    FLIT( 0.2607973155880152940767411),
    FLIT( 1.0764000870084511122748414),
    FLIT( 2.3870277098351015217828930),
    FLIT( 2.9162024907827889388141810),
    FLIT( 1.8490221603463142502481203),
    FLIT( 0.4752512220093550127020889),
};

static const FTYPE c[] = {
    FLIT( 0.0002789713704964882833917),
    FLIT( 0.0102087129014558439774702),
    FLIT( 0.1213786957647595828808649),
    FLIT( 0.6817041404588054596798441),
    FLIT( 2.0913983060166200316568211),
    FLIT( 3.7108459094480262158472635),
    FLIT( 3.8011093024551664668725800),
    FLIT( 2.0866477713509917566678483),
    FLIT( 0.4752512220093550127266425),
};
#elif FBITS <= 116
#define DPOLY(x)    FNAME(poly)(x, c, sizeof(c) / sizeof(c[0]) - 1)
#define NPOLY(x)    FNAME(poly)(x, s, sizeof(s) / sizeof(s[0]) - 1)

static const FTYPE s[] = {  /* 15/15, -0.5 <= x <= 0.5 */
    FLIT( 0.00000000007434995410546471772373529),
    FLIT( 0.00000009670485068378090629490727239),
    FLIT( 0.00000826911341563446949336707491059),
    FLIT( 0.00025558444086433078162578670568468),
    FLIT( 0.00402187233780870344770217153124892),
    FLIT( 0.03765066083678360492411342897845521),
    FLIT( 0.22865823100931728024862925096111996),
    FLIT( 0.94908751694765237139404172414507152),
    FLIT( 2.77812050916907462292718510936468075),
    FLIT( 5.83129047175581499298395699263615205),
    FLIT( 8.81488897168028390466884858724265116),
    FLIT( 9.51558482265929461174163609341416323),
    FLIT( 7.15519272594171007709185861560951491),
    FLIT( 3.56007842768309190400498597079353273),
    FLIT( 1.05337550479115722011919109769019935),
    FLIT( 0.14031850406490130313986771613771979),
};

static const FTYPE c[] = {
    FLIT( 0.00000001725350114999412611311851499),
    FLIT( 0.00000201029056027027351689836368329),
    FLIT( 0.00007772526996014173239650586383931),
    FLIT( 0.00148249142214596815977888839734683),
    FLIT( 0.01658250860140098983852847114980989),
    FLIT( 0.11965000619607762635967007798714487),
    FLIT( 0.59008608068589586827260812437329204),
    FLIT( 2.06240993222801461581827218462771050),
    FLIT( 5.22017250088580982185148455392022666),
    FLIT( 9.66770463262924418631221622435635511),
    FLIT(13.09958548646525667290739114158783135),
    FLIT(12.83669577994336642473703635688079911),
    FLIT( 8.85329725205336381504858016437116766),
    FLIT( 4.07507297140659540546959254329382206),
    FLIT( 1.12353475682360787168912495575905925),
    FLIT( 0.14031850406490130313986771613771979),
};
#else
#error log1p has insufficient precision
#endif

/* compute log(1+x) */
FTYPE __cdecl FFUN(log1p)(FTYPE x)
{
    switch (FNAME(fptest)(&x))
    {
        /* test for special codes */
        case FP_NAN:
        case 0:
            return x;

        case FP_INFINITE:
            if (!FISNEG(x))
                return x;

        default:  /* -INF or finite */
            if (x < FLIT(-1.0))
            {
                /* defined only for positive values, 1 <= |x| */
#ifdef _USE_MATHERR
                {
                    struct _exception e = { _DOMAIN, FERR(log1p) };
                    if (_matherr(&e)) return e.retval;
                }
#endif
                __feraise(FE_INVALID);
                return FCONST(nan);
            }
            else if (x == FLIT(-1.0))
            {
                /* defined only for positive values, 1 <= |x| */
#ifdef _USE_MATHERR
                {
                    struct _exception e = { _OVERFLOW, FERR(log1p) };
                    if (_matherr(&e)) return e.retval;
                }
#endif
                __feraise(FE_DIVBYZERO);
                return -FCONST(inf);
            }
            else if (FLIT(-0.5) < x && x < FLIT(0.5))
            {
                /* worth special handling */
                return x * NPOLY(x) / DPOLY(x);
            }
            else
            {
                return FFUN(log)(FLIT(1.0) + x);
            }
    }
}

