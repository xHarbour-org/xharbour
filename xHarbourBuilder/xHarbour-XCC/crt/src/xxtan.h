/****************************************************************************
 *                                                                          *
 * File    : xxtan.h                                                        *
 *                                                                          *
 * Purpose : Common tan[fl] functionality.                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-30  Created                                              *
 *           03-06-27  Added call to __matherr().                           *
 *                                                                          *
 ****************************************************************************/

#include <fenv.h>
#include "xmath.h"

/* coefficients */
#if FBITS <= 25
#define CPOLY(x)    (c[0] * x + c[1])
#define SPOLY(x)    (s[0])

static const FTYPE c[] = {  /* s3/c4, |x| < pi/4 (100.0%) */
    FLIT( 0.0097099364),
    FLIT(-0.4291161787),
};
static const FTYPE s[] = {
    FLIT(-0.0957822992),
};

#elif FBITS <= 54
#define CPOLY(x)    ((c[0] * x + c[1]) * x + c[2])
#define SPOLY(x)    ((s[0] * x + s[1]) * x + s[2])

static const FTYPE c[] = {  /* s7/c6, |x| < pi/4 (100.0%) */
    FLIT(-0.00020844956382258822),
    FLIT( 0.02334489464693293380),
    FLIT(-0.46161689768996201755),
};
static const FTYPE s[] = {
    FLIT(-0.00000748373924372997),
    FLIT( 0.00280592875035233052),
    FLIT(-0.12828356435663158978),
};

#elif FBITS <= 65
#define CPOLY(x)    (((c[0] * x + c[1]) * x + c[2]) * x + c[3])
#define SPOLY(x)    ((s[0] * x + s[1]) * x + s[2])

static const FTYPE c[] = {  /* s7/c8, |x| < pi/4 (100.0%) */
    FLIT( 0.0000004982155748201469973),
    FLIT(-0.0003118197048739718596888),
    FLIT( 0.0256639305740064946020274),
    FLIT(-0.4667170495229229421344638),
};
static const FTYPE s[] = {
    FLIT(-0.0000178621483399276882542),
    FLIT( 0.0034249140663654322654332),
    FLIT(-0.1333837161895896063094998),
};

#elif FBITS <= 122
#define CPOLY(x)    (((((c[0] * x + c[1]) * x + c[2]) * x + c[3]) * x + c[4]) * x + c[5])
#define SPOLY(x)    (((((s[0] * x + s[1]) * x + s[2]) * x + s[3]) * x + s[4]) * x + s[5])

static const FTYPE c[] = {  /* s13/c12, |x| < pi/4 (100.0%) */
    FLIT( 0.0000000000115609621956528865273653190745),
    FLIT(-0.0000000152282257707155133503259641727415),
    FLIT( 0.0000052362461617507082702365563783865073),
    FLIT(-0.0006628415426839907049976317509687715401),
    FLIT( 0.0318890553302089929317034042452254748273),
    FLIT(-0.4800104942477280723430750249033451924731),
};
static const FTYPE s[] = {
    FLIT( 0.0000000000001272546759264684873261426622),
    FLIT(-0.0000000005196106495025209303140153897877),
    FLIT( 0.0000003233844616565437096178594026037773),
    FLIT(-0.0000663016973907677862049130215087603811),
    FLIT( 0.0052188905809663021506783959441132263779),
    FLIT(-0.1466771609143947390097416915700118926095),
};
#else
#error tan has insufficient precision
#endif

#if FBITS < 24
#error tan has too much precision
#endif

static const FTYPE c1 = (FTYPE)(13176794.0 / 8388608.0);
static const FTYPE c2 = FLIT(7.5497899548918821691639751442098584e-8);

static const FTYPE twobypi = FLIT(0.63661977236758134307553505349005744);
static const FTYPE twopi = FLIT(6.2831853071795864769252867665590057);

/* compute tan(x) */
FTYPE __cdecl (FFUN(tan))(FTYPE x)
{
    FTYPE g;
    long quad;

    switch (FNAME(fptest)(&x))
    {
        case FP_NAN:
            return x;

        case FP_INFINITE:
#ifdef _USE_MATHERR
            {
                struct _exception e = { _DOMAIN, FERR(tan) };
                if (_matherr(&e)) return e.retval;
            }
#endif
            __feraise(FE_INVALID);
            return FCONST(nan);

        case 0:
            return x;

        default:  /* finite */
            if (x < -FMACRO(HUGE_RAD) || FMACRO(HUGE_RAD) < x)
                x = FFUN(fmod)(x, twopi);
            g = x * twobypi;
            quad = (long)(0 < g ? g + FLIT(0.5) : g - FLIT(0.5));
            g = quad;
            g = (x - g * c1) - g * c2;
            if (-FCONST(rteps) < g && g < FCONST(rteps))
                return ((unsigned int)quad & 0x1 ? FLIT(-1.0) / g : g);
            else
            {
                /* g*g worth computing */
                const FTYPE z = g * g;
                const FTYPE gd = FLIT(1.0) + z * CPOLY(z);

                g += g * z * SPOLY(z);
                return ((unsigned int)quad & 0x1 ? -gd / g : g / gd);
            }
    }
}

