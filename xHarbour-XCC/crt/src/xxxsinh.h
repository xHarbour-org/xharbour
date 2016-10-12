/****************************************************************************
 *                                                                          *
 * File    : xxxsinh.h                                                      *
 *                                                                          *
 * Purpose : Common _[FL]Sinh functionality.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-09-02  Created                                              *
 *           03-06-27  Added call to __matherr().                           *
 *                                                                          *
 ****************************************************************************/

#include <fenv.h>
#include "xmath.h"

/* coefficients */
#if FBITS <= 30
#define CPOLY(x)    (c[0] * x + c[1])
#define SPOLY(x)    (s[0])

static const FTYPE c[] = {  /* 3rd-order, |x| < ln(3)/2 (100.0%) */
    FLIT( 0.0018399770),
    FLIT(-0.0610359754),
};
static const FTYPE s[] = {
    FLIT( 0.1056307265),
};

#elif FBITS <= 53
#define CPOLY(x)    (c[0] * x + c[1])
#define SPOLY(x)    ((s[0] * x + s[1]) * x + s[2])

static const FTYPE c[] = {  /* 5th-order, |x| < ln(3)/2 (100.0%) */
    FLIT( 0.00015900461490295664),
    FLIT(-0.02056695099386675945),
};
static const FTYPE s[] = {
    FLIT( 0.00005352221341916260),
    FLIT( 0.00506451278229574483),
    FLIT( 0.14609971567280727143),
};

#elif FBITS <= 64
#define CPOLY(x)    ((c[0] * x + c[1]) * x + c[2])
#define SPOLY(x)    ((s[0] * x + s[1]) * x + s[2])

static const FTYPE c[] = {  /* 6th-order, |x| < ln(3)/2 (100.0%) */
    FLIT(-0.00000158330729052950769915),
    FLIT( 0.00027519426342349878805713),
    FLIT( -0.0241170695527346096673505),
};
static const FTYPE s[] = {
    FLIT( 0.0000417195220925635337445),
    FLIT( 0.0045890160046340899454803),
    FLIT( 0.1425495971139320626966453),
};

#elif FBITS <= 114
#define CPOLY(x)    ((((c[0] * x + c[1]) * x + c[2]) * x + c[3]) * x + c[4])
#define SPOLY(x)    ((((s[0] * x + s[1]) * x + s[2]) * x + s[3]) * x + s[4])

static const FTYPE c[] = {  /* 9th-order, |x| < ln(3)/2 (100.0%) */
    FLIT(-0.00000000000336993857130038006925178471496),
    FLIT( 0.00000000194074062258199190461301221438596),
    FLIT(-0.00000060338126386989611923562546176683121),
    FLIT( 0.00012225633519572222239274990096628782027),
    FLIT(-0.01570282210530478910525045132285980643671),
};
static const FTYPE s[] = {
    FLIT( 0.0000000013284592352809230453478855863105),
    FLIT( 0.0000005602726057324471474526599821935838),
    FLIT( 0.0000873285221372423111008817391507917175),
    FLIT( 0.0058384526509782573715176746781914259057),
    FLIT( 0.1509638445613618775614162153438248456945),
};
#else
#error sinh has insufficient precision
#endif

static const FTYPE ln3by2 = FLIT(0.54930614433405484569762261846126284);

/* compute tanh(x) */
FTYPE __cdecl FNAME(sinh)(FTYPE x, FTYPE y)
{
    short neg;

    switch (FNAME(fptest)(&x))
    {
        /* test for special codes */
        case FP_NAN:
            return x;

        case FP_INFINITE:
            return (y != FLIT(0.0)) ? x : FISNEG(x) ? -y : y;

        case 0:
            return x * y;

        default:  /* finite */
            if (y == FLIT(0.0))
                return (x < FLIT(0.0)) ? -y : y;

            if (x < FLIT(0.0))
                x = -x, neg = 1;
            else
                neg = 0;

            if (x < FCONST(rteps))
            {
                /* x tiny */
                x *= y;
            }
            else if (x < ln3by2)
            {
                /* |x| < ln(3)/2 */
                const FTYPE z = x * x;
                const FCOMP ch = FLIT(1.0) + z * CPOLY(z);

                x += x * z * SPOLY(z);
                x /= ch;
            }
            else if (x < FNAME(xbig))
            {
                /* worth subtracting exp(-x)/2 */
                FNAME(exp)(&x, FLIT(1.0), -1);
                x = y * (x - FLIT(0.25) / x);
            }
            else
            {
                switch (FNAME(exp)(&x, y, -1))
                {
                    /* report over/underflow */
                    case 0:
#ifdef _USE_MATHERR
                        {
                            struct _exception e = { _UNDERFLOW, FERR(sinh) };
                            if (_matherr(&e)) return e.retval;
                        }
#endif
                        __feraise(FE_UNDERFLOW);
                        break;

                    case FP_INFINITE:
#ifdef _USE_MATHERR
                        {
                            struct _exception e = { _OVERFLOW, FERR(sinh) };
                            if (_matherr(&e)) return e.retval;
                        }
#endif
                        __feraise(FE_OVERFLOW);
                        break;
                }
            }

            return (neg) ? -x : x;
    }
}

