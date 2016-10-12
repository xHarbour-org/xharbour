/****************************************************************************
 *                                                                          *
 * File    : xxerf.h                                                        *
 *                                                                          *
 * Purpose : Common erf[fl] functionality.                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

FTYPE (FNAME(erfc))(FTYPE x);

extern FTYPE FNAME(erf_one);
extern FTYPE FNAME(erf_small);

/* coefficients */
#if FBITS <= 26
#define DPOLY(x)    (((c[0] * x + c[1]) * x + c[2]) * x + c[3])
#define NPOLY(x)    (((s[0] * x + s[1]) * x + s[2]) * x + s[3])

static const FTYPE s[] = {  /* 3/3, |x| < 1.523 */
    FLIT(0.2614094187e-3),
    FLIT(0.3605236771e-1),
    FLIT(0.1313206918),
    FLIT(1.1283791672),
};

static const FTYPE c[] = {
    FLIT(0.6362810352e-2),
    FLIT(0.8185214294e-1),
    FLIT(0.4497136338),
    FLIT(1.0),
};
#elif FBITS <= 56
#define DPOLY(x)    ((((((c[0] * x + c[1]) * x + c[2]) * x + c[3]) * x + c[4]) * x + c[5]) * x + c[6])
#define NPOLY(x)    ((((((s[0] * x + s[1]) * x + s[2]) * x + s[3]) * x + s[4]) * x + s[5]) * x + s[6])

static const FTYPE s[] = {  /* 6/6, |x| < 1.523 */
    FLIT(0.12040999373658753679e-6),
    FLIT(0.88784405615357064309e-5),
    FLIT(0.32806639713878541747e-3),
    FLIT(0.34814902101731196859e-2),
    FLIT(0.51728385731987527358e-1),
    FLIT(0.17644011324127378370),
    FLIT(1.12837916709551257390),
};

static const FTYPE c[] = {
    FLIT(0.14060567875468972603e-5),
    FLIT(0.58411032889811748820e-4),
    FLIT(0.11742377551152094336e-2),
    FLIT(0.14283713727543944095e-1),
    FLIT(0.10907619238514398035),
    FLIT(0.48969931241768294256),
    FLIT(1.0),
};
#elif FBITS <= 67
#define DPOLY(x)    FNAME(poly)(x, c, sizeof(c) / sizeof(c[0]) - 1)
#define NPOLY(x)    FNAME(poly)(x, s, sizeof(s) / sizeof(s[0]) - 1)

static const FTYPE s[] = {  /* 7/7, |x| < 1.523 */
    FLIT(0.1173335177040224550671508e-8),
    FLIT(0.5216079249572345180419514e-6),
    FLIT(0.1125181866981447313660203e-4),
    FLIT(0.3977558882436845809440133e-3),
    FLIT(0.3360913618709334889923423e-2),
    FLIT(0.5253764285753105067039696e-1),
    FLIT(0.1665104998583204282304198),
    FLIT(1.1283791670955125738961589),
};

static const FTYPE c[] = {
    FLIT(0.4733455560436380663773544e-7),
    FLIT(0.2667997941112245291838663e-5),
    FLIT(0.7348946679631872029680150e-4),
    FLIT(0.1259564072809287409160560e-2),
    FLIT(0.1431814076013746416379793e-1),
    FLIT(0.1068600809262896309878225),
    FLIT(0.4808994216783745456626571),
    FLIT(1.0),
};
#elif FBITS <= 115
#define DPOLY(x)    FNAME(poly)(x, c, sizeof(c) / sizeof(c[0]) - 1)
#define NPOLY(x)    FNAME(poly)(x, s, sizeof(s) / sizeof(s[0]) - 1)

static const FTYPE s[] = {  /* 11/11, |x| < 1.523 */
    FLIT(0.51630403730106176957896163066178846e-15),
    FLIT(0.49216883710822226957326066712120517e-12),
    FLIT(0.26749838834244420657493483239069858e-10),
    FLIT(0.20936153135649591032720013480984730e-8),
    FLIT(0.50837799055863542101951484001252012e-7),
    FLIT(0.19519247483816617346350109590577561e-5),
    FLIT(0.26215824696504770787393598555592600e-4),
    FLIT(0.59391183411995598350191003079143702e-3),
    FLIT(0.43202969131006314964069654757878308e-2),
    FLIT(0.57391809147983659218080008306136341e-1),
    FLIT(0.17522105028249451845826062362718280),
    FLIT(1.12837916709551257389615890312154518),
};

static const FTYPE c[] = {
    FLIT(0.29885656240135625646372883056909688e-13),
    FLIT(0.39378594485318396123578294822005288e-11),
    FLIT(0.25732367455719518970474635679830478e-9),
    FLIT(0.10920248597362729336822861661274367e-7),
    FLIT(0.33223884538399406499612543947598922e-6),
    FLIT(0.75676892543413966092564352074666446e-5),
    FLIT(0.13108087039350296028951919419989479e-3),
    FLIT(0.17196835506108127612957305505535337e-2),
    FLIT(0.16688108848991384100189484950384267e-1),
    FLIT(0.11373514856731955191244262104412449),
    FLIT(0.48861894599979156643136182845639956),
    FLIT(1.0),
};
#else
#error erf has insufficient precision
#endif

/* compute erf(x) */
FTYPE __cdecl (FFUN(erf))(FTYPE x)
{
    FTYPE y;

    switch (FNAME(fptest)(&x))
    {
        /* test for special codes */
        case FP_NAN:
        case 0:
            return x;

        case FP_INFINITE:
            return FISNEG(x) ? FLIT(-1.0) : FLIT(1.0);

        default:  /* finite */
            y = x < FLIT(0.0) ? -x : x;
            if (y < FNAME(erf_small))
            {
                /* compute rational approximation */
                y = x * x;
                return x * NPOLY(y) / DPOLY(y);
            }
            else if (y < FNAME(erf_one))
                y = FLIT(1.0) - FNAME(erfc)(y);
            else
                y = FLIT(1.0);

            return (x < FLIT(0.0)) ? -y : y;
    }
}

