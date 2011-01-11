#ifndef _XMATH_H
#define _XMATH_H

#ifndef _MSC_VER
#include <errno.h>
#define _MSC_VER
#include <math.h>
#undef _MSC_VER
#include <stddef.h>
#else
#include <errno.h>
#include <math.h>
#include <stddef.h>
#endif

/* xmath.h - internal header */

#define _USE_MATHERR  /* implement Microsoft _matherr() */

typedef union {  /* pun float types as integer array */
    unsigned short _Word[4];
    float _Float;
    double _Double;
    long double _Long_double;
} __fpconst;

/* float properties */
#define _D0  3          /* little-endian, small long doubles */
#define _D1  2
#define _D2  1
#define _D3  0

#define _DBIAS  0x3fe   /* IEEE format double and float */
#define _DOFF   4

#define _FBIAS  0x7e
#define _FOFF   7

#define _LBIAS  0x3ffe  /* 80/128 long double bits */
#define _LOFF   15      /* 80/128 long double bits */

/* _DLONG: 0: 64, 1: 80, 2: 128 long double bits */

/* IEEE 754 double properties */
#define _DFRAC  ((unsigned short)((1 << _DOFF) - 1))
#define _DMASK  ((unsigned short)(0x7fff & ~_DFRAC))
#define _DMAX   ((unsigned short)((1 << (15 - _DOFF)) - 1))
#define _DSIGN  ((unsigned short)0x8000)
#define DSIGN(x)  (((unsigned short *)&(x))[_D0] & _DSIGN)
#define HUGE_EXP  (int)(_DMAX * 900L / 1000)
#define HUGE_RAD  2.73e9  /* ~ 2^33 / pi */
#define SAFE_EXP  ((short)(_DMAX >> 1))

/* IEEE 754 float properties */
#define _FFRAC  ((unsigned short)((1 << _FOFF) - 1))
#define _FMASK  ((unsigned short)(0x7fff & ~_FFRAC))
#define _FMAX   ((unsigned short)((1 << (15 - _FOFF)) - 1))
#define _FSIGN  ((unsigned short)0x8000)
#define FSIGN(x)  (((unsigned short *)&(x))[_F0] & _FSIGN)
#define FHUGE_EXP  (int)(_FMAX * 900L / 1000)
#define FHUGE_RAD  40.7    /* ~ 2^7 / pi */
#define FSAFE_EXP  ((short)(_FMAX >> 1))

#if _D0 == 0
#define _F0  0      /* big-endian */
#define _F1  1
#else
#define _F0  1      /* little-endian */
#define _F1  0
#endif

/* IEEE 754 long double properties */
#define _LFRAC  ((unsigned short)(-1))
#define _LMASK  ((unsigned short)0x7fff)
#define _LMAX   ((unsigned short)0x7fff)
#define _LSIGN  ((unsigned short)0x8000)
#define LSIGN(x)  (((unsigned short *)&(x))[_L0] & _LSIGN)
#define LHUGE_EXP  (int)(_LMAX * 900L / 1000)
#define LHUGE_RAD  2.73e9  /* ~ 2^33 / pi */
#define LSAFE_EXP  ((short)(_LMAX >> 1))

#if _D0 == 0
#define _L0  0      /* big-endian */
#define _L1  1
#define _L2  2
#define _L3  3
#define _L4  4
#define _L5  5      /* 128-bit only */
#define _L6  6
#define _L7  7
#else
#define _L0  3      /* little-endian, 64-bit long doubles */
#define _L1  2
#define _L2  1
#define _L3  0
#define _L4  xxx    /* should never be used */
#define _L5  xxx
#define _L6  xxx
#define _L7  xxx
#endif

#if 0
#define _L0  4      /* little-endian, 80-bit long doubles */
#define _L1  3
#define _L2  2
#define _L3  1
#define _L4  0
#define _L5  xxx    /* should never be used */
#define _L6  xxx
#define _L7  xxx
#endif

/* return values for __stopfx and __stoflt */
#define FL_ERR  0
#define FL_DEC  1
#define FL_HEX  2
#define FL_INF  3
#define FL_NAN  4
#define FL_NEG  8

int __stopfx(const char **, char **);
int __stoflt(const char *, const char *, char **, long[], int);
int __stoxflt(const char *, const char *, char **, long[], int);
int __wstopfx(const wchar_t **, wchar_t **);
int __wstoflt(const wchar_t *, const wchar_t *, wchar_t **, long[], int);
int __wstoxflt(const wchar_t *, const wchar_t *, wchar_t **, long[], int);

/* error reporting */
void __cdecl __feraise(int);

/* double declarations */
double __atan(double, int);
double __hypot(double, double, int *);
double __poly(double, const double *, int);
short __fpint(double *, short);
short __fpnorm(unsigned short *);
short __fpscale(double *, long);
double __fppow10(double, long);
short __fpunscale(short *, double *);

extern const __fpconst __inf, __nan;
extern const __fpconst __eps, __rteps;
extern const double __xbig, __zero;

/* float declarations */
float __atanf(float, int);
float __hypotf(float, float, int *);
float __polyf(float, const float *, int);
short __fpintf(float *, short);
short __fpnormf(unsigned short *);
short __fpscalef(float *, long);
float __fppow10f(float, long);
short __fpunscalef(short *, float *);

extern const __fpconst __inff, __nanf;
extern const __fpconst __epsf, __rtepsf;
extern const float __xbigf, __zerof;

/* long double functions */
long double __atanl(long double, int);
long double __hypotl(long double, long double, int *);
long double __polyl(long double, const long double *, int);
short __fpintl(long double *, short);
short __fpnorml(unsigned short *);
short __fpscalel(long double *, long);
long double __fppow10l(long double, long);
short __fpunscalel(short *, long double *);

extern const __fpconst __infl, __nanl;
extern const __fpconst __epsl, __rtepsl;
extern const long double __xbigl, __zerol;

#endif /* _XMATH_H */

