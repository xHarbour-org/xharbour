#ifndef _MATH_H
#define _MATH_H

/* math.h - C99 standard header */

/* macros */
#define HUGE_VAL   (1.0 / (1.0, 0.0))
#define HUGE_VALF  (1.0F / (1.0, 0.0F))
#define HUGE_VALL  (1.0L / (1.0, 0.0L))
#define INFINITY   (1.0F / (1.0, 0.0F))
#define NAN        (0.0 / (1.0, 0.0))

#define FP_INFINITE   1
#define FP_NAN        2
#define FP_NORMAL     (-1)
#define FP_SUBNORMAL  (-2)
#define FP_ZERO       0

#define FP_ILOGB0    (-0x7fffffff - 1)
#define FP_ILOGBNAN  0x7fffffff

#define MATH_ERRNO        1
#define MATH_ERREXCEPT    2
#define math_errhandling  (MATH_ERRNO|MATH_ERREXCEPT)

#define _FP_LT  1
#define _FP_EQ  2
#define _FP_GT  4

/* type definitions */
#if FLT_EVAL_METHOD == 1
typedef double float_t;
typedef double double_t;
#elif FLT_EVAL_METHOD == 2
typedef long double float_t;
typedef long double double_t;
#else /* FLT_EVAL_METHOD == 0 */
typedef float float_t;
typedef double double_t;
#endif

/* helper functions */
int __cdecl __fpclass(double);
int __cdecl __fpclassf(float);
int __cdecl __fpclassl(long double);
int __cdecl __fpcomp(double, double);
int __cdecl __fpcompf(float, float);
int __cdecl __fpcompl(long double, long double);
int __cdecl __fpsign(double);
int __cdecl __fpsignf(float);
int __cdecl __fpsignl(long double);

#define isfinite(x)  (fpclassify(x) <= 0)
#define isinf(x)     (fpclassify(x) == FP_INFINITE)
#define isnan(x)     (fpclassify(x) == FP_NAN)
#define isnormal(x)  (fpclassify(x) == FP_NORMAL)

#define isgreater(x,y)  (_FPCOMP(x,y) & _FP_GT)
#define isgreaterequal(x,y)  (_FPCOMP(x,y) & (_FP_EQ|_FP_GT))
#define isless(x,y)  (_FPCOMP(x,y) & _FP_LT)
#define islessequal(x,y)  (_FPCOMP(x,y) & (_FP_LT|_FP_EQ))
#define islessgreater(x,y)  (_FPCOMP(x,y) & (_FP_LT|_FP_GT))
#define isunordered(x,y)  (_FPCOMP(x,y) == 0)

#ifndef _ARG
#define _ARG(x)  (sizeof((x) + (float)0) == sizeof(float) ? 'f' : sizeof((x) + (double)0) == sizeof(double) ? 'd' : 'l')
#endif

#define fpclassify(x)  (_ARG(x) == 'f' ? __fpclassf(x) : _ARG(x) == 'd' ? __fpclass(x) : __fpclassl(x))
#define _FPCOMP(x,y)  (_ARG((x) + (y)) == 'f' ? __fpcompf(x,y) : _ARG((x) + (y)) == 'd' ? __fpcomp(x,y) : __fpcompl(x,y))
#define signbit(x)  (_ARG(x) == 'f' ? __fpsignf(x) : _ARG(x) == 'd' ? __fpsign(x) : __fpsignl(x))

/* double declarations */
double __cdecl acos(double);
double __cdecl acosh(double);
double __cdecl asin(double);
double __cdecl asinh(double);
double __cdecl atan(double);
double __cdecl atan2(double, double);
double __cdecl atanh(double);
double __cdecl cbrt(double);
double __cdecl ceil(double);
double __cdecl copysign(double, double);
double __cdecl erf(double);
double __cdecl erfc(double);
double __cdecl exp(double);
double __cdecl exp2(double);
double __cdecl expm1(double);
double __cdecl fabs(double);
double __cdecl fdim(double, double);
double __cdecl floor(double);
double __cdecl fma(double, double, double);
double __cdecl fmax(double, double);
double __cdecl fmin(double, double);
double __cdecl fmod(double, double);
double __cdecl frexp(double, int *);
double __cdecl hypot(double, double);
int __cdecl ilogb(double);
double __cdecl ldexp(double, int);
double __cdecl lgamma(double);
long long __cdecl llrint(double);
long long __cdecl llround(double);
double __cdecl log1p(double);
double __cdecl logb(double);
long __cdecl lrint(double);
long __cdecl lround(double);
double __cdecl modf(double, double *);
double __cdecl nan(const char *);
double __cdecl nearbyint(double);
double __cdecl nextafter(double, double);
double __cdecl nexttoward(double, long double);
double __cdecl pow(double, double);
double __cdecl remainder(double, double);
double __cdecl remquo(double, double, int *);
double __cdecl rint(double);
double __cdecl round(double);
double __cdecl scalbn(double, int);
double __cdecl scalbln(double, long);
double __cdecl sqrt(double);
double __cdecl tan(double);
double __cdecl tanh(double);
double __cdecl tgamma(double);
double __cdecl trunc(double);

/* float declarations */
float __cdecl acosf(float);
float __cdecl acoshf(float);
float __cdecl asinf(float);
float __cdecl asinhf(float);
float __cdecl atanf(float);
float __cdecl atanhf(float);
float __cdecl atan2f(float, float);
float __cdecl cbrtf(float);
float __cdecl ceilf(float);
float __cdecl copysignf(float, float);
float __cdecl erff(float);
float __cdecl erfcf(float);
float __cdecl expf(float);
float __cdecl exp2f(float);
float __cdecl expm1f(float);
float __cdecl fabsf(float);
float __cdecl fdimf(float, float);
float __cdecl floorf(float);
float __cdecl fmaf(float, float, float);
float __cdecl fmaxf(float, float);
float __cdecl fminf(float, float);
float __cdecl fmodf(float, float);
float __cdecl frexpf(float, int *);
float __cdecl hypotf(float, float);
int __cdecl ilogbf(float);
float __cdecl ldexpf(float, int);
float __cdecl lgammaf(float);
long long __cdecl llrintf(float);
long long __cdecl llroundf(float);
float __cdecl log1pf(float);
float __cdecl logbf(float);
long __cdecl lrintf(float);
long __cdecl lroundf(float);
float __cdecl modff(float, float *);
float __cdecl nanf(const char *);
float __cdecl nearbyintf(float);
float __cdecl nextafterf(float, float);
float __cdecl nexttowardf(float, long double);
float __cdecl powf(float, float);
float __cdecl remainderf(float, float);
float __cdecl remquof(float, float, int *);
float __cdecl rintf(float);
float __cdecl roundf(float);
float __cdecl scalbnf(float, int);
float __cdecl scalblnf(float, long);
float __cdecl sqrtf(float);
float __cdecl tanf(float);
float __cdecl tanhf(float);
float __cdecl tgammaf(float);
float __cdecl truncf(float);

/* long double declarations */
long double __cdecl acosl(long double);
long double __cdecl acoshl(long double);
long double __cdecl asinl(long double);
long double __cdecl asinhl(long double);
long double __cdecl atanl(long double);
long double __cdecl atanhl(long double);
long double __cdecl atan2l(long double, long double);
long double __cdecl cbrtl(long double);
long double __cdecl ceill(long double);
long double __cdecl copysignl(long double, long double);
long double __cdecl erfl(long double);
long double __cdecl erfcl(long double);
long double __cdecl expl(long double);
long double __cdecl exp2l(long double);
long double __cdecl expm1l(long double);
long double __cdecl fabsl(long double);
long double __cdecl fdiml(long double, long double);
long double __cdecl floorl(long double);
long double __cdecl fmal(long double, long double, long double);
long double __cdecl fmaxl(long double, long double);
long double __cdecl fminl(long double, long double);
long double __cdecl fmodl(long double, long double);
long double __cdecl frexpl(long double, int *);
long double __cdecl hypotl(long double, long double);
int __cdecl ilogbl(long double);
long double __cdecl ldexpl(long double, int);
long double __cdecl lgammal(long double);
long long __cdecl llrintl(long double);
long long __cdecl llroundl(long double);
long double __cdecl log1pl(long double);
long double __cdecl logbl(long double);
long __cdecl lrintl(long double);
long __cdecl lroundl(long double);
long double __cdecl modfl(long double, long double *);
long double __cdecl nanl(const char *);
long double __cdecl nearbyintl(long double);
long double __cdecl nextafterl(long double, long double);
long double __cdecl nexttowardl(long double, long double);
long double __cdecl powl(long double, long double);
long double __cdecl remainderl(long double, long double);
long double __cdecl remquol(long double, long double, int *);
long double __cdecl rintl(long double);
long double __cdecl roundl(long double);
long double __cdecl scalbnl(long double, int);
long double __cdecl scalblnl(long double, long);
long double __cdecl sqrtl(long double);
long double __cdecl tanl(long double);
long double __cdecl tanhl(long double);
long double __cdecl tgammal(long double);
long double __cdecl truncl(long double);

double __cdecl cos(double);
double __cdecl cosh(double);
double __cdecl log(double);
double __cdecl log10(double);
double __cdecl log2(double);
double __cdecl sin(double);
double __cdecl sinh(double);

/* double macro overrides */
#ifndef _M_ARM
#define cos(x)  __sin(x,1)
#define cosh(x)  __cosh(x,1)
#define log(x)  __log(x,0)
#define log10(x)  __log(x,1)
#define log2(x)  __log(x,2)
#define sin(x)  __sin(x,0)
#define sinh(x)  __sinh(x,1)
#endif /* _M_ARM */

float __cdecl cosf(float);
float __cdecl coshf(float);
float __cdecl logf(float);
float __cdecl log10f(float);
float __cdecl log2f(float);
float __cdecl sinf(float);
float __cdecl sinhf(float);

/* float macro overrides */
#ifndef _M_ARM
#define cosf(x)  __sinf(x,1)
#define coshf(x)  __coshf(x,1)
#define logf(x)  __logf(x,0)
#define log10f(x)  __logf(x,1)
#define log2f(x)  __logf(x,2)
#define sinf(x)  __sinf(x,0)
#define sinhf(x)  __sinhf(x,1)
#endif /* _M_ARM */

long double __cdecl cosl(long double);
long double __cdecl coshl(long double);
long double __cdecl logl(long double);
long double __cdecl log10l(long double);
long double __cdecl log2l(long double);
long double __cdecl sinl(long double);
long double __cdecl sinhl(long double);

/* long double macro overrides */
#ifndef _M_ARM
#define cosl(x)  __sinl(x,1)
#define coshl(x)  __coshl(x,1)
#define logl(x)  __logl(x,0)
#define log10l(x)  __logl(x,1)
#define log2l(x)  __logl(x,2)
#define sinl(x)  __sinl(x,0)
#define sinhl(x)  __sinhl(x,1)
#endif /* _M_ARM */

/* double declarations */
double __cdecl __cosh(double, double);
double __cdecl __log(double, int);
double __cdecl __sin(double, unsigned int);
double __cdecl __sinh(double, double);
short __cdecl __fptest(double *);
short __exp(double *, double, short);

/* float declarations */
float __cdecl __coshf(float, float);
float __cdecl __logf(float, int);
float __cdecl __sinf(float, unsigned int);
float __cdecl __sinhf(float, float);
short __cdecl __fptestf(float *);
short __expf(float *, float, short);

/* long double declarations */
long double __cdecl __coshl(long double, long double);
long double __cdecl __logl(long double, int);
long double __cdecl __sinl(long double, unsigned int);
long double __cdecl __sinhl(long double, long double);
short __cdecl __fptestl(long double *);
short __expl(long double *, long double, short);

#ifdef _MSC_EXTENSIONS
/* values for _exception type */
#define _DOMAIN  1
#define _SING  2
#define _OVERFLOW  3
#define _UNDERFLOW  4
#define _TLOSS  5
#define _PLOSS  6

/* compatibility names */
#ifdef __POCC__OLDNAMES
#define DOMAIN  _DOMAIN
#define SING  _SING
#define OVERFLOW  _OVERFLOW
#define UNDERFLOW  _UNDERFLOW
#define TLOSS  _TLOSS
#define PLOSS  _PLOSS
#endif /* __POCC__OLDNAMES */

/* passed to _matherr() when a fp exception is detected */
struct _exception {
    int type;
    char *name;
    double arg1;
    double arg2;
    double retval;
};

int __cdecl _matherr(struct _exception *);
#endif /* _MSC_EXTENSIONS */

#endif /* _MATH_H */

