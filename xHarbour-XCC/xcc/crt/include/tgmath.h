#ifndef _TGMATH_H
#define _TGMATH_H

#include <math.h>
#include <complex.h>

/* tgmath.h - C99 standard header (preliminary) */

#define __TGMATH_UNARY_REAL_ONLY(x,fcn)  ((sizeof(x) > sizeof(double)) ? fcn##l(x) : (sizeof(x) == sizeof(double)) ? fcn(x) : fcn##f(x))
#define __TGMATH_BINARY_FIRST_REAL_ONLY(x,y,fcn)  ((sizeof(x) > sizeof(double)) ? fcn##l(x,y) : (sizeof(x) == sizeof(double)) ? fcn(x,y) : fcn##f(x,y))
#define __TGMATH_BINARY_REAL_ONLY(x,y,fcn)  ((sizeof(x) > sizeof(double) || sizeof(y) > sizeof(double)) ? fcn##l(x,y) : (sizeof(x) == sizeof(double) || sizeof(y) == sizeof(double)) ? fcn(x,y) : fcn##f(x,y))
#define __TGMATH_TERNARY_FIRST_SECOND_REAL_ONLY(x,y,z,fcn)  ((sizeof(x) > sizeof(double) || sizeof(y) > sizeof(double)) ? fcn##l(x,y,z) : (sizeof(x) == sizeof(double) || sizeof(y) == sizeof(double)) ? fcn(x,y,z) : fcn##f(x,y,z))
#define __TGMATH_TERNARY_REAL_ONLY(x,y,z,fcn)  ((sizeof(x) > sizeof(double) || sizeof(y) > sizeof(double) || sizeof(z) > sizeof(double)) ? fcn##l(x,y,z) : (sizeof(x) == sizeof(double) || sizeof(y) == sizeof(double) || sizeof(z) == sizeof(double)) ? fcn(x,y,z) : fcn##f(x,y,z))

/* These definitions has to be changed as soon as the compiler understands the imaginary keyword */
#define __TGMATH_UNARY_REAL_IMAG(x,fcn,cfcn)  __TGMATH_UNARY_REAL_ONLY(x,fcn)
#define __TGMATH_UNARY_IMAG_ONLY(x,fcn)  (1/0)
#define __TGMATH_BINARY_REAL_IMAG(x,y,fcn,cfcn)  __TGMATH_BINARY_REAL_ONLY(x,y,fcn)

/* avoid clashing with math.h */
#undef cos
#undef cosh
#undef log
#undef log10
#undef log2
#undef sin
#undef sinh
#undef trunc

/* trigonometric functions */
#define acos(x)  __TGMATH_UNARY_REAL_IMAG(x,acos,cacos)
#define asin(x)  __TGMATH_UNARY_REAL_IMAG(x,asin,casin)
#define atan(x)  __TGMATH_UNARY_REAL_IMAG(x,atan,catan)
#define atan2(x,y)  __TGMATH_BINARY_REAL_ONLY(x,y,atan2)
#define cos(x)  __TGMATH_UNARY_REAL_IMAG(x,cos,ccos)
#define sin(x)  __TGMATH_UNARY_REAL_IMAG(x,sin,csin)
#define tan(x)  __TGMATH_UNARY_REAL_IMAG(x,tan,ctan)

/* hyperbolic functions */
#define acosh(x)  __TGMATH_UNARY_REAL_IMAG(x,acosh,cacosh)
#define asinh(x)  __TGMATH_UNARY_REAL_IMAG(x,asinh,casinh)
#define atanh(x)  __TGMATH_UNARY_REAL_IMAG(x,atanh,catanh)
#define cosh(x)  __TGMATH_UNARY_REAL_IMAG(x,cosh,ccosh)
#define sinh(x)  __TGMATH_UNARY_REAL_IMAG(x,sinh,csinh)
#define tanh(x)  __TGMATH_UNARY_REAL_IMAG(x,tanh,ctanh)

/* exponential and logarithmic functions */
#define exp(x)  __TGMATH_UNARY_REAL_IMAG(x,exp,cexp)
#define exp2(x)  __TGMATH_UNARY_REAL_ONLY(x,exp2)
#define expm1(x)  __TGMATH_UNARY_REAL_ONLY(x,expm1)
#define frexp(x,y)  __TGMATH_BINARY_FIRST_REAL_ONLY(x,y,frexp)
#define ldexp(x,y)  __TGMATH_BINARY_FIRST_REAL_ONLY(x,y,ldexp)
#define log(x)  __TGMATH_UNARY_REAL_IMAG(x,log,clog)
#define log10(x)  __TGMATH_UNARY_REAL_ONLY(x,log10)
#define log1p(x)  __TGMATH_UNARY_REAL_ONLY(x,log1p)
#define logb(x)  __TGMATH_UNARY_REAL_ONLY(x,logb)
#define log2(x)  __TGMATH_UNARY_REAL_ONLY(x,log2)

/* power functions */
#define pow(x,y)  __TGMATH_BINARY_REAL_IMAG(x,y,pow,cpow)
#define sqrt(x)  __TGMATH_UNARY_REAL_IMAG(x,sqrt,csqrt)
#define hypot(x,y)  __TGMATH_BINARY_REAL_ONLY(x,y,hypot)
#define cbrt(x)  __TGMATH_UNARY_REAL_ONLY(x,cbrt)

/* nearest integer, absolute value, and remainder functions */
#define ceil(x)  __TGMATH_UNARY_REAL_ONLY(x,ceil)
#define fabs(x)  __TGMATH_UNARY_REAL_IMAG(x,fabs,cabs)
#define floor(x)  __TGMATH_UNARY_REAL_ONLY(x,floor)
#define fmod(x,y)  __TGMATH_BINARY_REAL_ONLY(x,y,fmod)
#define nearbyint(x)  __TGMATH_UNARY_REAL_ONLY(x,nearbyint)
#define round(x)  __TGMATH_UNARY_REAL_ONLY(x,round)
#define trunc(x)  __TGMATH_UNARY_REAL_ONLY(x,trunc)
#define remquo(x,y,z)  __TGMATH_TERNARY_FIRST_SECOND_REAL_ONLY(x,y,z,remquo)
#define lrint(x)  __TGMATH_UNARY_REAL_ONLY(x,lrint)
#define llrint(x)  __TGMATH_UNARY_REAL_ONLY(x,llrint)
#define lround(x)  __TGMATH_UNARY_REAL_ONLY(x,lround)
#define llround(x)  __TGMATH_UNARY_REAL_ONLY(x,llround)
#define copysign(x,y)  __TGMATH_BINARY_REAL_ONLY(x,y,copysign)
#define erf(x)  __TGMATH_UNARY_REAL_ONLY(x,erf)
#define erfc(x)  __TGMATH_UNARY_REAL_ONLY(x,erfc)
#define gamma(x)  __TGMATH_UNARY_REAL_ONLY(x,gamma)
#define lgamma(x)  __TGMATH_UNARY_REAL_ONLY(x,lgamma)
#define rint(x)  __TGMATH_UNARY_REAL_ONLY(x,rint)
#define nextafter(x,y)  __TGMATH_BINARY_REAL_ONLY(x,y,nextafter)
#define nexttoward(x,y)  __TGMATH_BINARY_FIRST_REAL_ONLY(x,y,nexttoward)
#define remainder(x,y)  __TGMATH_BINARY_REAL_ONLY(x,y,remainder)
#define scalbn(x,y)  __TGMATH_BINARY_FIRST_REAL_ONLY(x,y,scalbn)
#define scalbln(x,y)  __TGMATH_BINARY_FIRST_REAL_ONLY(x,y,scalbln)
#define ilogb(x)  __TGMATH_UNARY_REAL_ONLY(x,ilogb)
#define fdim(x,y)  __TGMATH_BINARY_REAL_ONLY(x,y,fdim)
#define fmax(x,y)  __TGMATH_BINARY_REAL_ONLY(x,y,fmax)
#define fmin(x,y)  __TGMATH_BINARY_REAL_ONLY(x,y,fmin)
#define fma(vat1,y,z)  __TGMATH_TERNARY_REAL_ONLY(x,y,z,fma)

/* absolute value, conjugates, and projection */
#define carg(x)  __TGMATH_UNARY_IMAG_ONLY(x,carg)
#define conj(x)  __TGMATH_UNARY_IMAG_ONLY(x,conj)
#define cproj(x)  __TGMATH_UNARY_IMAG_ONLY(x,cproj)

/* decomposing complex values */
#define cimag(x)  __TGMATH_UNARY_IMAG_ONLY(x,cimag)
#define creal(x)  __TGMATH_UNARY_IMAG_ONLY(x,creal)

#endif /* _TGMATH_H */

