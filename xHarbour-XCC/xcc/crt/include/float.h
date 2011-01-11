#ifndef _FLOAT_H
#define _FLOAT_H

/* float.h - C99 standard header (IEEE 754 version) */

/* common properties */
#define FLT_RADIX  2
#define FLT_ROUNDS  1
#define FLT_EVAL_METHOD  -1

#define DECIMAL_DIG  16  /* 64-bit long double */

/* float properties */
#ifdef __MSPOCC__
#define FLT_EPSILON  1.192092896e-07F
#define FLT_MAX      3.402823466e+38F
#define FLT_MIN      1.175494351e-38F
#else
#define FLT_EPSILON  0x8p-26
#define FLT_MAX      0xf.fffffp+124
#define FLT_MIN      0x8p-129
#endif /* __MSPOCC__ */

#define FLT_DIG         6
#define FLT_MANT_DIG    24
#define FLT_MIN_10_EXP  -37
#define FLT_MAX_10_EXP  38
#define FLT_MIN_EXP     -125
#define FLT_MAX_EXP     128

/* double properties */
#ifdef __MSPOCC__
#define DBL_EPSILON  2.2204460492503131e-16
#define DBL_MAX      1.7976931348623158e+308
#define DBL_MIN      2.2250738585072014e-308
#else
#define DBL_EPSILON  0x8p-55
#define DBL_MAX      0xf.ffffffffffff8p+1020
#define DBL_MIN      0x8p-1025
#endif /* __MSPOCC__ */

#define DBL_DIG         15
#define DBL_MANT_DIG    53
#define DBL_MIN_10_EXP  -307
#define DBL_MAX_10_EXP  308
#define DBL_MIN_EXP     -1021
#define DBL_MAX_EXP     1024

/* 64-bit long double properties */
#ifdef __MSPOCC__
#define LDBL_EPSILON  2.2204460492503131e-16L
#define LDBL_MAX      1.7976931348623158e+308L
#define LDBL_MIN      2.2250738585072014e-308L
#else
#define LDBL_EPSILON  0x8p-55L
#define LDBL_MAX      0xf.ffffffffffff8p+1020L
#define LDBL_MIN      0x8p-1025L
#endif /* __MSPOCC__ */

#define LDBL_DIG         15
#define LDBL_MANT_DIG    53
#define LDBL_MIN_10_EXP  -307
#define LDBL_MAX_10_EXP  308
#define LDBL_MIN_EXP     -1021
#define LDBL_MAX_EXP     1024

#endif /* _FLOAT_H */

