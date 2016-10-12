/****************************************************************************
 *                                                                          *
 * File    : xxlftype.h                                                     *
 *                                                                          *
 * Purpose : Parameters for long double floating-point type.                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *           03-06-25  Added macro FERR() for __matherr.                    *
 *                                                                          *
 ****************************************************************************/

#include <float.h>

#define FTYPE       long double
#define FCOMP       long double
#define FCTYPE      _Lcomplex
#define FBITS       LDBL_MANT_DIG
#define FMAXEXP     LDBL_MAX_EXP
#define FFUN(fun)   fun##l
#define FMACRO(x)   L##x
#define FNAME(fun)  __##fun##l
#define FCONST(obj) __##obj##l._Long_double
#define FLIT(lit)   lit##L
#define FISNEG(exp) LSIGN(exp)
#define FCPTYPE     complex<long double>
#define FERR(fun)   FERR_(fun##l)
#define FERR_(x)    #x

