/****************************************************************************
 *                                                                          *
 * File    : xxdftype.h                                                     *
 *                                                                          *
 * Purpose : Parameters for double floating-point type.                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *           03-06-25  Added macro FERR() for __matherr.                    *
 *                                                                          *
 ****************************************************************************/

#include <float.h>

#define FTYPE       double
#define FCOMP       double
#define FCTYPE      _Dcomplex
#define FBITS       DBL_MANT_DIG
#define FMAXEXP     DBL_MAX_EXP
#define FFUN(fun)   fun
#define FMACRO(x)   x
#define FNAME(fun)  __##fun
#define FCONST(obj) __##obj._Double
#define FLIT(lit)   lit
#define FISNEG(exp) DSIGN(exp)
#define FCPTYPE     complex<double>
#define FERR(fun)   #fun
