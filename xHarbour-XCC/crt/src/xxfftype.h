/****************************************************************************
 *                                                                          *
 * File    : xxfftype.h                                                     *
 *                                                                          *
 * Purpose : Parameters for float floating-point type.                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *           03-06-25  Added macro FERR() for __matherr.                    *
 *                                                                          *
 ****************************************************************************/

#include <float.h>

#define FTYPE       float
#define FCOMP       double
#define FCTYPE      _Fcomplex
#define FBITS       FLT_MANT_DIG
#define FMAXEXP     FLT_MAX_EXP
#define FFUN(fun)   fun##f
#define FMACRO(x)   F##x
#define FNAME(fun)  __##fun##f
#define FCONST(obj) __##obj##f._Float
#define FLIT(lit)   lit##F
#define FISNEG(exp) FSIGN(exp)
#define FCPTYPE     complex<float>
#define FERR(fun)   FERR_(fun##f)
#define FERR_(x)    #x
