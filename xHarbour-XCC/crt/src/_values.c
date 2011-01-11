/****************************************************************************
 *                                                                          *
 * File    : _values.c                                                      *
 *                                                                          *
 * Purpose : Values used by math functions -- IEEE 754 version.             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-10  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* macros */
#define NBITS   (48 + _DOFF)

#if _D0 == 0
#define INIT(w0)       { w0, 0, 0, 0 }
#define INIT2(w0, w1)  { w0, 0, 0, w1 }
#else
#define INIT(w0)       { 0, 0, 0, w0 }
#define INIT2(w0, w1)  { w1, 0, 0, w0 }
#endif

                /* static data */
// const __fpconst _Denorm = { INIT2(0, 1) };
const __fpconst __inf = { INIT(_DMAX << _DOFF) };
const __fpconst __nan = { INIT((_DMAX << _DOFF) | (1 << (_DOFF - 1))) };
const __fpconst __eps = { INIT((_DBIAS - NBITS - 1) << _DOFF) };
const __fpconst __rteps = { INIT((_DBIAS - NBITS / 2) << _DOFF) };

const double __xbig = (NBITS + 1) * 347L / 1000;
const double __zero = 0.0;

