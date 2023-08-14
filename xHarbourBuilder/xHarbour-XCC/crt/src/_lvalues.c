/****************************************************************************
 *                                                                          *
 * File    : _lvalues.c                                                     *
 *                                                                          *
 * Purpose : Values used by math functions -- IEEE 754 long version.        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-10  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* macros -- 64-bit */
#define NBITS  (48 + _DOFF)
#if _D0 == 0
#define INIT(w0)       { w0, 0, 0, 0 }
#define INIT2(w0, w1)  { w0, 0, 0, w1 }
#else
#define INIT(w0)       { 0, 0, 0, w0 }
#define INIT2(w0, w1)  { w1, 0, 0, w0 }
#endif

/* static data */
const __fpconst __infl = { INIT(_DMAX << _DOFF) };
const __fpconst __nanl = { INIT((_DMAX << _DOFF) | (1 << (_DOFF - 1))) };
const __fpconst __epsl = { INIT((_DBIAS - NBITS - 1) << _DOFF) };
const __fpconst __rtepsl = { INIT((_DBIAS - NBITS / 2) << _DOFF) };

#if 0
/* macros -- 80-bit */
#define NBITS 64
#if _D0 == 0
#define INIT(w0, w1)       { w0, w1, 0, 0, 0 }
#define INIT3(w0, w1, wn)  { w0, w1, 0, 0, wn }
#else
#define INIT(w0, w1)       { 0, 0, 0, w1, w0 }
#define INIT3(w0, w1, wn)  { wn, 0, 0, w1, w0 }
#endif

/* static data */
const __fpconst __infl = { INIT(_LMAX, 0x8000) };
const __fpconst __nanl = { INIT(_LMAX, 0xc000) };
const __fpconst __epsl = { INIT(_LBIAS - NBITS - 1, 0x8000) };
const __fpconst __rtepsl = { INIT(_LBIAS - NBITS / 2, 0x8000) };
#endif

const long double __xbigl = (NBITS + 1) * 347L / 1000;
const long double __zerol = 0.0L;

