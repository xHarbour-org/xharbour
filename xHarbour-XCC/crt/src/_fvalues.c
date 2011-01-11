/****************************************************************************
 *                                                                          *
 * File    : _fvalues.c                                                     *
 *                                                                          *
 * Purpose : Values used by math functions -- IEEE 754 version.             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/*  3         2         1
 * 10987654321098765432109876543210
 * SEEEEEEEEMMMMMMMMMMMMMMMMMMMMMMM
 * --------++++++++--------++++++++
 */

/* macros */
#define NBITS  (16 + _FOFF)

#if _D0 == 0
#define INIT(w0)        {w0,0}
#define INIT2(w0, w1)   {w0,w1}
#else
#define INIT(w0)        {0,w0}
#define INIT2(w0, w1)   {w1,w0}
#endif

/* static data */
// const __fpconst _FDenorm = { INIT2(0, 1) };
const __fpconst __inff = { INIT(_FMAX << _FOFF) };
const __fpconst __nanf = { INIT((_FMAX << _FOFF) | (1 << (_FOFF-1))) };
const __fpconst __epsf = { INIT((_FBIAS - NBITS - 1) << _FOFF) };
const __fpconst __rtepsf = { INIT((_FBIAS - NBITS / 2) << _FOFF) };

const float __xbigf = (NBITS + 1) * 347L / 1000;
const float __zerof = 0.0F;

