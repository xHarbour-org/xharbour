/****************************************************************************
 *                                                                          *
 * File    : _fptestl.c                                                     *
 *                                                                          *
 * Purpose : __fptestl function -- IEEE 754 version.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-06  Rewritten.                                           *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* categorize *px -- 64-bit */
short __cdecl __fptestl(long double *px)
{
    return __fptest((double *)px);
}

#if 0
/* categorize *px -- 80-bit */
short __cdecl __fptestl(long double *px)
{
    unsigned short *ps = (unsigned short *)px;
    short xchar = ps[_L0] & _LMASK;

    if (xchar == _LMAX)
        return ((ps[_L1] & 0x7fff) != 0 || ps[_L2] != 0 || ps[_L3] != 0 || ps[_L4] != 0) ? FP_NAN : FP_INFINITE;
    else if (xchar > 0 || ps[_L1] != 0 || ps[_L2] || ps[_L3] || ps[_L4])
        return FP_NORMAL;
    else
        return 0;
}
#endif
