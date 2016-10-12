/****************************************************************************
 *                                                                          *
 * File    : _fptestf.c                                                     *
 *                                                                          *
 * Purpose : __fptestf function -- IEEE 754 version.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* categorize *px */
short __cdecl __fptestf(float *px)
{
    unsigned short *ps = (unsigned short *)px;

    if ((ps[_F0] & _FMASK) == _FMAX << _FOFF)
        return ((ps[_F0] & _FFRAC) != 0 || ps[_F1] != 0) ? FP_NAN : FP_INFINITE;
    else if ((ps[_F0] & ~_FSIGN) != 0 || ps[_F1] != 0)
        return FP_NORMAL;
    else
        return 0;
}

