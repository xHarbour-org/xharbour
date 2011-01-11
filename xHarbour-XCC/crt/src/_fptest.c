/****************************************************************************
 *                                                                          *
 * File    : _fptest.c                                                      *
 *                                                                          *
 * Purpose : __fptest function -- IEEE 754 version.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* categorize *px */
short __cdecl __fptest(double *px)
{
    unsigned short *ps = (unsigned short *)px;

    if ((ps[_D0] & _DMASK) == _DMAX << _DOFF)
        return ((ps[_D0] & _DFRAC) != 0 || ps[_D1] != 0 || ps[_D2] != 0 || ps[_D3] != 0) ? FP_NAN : FP_INFINITE;
    else if ((ps[_D0] & ~_DSIGN) != 0 || ps[_D1] != 0 || ps[_D2] != 0 || ps[_D3] != 0)
        return FP_NORMAL;
    else
        return 0;
}

