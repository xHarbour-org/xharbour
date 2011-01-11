/****************************************************************************
 *                                                                          *
 * File    : copysignf.c                                                    *
 *                                                                          *
 * Purpose : copysignf function [new C99].                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-08-29  Rewritten                                            *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* copy sign from y to x */
float __cdecl (copysignf)(float x, float y)
{
    unsigned short *const px = (unsigned short *)&x + _F0;
    unsigned short *const py = (unsigned short *)&y + _F0;

    *px = *px & ~_FSIGN | *py & _FSIGN;
    return x;
}

