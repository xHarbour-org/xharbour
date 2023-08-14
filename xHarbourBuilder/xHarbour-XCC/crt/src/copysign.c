/****************************************************************************
 *                                                                          *
 * File    : copysign.c                                                     *
 *                                                                          *
 * Purpose : copysign function [new C99].                                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-08-29  Rewritten                                            *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* copy sign from y to x */
double __cdecl (copysign)(double x, double y)
{
    unsigned short *const px = (unsigned short *)&x + _D0;
    unsigned short *const py = (unsigned short *)&y + _D0;

    *px = *px & ~_DSIGN | *py & _DSIGN;
    return x;
}

