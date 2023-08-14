/****************************************************************************
 *                                                                          *
 * File    : copysignl.c                                                    *
 *                                                                          *
 * Purpose : copysignl function [new C99].                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-08-29  Rewritten                                            *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* copy sign from y to x */
long double __cdecl (copysignl)(long double x, long double y)
{
    unsigned short *const px = (unsigned short *)&x + _L0;
    unsigned short *const py = (unsigned short *)&y + _L0;

    *px = *px & ~_LSIGN | *py & _LSIGN;
    return x;
}

