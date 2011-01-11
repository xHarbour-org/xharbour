/****************************************************************************
 *                                                                          *
 * File    : difftime.c                                                     *
 *                                                                          *
 * Purpose : difftime function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <time.h>
#include "xtime.h"

/* compute difference in times */
double __cdecl (difftime)(time_t t1, time_t t0)
{
    t0 -= _TBIAS, t1 -= _TBIAS;
    return (t0 <= t1) ? (double)(t1 - t0) : -(double)(t0 - t1);
}

