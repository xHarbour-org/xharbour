/****************************************************************************
 *                                                                          *
 * File    : lldiv.c                                                        *
 *                                                                          *
 * Purpose : lldiv function [new C99].                                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>
#include <stdio.h>

/* compute long long quotient and remainder */
lldiv_t __cdecl (lldiv)(long long numer, long long denom)
{
    static const int fixneg = -1 / 2;
    lldiv_t val;

    val.quot = numer / denom;
    val.rem = numer - denom * val.quot;
    if (fixneg < 0 && val.quot < 0 && val.rem != 0)
    {
        /* fix incorrect truncation */
        val.quot += 1;
        val.rem -= denom;
    }

    return val;
}

