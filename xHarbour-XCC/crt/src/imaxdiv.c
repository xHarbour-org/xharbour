/****************************************************************************
 *                                                                          *
 * File    : imaxdiv.c                                                      *
 *                                                                          *
 * Purpose : imaxdiv function [new C99].                                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stddef.h>
#include <inttypes.h>

/* compute intmax_t quotient and remainder */
imaxdiv_t __cdecl (imaxdiv)(intmax_t numer, intmax_t denom)
{
    static const int fixneg = -1 / 2;
    imaxdiv_t val;

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

