/****************************************************************************
 *                                                                          *
 * File    : div.c                                                          *
 *                                                                          *
 * Purpose : div function.                                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>

/* compute int quotient and remainder */
div_t __cdecl (div)(int numer, int denom)
{
    static const int fixneg = -1 / 2;
    div_t val;

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

