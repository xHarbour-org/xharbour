/****************************************************************************
 *                                                                          *
 * File    : xxilogb.h                                                      *
 *                                                                          *
 * Purpose : Common ilogb[fl] functionality.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <float.h>
#include <limits.h>
#include "xmath.h"

#if FLT_RADIX != 2
#error correct only for FLT_RADIX == 2
#endif

/* compute ilogb(x) */
int __cdecl FFUN(ilogb)(FTYPE x)
{
    short xexp;

    switch (FNAME(fpunscale)(&xexp, &x))
    {
        /* test for special codes */
        case FP_NAN:
            return FP_ILOGBNAN;

        case 0:
            return FP_ILOGB0;

        case FP_INFINITE:
            return INT_MAX;  /* INF */

        default:  /* finite */
            return --xexp;
    }
}

