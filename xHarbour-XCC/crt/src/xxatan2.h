/****************************************************************************
 *                                                                          *
 * File    : xxatan2.h                                                      *
 *                                                                          *
 * Purpose : Common atan2[fl] functionality.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-29  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* compute atan(y/x) */
FTYPE __cdecl (FFUN(atan2))(FTYPE  y, FTYPE x)
{
    FTYPE z;
    const short errx = FNAME(fptest)(&x);
    const short erry = FNAME(fptest)(&y);
    unsigned short hex;

    if (errx <= 0 && erry <= 0)
    {
        /* x & y both finite or 0 */
        if (FISNEG(y))
            y = -y, hex = 0x8;
        else
            hex = 0x0;

        if (FISNEG(x))
            x = -x, hex ^= 0x6;

        if (x < y)
            z = x / y, hex ^= 0x2;
        else if (errx < 0)
            z = y / x;
        else
            z = FLIT(0.0);
    }
    else if (erry == FP_NAN)
        return y;
    else if (errx == FP_NAN)
        return x;
    else
    {
        /* at least one FP_INFINITE */
        z = (errx == erry) ? FLIT(1.0) : FLIT(0.0);
        hex = FISNEG(y) ? 0x8 : 0x0;
        if (FISNEG(x))
            hex ^= 0x6;
        if (erry == FP_INFINITE)
            hex ^= 0x2;
    }

    return (FNAME(atan)(z, hex));
}

