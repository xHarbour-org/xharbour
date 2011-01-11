/****************************************************************************
 *                                                                          *
 * File    : _fpint.c                                                       *
 *                                                                          *
 * Purpose : __fpint function -- IEEE 754 version.                          *
 *                                                                          *
 * Comment : Clear fraction bits and test whether any bits got cleared.     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-02  Modified for C99.                                    *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* test and drop (scaled) fraction bits */
short __fpint(double *px, short xexp)
{
    unsigned short *ps = (unsigned short *)px;
    unsigned short frac;
    short xchar = (ps[_D0] & _DMASK) >> _DOFF;

    if (xchar == _DMAX)
        return ((ps[_D0] & _DFRAC) == 0 && ps[_D1] == 0 && ps[_D2] == 0 && ps[_D3] == 0) ? FP_INFINITE : FP_NAN;
    else if ((ps[_D0] & ~_DSIGN) == 0 && ps[_D1] == 0 && ps[_D2] == 0 && ps[_D3] == 0)
        return 0;

    xchar = (_DBIAS + 48 + _DOFF + 1) - xchar - xexp;
    if (xchar <= 0)
    {
        /* no frac bits to drop */
        return 0;
    }
    else if ((48 + _DOFF + 1) <= xchar)
    {
        /* all frac bits */
        ps[_D0] &= _DSIGN;
        ps[_D1] = 0;
        ps[_D2] = 0;
        ps[_D3] = 0;
        return FP_NORMAL;  /* report on frac, not result */
    }
    else
    {
        /* strip out frac bits */
        static const unsigned short mask[] = {
            0x0000, 0x0001, 0x0003, 0x0007,
            0x000f, 0x001f, 0x003f, 0x007f,
            0x00ff, 0x01ff, 0x03ff, 0x07ff,
            0x0fff, 0x1fff, 0x3fff, 0x7fff
        };
        static const size_t sub[] = {_D3, _D2, _D1, _D0};

        frac = mask[xchar & 0xf];
        xchar >>= 4;
        frac &= ps[sub[xchar]];
        ps[sub[xchar]] ^= frac;

        switch (xchar)
        {
            /* cascade through! */
            case 3: frac |= ps[_D1], ps[_D1] = 0;
            case 2: frac |= ps[_D2], ps[_D2] = 0;
            case 1: frac |= ps[_D3], ps[_D3] = 0;
        }

        return (frac != 0) ? FP_NORMAL : 0;
    }
}

