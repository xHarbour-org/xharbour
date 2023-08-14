/****************************************************************************
 *                                                                          *
 * File    : _fpintl.c                                                      *
 *                                                                          *
 * Purpose : __fpintl function -- IEEE 754 version.                         *
 *                                                                          *
 * Comment : Clear fraction bits and test whether any bits got cleared.     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-06  Rewritten                                            *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* test and drop (scaled) fraction bits -- 64-bit */
short __fpintl(long double *px, short xexp)
{
    return __fpint((double *)px, xexp);
}

#if 0
/* test and drop (scaled) fraction bits -- 80-bit */
short _LDint(long double *px, short xexp)
{
    unsigned short *ps = (unsigned short *)px;
    short xchar = ps[_L0] & _LMASK;

    if (xchar == _LMAX)
        return ((ps[_L1] & 0x7fff) == 0 && ps[_L2] == 0 && ps[_L3] == 0 && ps[_L4] == 0) ? FP_INFINITE : FP_NAN;
    else if (ps[_L1] == 0 && ps[_L2] == 0 && ps[_L3] == 0 && ps[_L4] == 0)
        return 0;

    xchar = (_LBIAS + 64) - xchar - xexp;
    if (xchar <= 0)
        return 0;  /* no frac bits to drop */
    else if (xchar >= 64)
    {
        /* all frac bits */
        ps[_L0] &= _LSIGN;
        ps[_L1] = 0;
        ps[_L2] = 0;
        ps[_L3] = 0;
        ps[_L4] = 0;
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
        static const size_t sub[] = { _L4, _L3, _L2, _L1 };
        unsigned short frac = mask[xchar & 0xf];

        xchar >>= 4;
        frac &= ps[sub[xchar]];
        ps[sub[xchar]] ^= frac;

        switch (xchar)
        {
            /* cascade through! */
            case 3:
                frac |= ps[_L2], ps[_L2] = 0;
            case 2:
                frac |= ps[_L3], ps[_L3] = 0;
            case 1:
                frac |= ps[_L4], ps[_L4] = 0;
        }

        return (frac != 0) ? FP_NORMAL : 0;
    }
}
#endif

