/****************************************************************************
 *                                                                          *
 * File    : _setlocale.c                                                   *
 *                                                                          *
 * Purpose : __setlocale function.                                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-06  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <ctype.h>
#include "xlocale.h"

/* set category for locale */
__locinfo *__setlocale(int cat, __locinfo *p)
{
    switch (cat)
    {
        /* set a category */
        case LC_COLLATE:
            __collatetab = p->collatetab;
            __wcollatetab = p->wcollatetab;
            break;

        case LC_CTYPE:
            __ctypetab = p->ctypetab;
            __tolowertab = p->tolowertab;
            __touppertab = p->touppertab;
            __mbcurmax = (p->mbcurmax <= MB_LEN_MAX) ? p->mbcurmax : MB_LEN_MAX;
            __mbtowctab = p->mbtowctab;
            __wctombtab = p->wctombtab;
            __wctranstab = p->wctranstab;
            __wctypetab = p->wctypetab;
            break;

        case LC_MONETARY:
            /* set monetary category */
            __locale.currency_symbol = p->loc.currency_symbol;
            __locale.int_curr_symbol = p->loc.int_curr_symbol;
            __locale.mon_decimal_point = p->loc.mon_decimal_point;
            __locale.mon_grouping = p->loc.mon_grouping;
            __locale.mon_thousands_sep = p->loc.mon_thousands_sep;
            __locale.negative_sign = p->loc.negative_sign;
            __locale.positive_sign = p->loc.positive_sign;

            __locale.frac_digits = p->loc.frac_digits;
            __locale.n_cs_precedes = p->loc.n_cs_precedes;
            __locale.n_sep_by_space = p->loc.n_sep_by_space;
            __locale.n_sign_posn = p->loc.n_sign_posn;
            __locale.p_cs_precedes = p->loc.p_cs_precedes;
            __locale.p_sep_by_space = p->loc.p_sep_by_space;
            __locale.p_sign_posn = p->loc.p_sign_posn;

            __locale.int_frac_digits = p->loc.int_frac_digits;
            __locale.int_n_cs_precedes = p->loc.int_n_cs_precedes;
            __locale.int_n_sep_by_space = p->loc.int_n_sep_by_space;
            __locale.int_n_sign_posn = p->loc.int_n_sign_posn;
            __locale.int_p_cs_precedes = p->loc.int_p_cs_precedes;
            __locale.int_p_sep_by_space = p->loc.int_p_sep_by_space;
            __locale.int_p_sign_posn = p->loc.int_p_sign_posn;
            break;

        case LC_NUMERIC:
            /* set numeric category */
            __locale.decimal_point = (p->loc.decimal_point[0] != '\0') ? p->loc.decimal_point : (char *)".";
            __locale.grouping = p->loc.grouping;
            __locale.thousands_sep = p->loc.thousands_sep;
            break;

        case LC_TIME:
            __times = p->times;
            break;
    }

    return p;
}

