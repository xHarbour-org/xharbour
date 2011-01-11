/****************************************************************************
 *                                                                          *
 * File    : localeconv.c                                                   *
 *                                                                          *
 * Purpose : localeconv function.                                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-12  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <limits.h>
#include <locale.h>

/* static data */
static const char null[] = "";

extern struct lconv __locale = {
    /* LC_MONETARY */
    (char *)null,       /* currency_symbol */
    (char *)null,       /* int_curr_symbol */
    (char *)null,       /* mon_decimal_point */
    (char *)null,       /* mon_grouping */
    (char *)null,       /* mon_thousands_sep */
    (char *)null,       /* negative_sign */
    (char *)null,       /* positive_sign */

    CHAR_MAX,           /* frac_digits */
    CHAR_MAX,           /* n_cs_precedes */
    CHAR_MAX,           /* n_sep_by_space */
    CHAR_MAX,           /* n_sign_posn */
    CHAR_MAX,           /* p_cs_precedes */
    CHAR_MAX,           /* p_sep_by_space */
    CHAR_MAX,           /* p_sign_posn */

    CHAR_MAX,           /* int_frac_digits */
    CHAR_MAX,           /* int_n_cs_precedes */
    CHAR_MAX,           /* int_n_sep_by_space */
    CHAR_MAX,           /* int_n_sign_posn */
    CHAR_MAX,           /* int_p_cs_precedes */
    CHAR_MAX,           /* int_p_sep_by_space */
    CHAR_MAX,           /* int_p_sign_posn */

    /* LC_NUMERIC */
    ".",                /* decimal_point */
    (char *)null,       /* grouping */
    (char *)null,       /* thousands_sep */
};

/* get pointer to current locale */
struct lconv * __cdecl (localeconv)(void)
{
    return &__locale;
}

