#ifndef _LOCALE_H
#define _LOCALE_H

/* locale.h - C99 standard header */

/* macros */
#ifndef NULL
#define NULL  ((void *)0)
#endif

/* locale categories */
#define LC_ALL       (LC_COLLATE|LC_CTYPE|LC_MONETARY|LC_NUMERIC|LC_TIME)
#define LC_COLLATE   1
#define LC_CTYPE     2
#define LC_MONETARY  4
#define LC_NUMERIC   8
#define LC_TIME      16

/* type definitions */
struct lconv {
    /* LC_MONETARY */
    char *currency_symbol;
    char *int_curr_symbol;
    char *mon_decimal_point;
    char *mon_grouping;
    char *mon_thousands_sep;
    char *negative_sign;
    char *positive_sign;
    char frac_digits;
    char n_cs_precedes;
    char n_sep_by_space;
    char n_sign_posn;
    char p_cs_precedes;
    char p_sep_by_space;
    char p_sign_posn;
    char int_frac_digits;
    char int_n_cs_precedes;
    char int_n_sep_by_space;
    char int_n_sign_posn;
    char int_p_cs_precedes;
    char int_p_sep_by_space;
    char int_p_sign_posn;
    /* LC_NUMERIC */
    char *decimal_point;
    char *grouping;
    char *thousands_sep;
};

/* declarations */
struct lconv * __cdecl localeconv(void);
char * __cdecl setlocale(int, const char *);

/* data declarations */
extern struct lconv __locale;

/* macro overrides */
#define localeconv()  (&__locale)

#endif /* _LOCALE_H */

