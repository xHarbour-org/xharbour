#ifndef _XTINFO_H
#define _XTINFO_H

#include <time.h>

/* xtinfo.h - internal header */

/* type definitions */
typedef struct {  /* format strings for date and time */
    const char *am_pm;
    const char *days;
    const char *abday;
    const char *day;
    const char *months;
    const char *abmon;
    const char *mon;
    const char *formats;
    const char *d_t_fmt;
    const char *d_fmt;
    const char *t_fmt;
    const char *t_fmt_ampm;
    const char *era_formats;
    const char *era_d_t_fmt;
    const char *era_d_fmt;
    const char *era_t_fmt;
    const char *era_t_fmt_ampm;
    const char *era;
    const char *alt_digits;
    const char *isdst;
    const char *tzone;
} __timeinfo;

/* declarations */
size_t __strftime(char *, size_t, const char *, const struct tm *, const __timeinfo *);

/* data declarations */
extern __timeinfo __times;

#endif /* _XTINFO_H */

