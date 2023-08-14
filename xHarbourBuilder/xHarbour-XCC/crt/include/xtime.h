#ifndef _XTIME_H
#define _XTIME_H

#include <xtinfo.h>

/* xtime.h - internal header */

/* macros */
#define WDAY  1   /* to get day of week right */
#define _TBIAS  ((70 * 365LU + 17) * 86400)

/* type definitions */
typedef struct {  /* rule for daylight savings time */
    unsigned char wday, hour, day, mon, year;
} __dst;

/* internal declarations */
int __extradays(int, int);
const char *__gentime(const struct tm *, const __timeinfo *, char, char, int *, char *);
__dst *__getdst(const char *);
const char *__gettime(const char *, int, int *);
int __isdst(const struct tm *);
const char *__getzone(void);
struct tm *__ttotm(struct tm *, time_t, int);
time_t __tzoff(void);
char *__tzone(void);

#endif /* _XTIME_H */

