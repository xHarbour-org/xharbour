#ifndef _TIME_H
#define _TIME_H

/* time.h - C99 standard header */

/* macros */
#ifndef NULL
#define NULL  ((void *)0)
#endif

#define CLOCKS_PER_SEC  1000

/* type definitions */
#ifndef _SIZE_T_DEFINED
#define _SIZE_T_DEFINED
typedef unsigned int size_t;
#endif

#ifndef _TIME_T_DEFINED
#define _TIME_T_DEFINED
typedef unsigned long time_t;
#endif

#ifndef _CLOCK_T_DEFINED
#define _CLOCK_T_DEFINED
typedef unsigned int clock_t;
#endif

struct tm {
    int tm_sec;     /* seconds after the minute [0,60] */
    int tm_min;     /* minutes after the hour [0,59] */
    int tm_hour;    /* hours since midnight [0,23] */
    int tm_mday;    /* day of the month [1,31] */
    int tm_mon;     /* month since January [0,11] */
    int tm_year;    /* years since 1900 */
    int tm_wday;    /* days since Sunday [0,6] */
    int tm_yday;    /* days since January 1 [0,365] */
    int tm_isdst;   /* Daylight Saving Time flag */
};

/* declarations */
char * __cdecl asctime(const struct tm *);
clock_t __cdecl clock(void);
char * __cdecl ctime(const time_t *);
double __cdecl difftime(time_t, time_t);
struct tm * __cdecl gmtime(const time_t *);
struct tm * __cdecl localtime(const time_t *);
time_t __cdecl mktime(struct tm *);
size_t __cdecl strftime(char * restrict, size_t, const char * restrict, const struct tm * restrict);
time_t __cdecl time(time_t *);

/* private extensions to standard C */
char * __cdecl _strdate(char *);
char * __cdecl _strtime(char *);

#ifndef _M_ARM
unsigned long long __cdecl _rdtsc(void);
#endif /* _M_ARM */

#endif /* _TIME_H */
