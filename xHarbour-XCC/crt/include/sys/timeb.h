#ifndef _TIMEB_H
#define _TIMEB_H

/* sys/timeb.h - private header for _ftime() definitions */

/* type definitions */
#ifndef _TIME_T_DEFINED
#define _TIME_T_DEFINED
typedef unsigned long time_t;
#endif

#ifndef _TIMEB_DEFINED
#define _TIMEB_DEFINED
struct _timeb {
    time_t time;
    unsigned short millitm;
    short timezone;
    short dstflag;
};
#endif /* _TIMEB_DEFINED */

/* declarations */
void __cdecl _ftime(struct _timeb *);

#endif /* _TIMEB_H */
