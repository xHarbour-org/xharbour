/****************************************************************************
 *                                                                          *
 * File    : mktime.c                                                       *
 *                                                                          *
 * Purpose : mktime function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-03  Modified for C99.                                    *
 *                                                                          *
 ****************************************************************************/

#include <limits.h>
#include "xtime.h"

/* convert local time structure to scalar time */
time_t __cdecl (mktime)(struct tm *t)
{
    double dsecs;
    int mon, year, ymon;
    time_t secs;

    ymon = t->tm_mon / 12;
    mon = t->tm_mon - ymon * 12;
    if (mon < 0)
        mon += 12, --ymon;
    if (ymon < 0 && t->tm_year < INT_MIN - ymon || ymon > 0 && INT_MAX - ymon < t->tm_year)
        return (time_t)(-1);

    year = t->tm_year + ymon;
    dsecs = 86400.0 * (__extradays(year, mon) - 1) + 31536000.0 * year + 86400.0 * t->tm_mday;
    dsecs += 3600.0 * t->tm_hour + 60.0 * t->tm_min + (double)t->tm_sec;
    if (dsecs < 0.0)
        return (time_t)(-1);

    secs = (time_t)(dsecs - (double)_TBIAS);
    __ttotm(t, secs, t->tm_isdst);
    if (t->tm_isdst > 0)
        secs -= 3600;

    return secs - __tzoff();
}

