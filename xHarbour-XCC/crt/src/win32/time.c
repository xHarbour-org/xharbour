/****************************************************************************
 *                                                                          *
 * File    : time.c                                                         *
 *                                                                          *
 * Purpose : time function.                                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           03-06-26  Rewritten. Didn't work with time-zones.              *
 *                                                                          *
 ****************************************************************************/

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <time.h>

/* number of 100 nanosecond units from 1/1/1601 to 1/1/1970 */
#define EPOCH_BIAS  116444736000000000LL

/* return the system time */
time_t __cdecl (time)(time_t *tod)
{
    unsigned long long tics;
    time_t t;

    GetSystemTimeAsFileTime((FILETIME *)&tics);  /* oops! */

    t = (time_t)((tics - EPOCH_BIAS) / 10000000LL);
    if (tod) *tod = t;

    return t;
}

