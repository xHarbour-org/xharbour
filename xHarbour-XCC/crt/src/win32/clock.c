/****************************************************************************
 *                                                                          *
 * File    : clock.c                                                        *
 *                                                                          *
 * Purpose : clock function -- win32 version.                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <time.h>

static unsigned long long start_tics;

/* initialize CPU time during startup */
void __clockinit(void)
{
    GetSystemTimeAsFileTime((FILETIME *)&start_tics);  /* oops! */
}


/* return CPU time */
clock_t __cdecl (clock)(void)
{
    unsigned long long current_tics;

    GetSystemTimeAsFileTime((FILETIME *)&current_tics);  /* oops! */

    /* calculate elapsed number of 100-nanosecond units */
    current_tics -= start_tics;

    /* return number of elapsed milliseconds */
    return (clock_t)(current_tics / 10000);
}


