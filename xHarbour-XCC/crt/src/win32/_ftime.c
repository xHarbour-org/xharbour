/****************************************************************************
 *                                                                          *
 * File    : _ftime.c                                                       *
 *                                                                          *
 * Purpose : _ftime function -- win32 version.                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           03-06-26  Created                                              *
 *                                                                          *
 ****************************************************************************/

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <sys/types.h>
#include <sys/timeb.h>
#include "xtime.h"

/* number of 100 nanosecond units from 1/1/1601 to 1/1/1970 */
#define EPOCH_BIAS  116444736000000000LL

/* return the system date/time in a structure form */
void __cdecl (_ftime)(struct _timeb *tb)
{
    unsigned long long tics;

    GetSystemTimeAsFileTime((FILETIME *)&tics);  /* oops! */

    tb->time = (time_t)((tics - EPOCH_BIAS) / 10000000LL);
    tb->millitm = (unsigned short)((tics / 10000LL) % 1000LL);
    tb->timezone = (short)(__tzoff() / 60);
    tb->dstflag = -1;  // ?
}
