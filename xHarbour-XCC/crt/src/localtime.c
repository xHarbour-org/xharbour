/****************************************************************************
 *                                                                          *
 * File    : localtime.c                                                    *
 *                                                                          *
 * Purpose : localtime function.                                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-05  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>
#include "xtime.h"

#define MAXTZ  (100 * 13)   /* biggest valid HHMM offset from UTC */

typedef const char *old_t;

/* determine local time offset */
time_t __tzoff(void)
{
    static old_t oldzone = 0;
    static long tzoff = 0;

    if (oldzone != __times.tzone)
    {
        /* determine time zone offset (East is +) */
        const char *p, *pe;
        int n;

        if (__times.tzone[0] == '\0')
            __times.tzone = __getzone();

        p = __gettime(__times.tzone, 2, &n);

        tzoff = strtol(p, (char **)&pe, 10);
        if (pe - p != n || tzoff <= -MAXTZ || MAXTZ <= tzoff)
            tzoff = 0;
        tzoff -= (tzoff / 100) * 40;  /* HHMM -- changed for C9X */
        tzoff = -tzoff;               /* also change sense of offset for C9X */
        oldzone = __times.tzone;
    }

    return -tzoff * 60;
}

/* convert to local time structure */
struct tm * __cdecl (localtime)(const time_t *tod)
{
    return __ttotm(0, *tod + __tzoff(), -1);
}

