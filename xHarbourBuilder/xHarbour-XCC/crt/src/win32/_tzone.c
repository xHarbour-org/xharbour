/****************************************************************************
 *                                                                          *
 * File    : _tzone.c                                                       *
 *                                                                          *
 * Purpose : Return current time-zone -- win32 version.                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           03-06-26  Created                                              *
 *                                                                          *
 ****************************************************************************/

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <stdlib.h>
#include <stdio.h>
#include "xtime.h"

/* return the current time-zone */
char *__tzone(void)
{
    static char tzbuf[] = ":GMT:GMT:+0000";
    TIME_ZONE_INFORMATION tzi;
    long tzoff;

    switch (GetTimeZoneInformation(&tzi))
    {
        case TIME_ZONE_ID_UNKNOWN:
        default:
            return ":";  /* see defzone in _getzone.c */

        case TIME_ZONE_ID_STANDARD:
            tzoff = - tzi.Bias / 60;
            break;

        case TIME_ZONE_ID_DAYLIGHT:
            tzoff = - (tzi.Bias + tzi.DaylightBias) / 60;
            break;
    }

    sprintf(tzbuf + 9, "%+04.4ld", tzoff * 100);

    return tzbuf;
}
