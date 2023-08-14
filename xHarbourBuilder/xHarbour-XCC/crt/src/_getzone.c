/****************************************************************************
 *                                                                          *
 * File    : _getzone.c                                                     *
 *                                                                          *
 * Purpose : __getzone function.                                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-02  Modified for C99.                                    *
 *           03-06-26  Try to use the correct time-zone.                    *
 *                                                                          *
 ****************************************************************************/

#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>
#include "xtime.h"

/* static const char *defzone = ":";  see: win32\_tzone.c */
static char *tzone = 0;

/* EST: Eastern Standard Time = UTC - 5h */
/* EDT: Eastern Daylight Saving Time = UTC - 4h */

/* reformat TZ from EST-05EDT */
static char *reformat(const char *s)
{
    static char tzbuf[] = ":EST:EDT:-0500";
    int i;

    for (i = 1; i <= 3; i++)
    {
        if (isalpha(*s))
            tzbuf[i] = *s, tzbuf[i + 4] = *s++;
        else
            return 0;
    }

    tzbuf[9] = (*s == '-' || *s == '+') ? *s++ : '+';

    if (!isdigit(*s))
        return 0;

    tzbuf[10] = *s++;

    if (!isdigit(*s))
        return 0;

    tzbuf[11] = *s++;

    if (isalpha(*s))
    {
        for (i = 5; i <= 7; i++)
        {
            if (isalpha(*s))
                tzbuf[i] = *s++;
            else
                return 0;
        }
    }

    return (*s == '\0') ? tzbuf : 0;
}

/* get time zone information */
const char *__getzone(void)
{
    static int init = 0;
    if (!init)
    {
        init = 1;

        if ((tzone = getenv("TIMEZONE")) != 0)
            ;
        else if ((tzone = getenv("TZ")) != 0)
            tzone = reformat(tzone);

        if (tzone == 0)
            tzone = __tzone();  /* 03-06-26 */
            /* tzone = (char *)defzone; */
    }
    return tzone;
}
