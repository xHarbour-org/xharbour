/****************************************************************************
 *                                                                          *
 * File    : _isdst.c                                                       *
 *                                                                          *
 * Purpose : __isdst function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-09  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>
#include "xtime.h"

typedef const char *dst_t;
typedef __dst *rules_t;

/* test whether Daylight Savings Time in effect */
int __isdst(const struct tm *t)
{
    static dst_t olddst = 0;
    static rules_t rules = 0;
    __dst *pr;

    if (olddst != __times.isdst)
    {
        /* find current dst_rules */
        if (__times.isdst[0] == '\0')
        {
            /* look beyond time_zone info */
            int n;

            if (__times.tzone[0] == '\0')
                __times.tzone = __getzone();

            __times.isdst = __gettime(__times.tzone, 3, &n);

            if (__times.isdst[0] != '\0')
                --__times.isdst;  /* point to delimiter */
        }

        if ((pr = __getdst(__times.isdst)) == 0)
            return -1;

        free(rules);
        rules = pr;
        olddst = __times.isdst;
    }

    /* check time against rules */
    {
        int ans = 0;
        const int d0 = __extradays(t->tm_year, 0);
        const int hour = t->tm_hour + 24 * t->tm_yday;
        const int wd0 = (365L * t->tm_year + d0 + WDAY) % 7 + 14;

        for (pr = rules; pr->wday != (unsigned char)-1; ++pr)
        {
            if (pr->year <= t->tm_year)
            {
                /* found early enough year */
                int rday = __extradays(t->tm_year, pr->mon) - d0 + pr->day;

                if (pr->wday > 0)
                {
                    /* shift to specific weekday */
                    int wd = (rday + wd0 - pr->wday) % 7;

                    rday += (wd == 0) ? 0 : 7 - wd;
                    if (pr->wday <= 7)
                        rday -= 7;  /* strictly before */
                }

                if (hour < rday * 24 + pr->hour)
                    return ans;

                ans = pr->year == (pr + 1)->year ? !ans : 0;
            }
        }

        return ans;
    }
}

