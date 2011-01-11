/****************************************************************************
 *                                                                          *
 * File    : _ttotm.c                                                       *
 *                                                                          *
 * Purpose : __ttotm and __extradays functions.                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-10  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include "xtime.h"
#include "xthread.h"
#include "xalloc.h"

/* macros */
#define MONTAB(year)  ((year) & 03 || (year) == 0 ? mos : lmos)

/* static data */
static const short lmos[] = { 0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335 };
static const short mos[] = { 0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334 };

/* compute extra days to start of month */
int __extradays(int year, int mon)
{
    int days;

    if (year > 0)  /* correct for leap year: 1801-2099 */
        days = (year - 1) / 4;
    else if (year <= -4)
        days = 1 + (4 - year) / 4;
    else
        days = 0;

    return days + MONTAB(year)[mon];
}

/* convert scalar time to time structure */
struct tm *__ttotm(struct tm *t, time_t secsarg, int isdst)
{
    int year;
    long days;
    unsigned long secs;

    secsarg += _TBIAS;  /* changed to (wraparound) time since 1 Jan 1900 */

    if (t == 0)
#ifdef __MT__
    {
        tiddata *mtd = __get_mtd();
        if (!mtd->timebuf) mtd->timebuf = (struct tm *)malloc(sizeof(struct tm));
        t = (struct tm *)mtd->timebuf;
    }
#else /* __MT__ */
    {
        static struct tm ts = {0};
        t = &ts;
    }
#endif /* __MT__ */

    t->tm_isdst = isdst;

    for (secs = secsarg; ; secs = secsarg + 3600)
    {
        /* loop to correct for DST */
        days = secs / 86400;
        t->tm_wday = (days + WDAY) % 7;

        /* determine year */
        {
            long i;

            for (year = days / 365; days < (i = __extradays(year, 0) + 365L * year); )
                --year;  /* correct guess and recheck */
            days -= i;
            t->tm_year = year;
            t->tm_yday = days;
        }

        /* determine month */
        {
            int mon;
            const short *pm = MONTAB(year);

            for (mon = 12; days < pm[--mon]; )
                ;
            t->tm_mon = mon;
            t->tm_mday = days - pm[mon] + 1;
        }

        secs = secs % 86400;
        t->tm_hour = secs / 3600;
        secs %= 3600;
        t->tm_min = secs / 60;
        t->tm_sec = secs % 60;
        if (t->tm_isdst >= 0 || (t->tm_isdst = __isdst(t)) <= 0)
            return t;  /* loop only if <0 => 1 */
    }
}

