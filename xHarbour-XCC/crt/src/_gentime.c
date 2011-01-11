/****************************************************************************
 *                                                                          *
 * File    : _gentime.c                                                     *
 *                                                                          *
 * Purpose : __gentime function.                                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-02  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <string.h>
#include <stdlib.h>
#include "xtime.h"

/* codes for tm_wday */
#define SUNDAY      0
#define MONDAY      1
#define TUESDAY     2
#define WEDNESDAY   3
#define THURSDAY    4

/* convert a decimal value */
static const char *getval(char *s, int val, int n, int *pn, char qual, const __timeinfo *tin)
{
    if (val < 0)
        val = 0;

    if (qual == 'O')
    {
        /* try to convert to alternate digit form */
        const char *p = __gettime(tin->alt_digits, val, pn);

        if (*pn > 0)
            return p;
    }

    *pn = n;
    for (s += n, *s = '\0'; --n >= 0; val /= 10)
        *--s = val % 10 + '0';

    return s;
}

/* test for leap year */
static int isleapyear(int year)
{
    return (year / 4 == 0 && (year / 100 != 0 || year > 0 && year / 400 == 100 || year < 0 && -year / 400 == 300));
}

/* find week of year */
static int wkyr(int wstart, int wday, int yday)
{
    wday = (wday + 7 - wstart) % 7;
    return (yday + 7 - wday) / 7;
}

/* find week/year, ISO 8601 -- added with C9X */
static int ISOwkyr(int year, int wday, int yday)
{
    int wkno = wkyr(MONDAY, wday, yday);
    int isleap = isleapyear(year);
    int yunleap = yday - isleap;
    int jan1 = (371 - yday + wday) % 7;
    int dec32 = (jan1 + isleap + 365) % 7;

    if (yunleap >= 364 && dec32 == TUESDAY || yunleap >= 363 && dec32 == WEDNESDAY || yunleap >= 362 && dec32 == THURSDAY)
        wkno = -1;  /* push into next year */
    else if (jan1 == TUESDAY || jan1 == WEDNESDAY || jan1 == THURSDAY)
        ++wkno;

    return wkno;
}

/* find week of year, ISO 8601 -- added with C9X */
static int ISOweek(int year, int wday, int yday)
{
    int wkno = ISOwkyr(year, wday, yday);

    if (wkno == 0)
        return (ISOwkyr(year - 1, wday + 7 - yday, isleapyear(year - 1) ? 366 : 365));
    else if (wkno > 0)
        return wkno;
    else
        return 1;
}

/* find year, ISO 8601 -- added with C9X */
static int ISOyear(int year, int wday, int yday)
{
    int wkno = ISOwkyr(year, wday, yday);

    if (wkno == 0)
        return year - 1;
    else if (wkno > 0)
        return year;
    else
        return year + 1;
}

/* compare date in *tin against yyyy/mm/dd at s */
static int cmp_era_date(const struct tm *t, const char *s)
{
    char *eptr;
    long val;

    val = strtol(s, &eptr, 10);
    if (s == eptr || *eptr != '/')
        return 2;  /* fail */
    else if (val != t->tm_year + 1900)
        return (t->tm_year + 1900 < val) ? -1 : +1;

    val = strtol(s = eptr + 1, &eptr, 10);
    if (s == eptr || *eptr != '/')
        return 2;
    else if (val != t->tm_mon + 1)
        return (t->tm_mon + 1 < val) ? -1 : +1;

    val = strtol(eptr + 1, &eptr, 10);
    if (s == eptr)
        return 2;
    else if (val != t->tm_mday)
        return (t->tm_mday < val) ? -1 : +1;
    else
        return 0;
}

/* get era field, if any */
static const char *getera(const struct tm *t, const __timeinfo *tin)
{
    const char *s;
    int i, len;

    for (i = 0; *(s = __gettime(tin->era, i, &len)) != '\0'; ++i)
    {
        /* see if date is in range */
        const char *s1 = __gettime(s + 1, 1, &len);
        int ans = cmp_era_date(t, s1);

        if (*s == '-' && (ans == -1 || ans == 0) || *s == '+' && (ans == 0 || ans == 1))
        {
            /* start date okay, check end date */
            s1 = __gettime(s + 1, 2, &len);
            if (s1[0] == *s && s1[1] == '*')
                return s;

            ans = cmp_era_date(t, s1);
            if (*s == '-' && (ans == 0 || ans == 1) || *s == '+' && (ans == -1 || ans == 0))
                return s;
        }
    }

    return "+:";
}

/* format a time field */
const char *__gentime(const struct tm *t, const __timeinfo *tin, char qual, char code, int *pn, char *ac)
{
    const char *p;

    switch (code)
    {
        /* switch on conversion specifier */
        case 'a':  /* put short weekday name */
            p = __gettime(tin->abday, t->tm_wday << 1, pn);
            break;

        case 'A':  /* put full weekday name */
            p = __gettime(tin->day, (t->tm_wday << 1) + 1, pn);
            break;

        case 'b':  /* put short month name */
        case 'h':  /* put short month name, same as 'b' */
            p = __gettime(tin->abmon, t->tm_mon << 1, pn);
            break;

        case 'B':  /* put full month name */
            p = __gettime(tin->mon, (t->tm_mon << 1) + 1, pn);
            break;

        case 'c':  /* put date and time */
            p = __gettime(qual == 'E' ? tin->era_d_t_fmt : tin->d_t_fmt, 0, pn);
            *pn = -*pn;
            break;

        case 'C':  /* put century, from 00  -- added with C9X */
            if (qual != 'E' || *(p = __gettime(getera(t, tin) + 1, 3, pn)) == '\0')
                p = getval(ac, 19 + t->tm_year / 100, 2, pn, qual, tin);
            break;

        case 'd':  /* put day of month, from 01 */
            p = getval(ac, t->tm_mday, 2, pn, qual, tin);
            break;

        case 'D':  /* put month/day/year -- added with C9X */
            p = "%m/%d/%y", *pn = -8;
            break;

        case 'e':  /* put day of month, from 1 -- changed from 'D' for C9X */
            p = getval(ac, t->tm_mday, 2, pn, qual, tin);
            if (ac[0] == '0') ac[0] = ' ';
            break;

        case 'F':  /* put year-month-day, ISO 8601 -- added with C9X */
            p = "%Y-%m-%d", *pn = -8;
            break;

        case 'g':  /* put year of century, ISO 8601 -- added with C9X */
        {
            /* correct for negative years */
            int year = ISOyear(t->tm_year, t->tm_wday, t->tm_yday) % 100;
            if (year < 0) year += 100;
            p = getval(ac, year, 2, pn, qual, tin);
            break;
        }

        case 'G':  /* put year, ISO 8601 -- added with C9X */
            p = getval(ac, ISOyear(t->tm_year, t->tm_wday, t->tm_yday) + 1900, 4, pn, qual, tin);
            break;

        case 'H':  /* put hour of 24-hour day */
            p = getval(ac, t->tm_hour, 2, pn, qual, tin);
            break;

        case 'I':  /* put hour of 12-hour day */
            p = getval(ac, (t->tm_hour + 11) % 12 + 1, 2, pn, qual, tin);
            break;

        case 'j':  /* put day of year, from 001 */
            p = getval(ac, t->tm_yday + 1, 3, pn, qual, tin);
            break;

        case 'm':  /* put month of year, from 01 */
            p = getval(ac, t->tm_mon + 1, 2, pn, qual, tin);
            break;

        case 'M':  /* put minutes after the hour */
            p = getval(ac, t->tm_min, 2, pn, qual, tin);
            break;

        case 'n':  /* put newline -- added with C9X */
            p = "\n", *pn = 1;
            break;

        case 'p':  /* put AM/PM */
            p = __gettime(tin->am_pm, t->tm_hour >= 12, pn);
            break;

        case 'r':  /* put 12-hour time -- added with C9X */
            p = __gettime(qual == 'E' ? tin->era_t_fmt_ampm : tin->t_fmt_ampm, 3, pn);
            *pn = -*pn;
            break;

        case 'R':  /* put hour:minute -- added with C9X */
            p = "%H:%M", *pn = -5;
            break;

        case 'S':  /* put seconds after the minute */
            p = getval(ac, t->tm_sec, 2, pn, qual, tin);
            break;

        case 't':  /* put horizontal tab -- added with C9X */
            p = "\t", *pn = 1;
            break;

        case 'T':  /* put hour:minute:second, ISO 8601 -- added with C9X */
            p = "%H:%M:%S", *pn = -8;
            break;

        case 'u':  /* put day of week, ISO 8601 -- added with C9X */
            p = getval(ac, t->tm_wday == 0 ? 7 : t->tm_wday, 1, pn, qual, tin);
            break;

        case 'U':  /* put Sunday week of the year */
            p = getval(ac, wkyr(SUNDAY, t->tm_wday, t->tm_yday), 2, pn, qual, tin);
            break;

        case 'V':  /* put week number, ISO 8601 -- added with C9X */
            p = getval(ac, ISOweek(t->tm_year, t->tm_wday, t->tm_yday), 2, pn, qual, tin);
            break;

        case 'w':  /* put day of week, from Sunday */
            p = getval(ac, t->tm_wday, 1, pn, qual, tin);
            break;

        case 'W':  /* put Monday week of the year */
            p = getval(ac, wkyr(MONDAY, t->tm_wday, t->tm_yday), 2, pn, qual, tin);
            break;

        case 'x':  /* put date */
            p = __gettime(qual == 'E' ? tin->era_d_fmt : tin->d_fmt, 1, pn);
            *pn = -*pn;
            break;

        case 'X':  /* put time */
            p = __gettime(qual == 'E' ? tin->era_t_fmt : tin->t_fmt, 2, pn);
            *pn = -*pn;
            break;

        case 'y':  /* put year of the century */
        {
            /* change to year in era if valid era present */
            int year = t->tm_year % 100;
            int digits = 2;

            if (year < 0) year += 100;

            if (qual == 'E' && (p = getera(t, tin))[2] != '\0')
            {
                /* compute year in era */
                char *eptr;
                long val = 1900 + t->tm_year - strtol(__gettime(p + 1, 1, pn), 0, 10);

                if (p[0] == '-') val = -val;
                val += strtol(p + 2, &eptr, 10);
                if (p + 2 != eptr && eptr[0] == p[1])
                {
                    /* accept era year */
                    year = (int)val;
                    for (digits = 1; 0 < (val /= 10); )
                        ++digits;
                }
            }
            p = getval(ac, year, digits, pn, qual, tin);
            break;
        }

        case 'Y':  /* put year */
            if (qual != 'E' || (*(p = __gettime(getera(t, tin) + 1, 4, pn)) == ';' || *pn == 0))
                p = getval(ac, t->tm_year + 1900, 4, pn, qual, tin);
            else
            {
                /* stop format at semicolon */
                const char *p1 = strchr(p, ';');

                if (p1 != 0)
                    *pn = p1 - p;
                *pn = -*pn;
            }
            break;

        case 'z':  /* put time zone offset, ISO 8601 -- added with C9X */
            p = __gettime(tin->tzone[0] == '\0' ? __getzone() : tin->tzone, 2, pn);
            if (*pn > 0 && t->tm_isdst > 0)
            {
                /* adjust time zone offset for DST */
                int val = strtol(p, 0, 10) + 100;

                if (val >= 0)
                    ac[0] = '+';
                else
                    ac[0] = '-', val = -val;

                p = getval(ac + 1, val, 4, pn, qual, tin) - 1;
                ++*pn;
            }
            break;

        case 'Z':  /* put time zone name */
            p = __gettime(tin->tzone[0] == '\0' ? __getzone() : tin->tzone, t->tm_isdst > 0, pn);
            break;

        case '%':  /* put "%" */
            p = "%", *pn = 1;
            break;

        default:  /* unknown field, print it */
            ac[0] = code, ac[1] = '\0';
            p = ac, *pn = 2;
    }

    return p;
}

