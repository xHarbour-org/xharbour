/****************************************************************************
 *                                                                          *
 * File    : _getdst.c                                                      *
 *                                                                          *
 * Purpose : __getdst function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include "xtime.h"

/* accumulate digits */
static int getint(const char *s, int n)
{
    int value;

    for (value = 0; --n >= 0 && isdigit(*s); ++s)
        value = value * 10 + *s - '0';

    return (n >= 0) ? -1 : value;
}

/* parse DST rules */
__dst *__getdst(const char *s)
{
    const char delim = *s++;
    __dst *pr, *rules;

    if (delim == '\0')
        return 0;

    /* buy space for rules */
    {
        const char *s1, *s2;
        int i;

        for (s1 = s, i = 2; (s2 = strchr(s1, delim)) != 0; ++i)
            s1 = s2 + 1;

        if ((rules = (__dst *)malloc(sizeof(__dst) * i)) == 0)
            return 0;
    }

    /* parse rules */
    {
        int year = 0;

        for (pr = rules;; ++pr, ++s)
        {
            /* parse next rule */
            if (*s == '(')
            {
                /* got a year qualifier */
                year = getint(s + 1, 4) - 1900;
                if (year < 0 || s[5] != ')')
                    break;  /* invalid year */
                s += 6;
            }
            pr->year = year;
            pr->mon = getint(s, 2) - 1, s += 2;
            pr->day = getint(s, 2) - 1, s += 2;

            if (isdigit(*s))
                pr->hour = getint(s, 2), s += 2;
            else
                pr->hour = 0;

            if (pr->mon >= 12 || pr->day > 99 || pr->hour > 99)
                break;  /* invalid month, day, or hour */

            if (*s != '+' && *s != '-')
                pr->wday = 0;
            else if (s[1] < '0' || '6' < s[1])
                break;  /* invalid week day */
            else
            {
                /* compute week day field */
                pr->wday = (s[1] == '0') ? 7 : s[1] - '0';
                if (*s == '+')  /* '-': strictly before */
                    pr->wday += 7;  /* '+': on or after */
                s += 2;
            }

            if (*s == '\0')
            {
                /* done, terminate list */
                (pr + 1)->wday = (unsigned char)-1;
                (pr + 1)->year = year;
                return rules;
            }
            else if (*s != delim)
                break;
        }

        free(rules);
        return 0;
    }
}

