/****************************************************************************
 *                                                                          *
 * File    : asctime.c                                                      *
 *                                                                          *
 * Purpose : asctime function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-02  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include "xtime.h"
#include "xthread.h"
#include "xalloc.h"

#define TBUF_SIZE  sizeof("Day Mon dd hh:mm:ss yyyy\n")

/* static data */
static const char alt_digits[] = "";
static const char am_pm[] = ":AM:PM";
static const char days[] = ":Sun:Sunday:Mon:Monday:Tue:Tuesday:Wed:Wednesday:Thu:Thursday:Fri:Friday:Sat:Saturday";
static const char era[] = "";
static const char fmts[] = "|%a %b %e %T %Y|%m/%d/%y|%H:%M:%S|%I:%M:%S %p";
static const char isdst[] = "";
static const char mons[] = ":Jan:January:Feb:February:Mar:March:Apr:April:May:May:Jun:June:Jul:July:Aug:August:Sep:September:Oct:October:Nov:November:Dec:December";
static const char tzone[] = "";  /* adapt by default */

/* time info for C locale */
static __timeinfo ctinfo = {
    am_pm, days, days, days, mons, mons, mons,
    fmts, fmts, fmts, fmts, fmts,
    fmts, fmts, fmts, fmts, fmts,
    era, alt_digits, isdst, tzone
};

/* time info for current locale */
extern __timeinfo __times = {
    am_pm, days, days, days, mons, mons, mons,
    fmts, fmts, fmts, fmts, fmts,
    fmts, fmts, fmts, fmts, fmts,
    era, alt_digits, isdst, tzone
};

#ifdef __MT__
/* format time as "Day Mon dd hh:mm:ss yyyy\n" */
char * __cdecl (asctime)(const struct tm *t)
{
    tiddata *mtd = __get_mtd();
    if (!mtd->asctimebuf) mtd->asctimebuf = (char *)malloc(TBUF_SIZE);
    __strftime(mtd->asctimebuf, TBUF_SIZE, "%c\n", t, &ctinfo);
    return mtd->asctimebuf;
}
#else /* __MT__ */
/* format time as "Day Mon dd hh:mm:ss yyyy\n" */
char * __cdecl (asctime)(const struct tm *t)
{
    static char tbuf[TBUF_SIZE];
    __strftime(tbuf, TBUF_SIZE, "%c\n", t, &ctinfo);
    return tbuf;
}
#endif /* __MT__ */
