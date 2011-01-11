/****************************************************************************
 *                                                                          *
 * File    : setlocale.c                                                    *
 *                                                                          *
 * Purpose : setlocale function.                                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-05  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include "xlocale.h"
#include "xstrxfrm.h"
#include "xwchar.h"

#if LC_COLLATE != 1 || LC_CTYPE != 2 || LC_MONETARY != 4 || LC_NUMERIC != 8 || LC_TIME != 16 || LC_ALL != 31
#error LOCALE CATEGORIES INCONSISTENT
#endif

#define _CATMASK(n)  (1 << (n))
#define _NCAT  6

/* type definitions */
struct _Locprofile {
    __locinfo *array[_NCAT];
};

/* static data */
typedef char *name_t;

__locinfo __c_locale = {"C"};

static name_t curname = "C";
static name_t namalloc = 0;
static const char *const nmcats[_NCAT] = {
    0, "collate:", "ctype:", "monetary:", "numeric:", "time:"
};

static struct _Locprofile profile = {{
    &__c_locale, &__c_locale, &__c_locale, &__c_locale,
    &__c_locale, &__c_locale
}};

/* get pointer to "C" locale */
static __locinfo *init_c_locale(void)
{
    if (__c_locale.collatetab.tab[0] == 0)
    {
        /* initialize "C" locale */
        __c_locale.collatetab = __collatetab;
        __c_locale.wcollatetab = __wcollatetab;
        __c_locale.ctypetab = __ctypetab;
        __c_locale.tolowertab = __tolowertab;
        __c_locale.touppertab = __touppertab;
        __c_locale.mbtowctab = __mbtowctab;
        __c_locale.wctombtab = __wctombtab;
        __c_locale.wctranstab = __wctranstab;
        __c_locale.wctypetab = __wctypetab;
        __c_locale.mbcurmax = MB_CUR_MAX;
        __c_locale.loc = __locale;
        __c_locale.times = __times;
    }
    return &__c_locale;
}

/* map locale name to profile */
int _Getnloc(__locinfo **pc, int cat, const char *locname)
{
    __locinfo *p;
    int changed = 0;
    size_t i;

    if ((cat & LC_ALL) != cat)
        return -1;  /* bad category mask */

    if (locname[0] == '\0')
        locname = "C"; /* _Defloc(); */

    for (i = 0; i < _NCAT; i++)
    {
        if ((_CATMASK(i) & cat) != 0)
        {
            /* set a category */
            if ((p = __getlocale(nmcats[i], locname)) == 0)
                return -1;

            if (p != pc[i])
                changed = 1;

            pc[i] = p;
        }
    }

    if (cat == LC_ALL && (p = __getlocale("", locname)) != 0 && p != pc[0])
        pc[0] = p, changed = 1;

    return changed;
}

/* build a locale name */
char *_Getlname(__locinfo **pc, int cat, const char **pn)
{
    char *s;
    int based;
    size_t i, len, n;

    for (based = 0, len = 0, n = 0, i = 0; i < _NCAT; i++)
    {
        if ((_CATMASK(i) & cat) == 0)
            ;
        else if (pc[i] == pc[0])
            based = 1;
        else
        {
            /* count a changed subcategory */
            len += strlen(nmcats[i]) + strlen(pc[i]->name) + 1;
            ++n;
        }
    }

    if (n == 0)
    {
        /* uniform or vacuous locale */
        free((void *)*pn), *pn = 0;
        return (based) ? (char *)pc[0]->name : 0;
    }

    if (based)
        len += strlen(pc[0]->name);
    else
        --len;

    if ((s = (char *)malloc(len + 1)) == 0)
    {
        return 0;
    }
    else
    {
        /* build complex name */
        free((void *)*pn), *pn = s;

        if (based)
            s += strlen(strcpy(s, pc[0]->name));

        for (i = 0; ++i < _NCAT; )
        {
            if ((_CATMASK(i) & cat) != 0 && pc[i] != pc[0])
            {
                /* add a component */
                if (based)
                    *s++ = ';';
                based = 1;
                s += strlen(strcpy(s, nmcats[i]));
                s += strlen(strcpy(s, pc[i]->name));
            }
        }
        return (char *)*pn;
    }
}

/* set new locale */
char * __cdecl (setlocale)(int cat, const char *locname)
{
    name_t *pcurname = &curname;

    if (locname != 0)
    {
        /* set categories */
        struct _Locprofile prof = profile;
        int changed;

        init_c_locale();

        changed = _Getnloc(prof.array, cat, locname);
        if (changed < 0)
        {
            pcurname = 0;
        }
        else if (changed > 0)
        {
            /* change to new locale */
            char *name = _Getlname(prof.array, LC_ALL, (const char **)&namalloc);
            size_t i;

            if (name == 0)
            {
                pcurname = 0;
            }
            else
            {
                /* valid locale, set each category */
                *pcurname = name;
                for (i = 0; i < _NCAT; i++)
                {
                    if (profile.array[i] != prof.array[i])
                        profile.array[i] = __setlocale(_CATMASK(i), prof.array[i]);
                }
            }
        }
    }

    return (pcurname != 0) ? *pcurname : 0;
}

