/****************************************************************************
 *                                                                          *
 * File    : _getlocale.c                                                   *
 *                                                                          *
 * Purpose : __getlocale function.                                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-06  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <string.h>
#include "xlocale.h"
#include "xthread.h"
#include "xcrt.h"

#ifdef _LOCALE_FILE_SUPPORT  /* file locale not supported - PO 2002-10-29 */
/* free all constructed locales */
static void freeall(void)
{
    __locinfo *p, *q;

    for (p = __c_locale.next; p != 0; p = q)
    {
        /* free a locale */
        q = p->next;
        __freelocale(p);
        free(p);
    }

    __freelocale(&__c_locale);
    __c_locale.next = 0;
}
#endif

/* get locale pointer, given category and name */
__locinfo *__getlocale(const char *nmcat, const char *lname)
{
    const char *ns, *s;
    size_t nl;
    __locinfo *p;

    /* find category component of name */
    {
        size_t n;

        for (ns = 0, s = lname;; s += n + 1)
        {
            /* look for exact match or LC_ALL */
            if (s[n = strcspn(s, ":;")] == '\0' || s[n] == ';')
            {
                /* memorize first LC_ALL */
                if (ns == 0 && n > 0)
                    ns = s, nl = n;

                if (s[n] == '\0')
                    break;
            }
            else if (memcmp(nmcat, s, ++n) == 0)
            {
                /* found exact category match */
                ns = s + n, nl = strcspn(ns, ";");
                break;
            }
            else if (s[n += strcspn(s + n, ";")] == '\0')
                break;
        }

        if (ns == 0)
            return 0;  /* invalid name */
    }

#ifdef __MT__
    __mtlock(_LOCALE_LOCK);
    __try
    {
#endif
        for (p = &__c_locale; p != 0; p = p->next)
        {
            if (memcmp(p->name, ns, nl) == 0 && p->name[nl] == '\0')
                break;
        }

#ifdef _LOCALE_FILE_SUPPORT  /* file locale not supported - PO 2002-10-29 */
        if (p == 0)
        {
            static int init = 0;

            /* look for locale in file */
            p = _Findloc(ns, nl);

            if (!init)
            {
                init = 1;
                _Atexit(&freeall);
            }
        }
#endif
#ifdef __MT__
    }
    __finally
    {
        __mtunlock(_LOCALE_LOCK);
    }
#endif

    return p;
}

