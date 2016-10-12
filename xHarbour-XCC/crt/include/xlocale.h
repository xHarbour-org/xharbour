#ifndef _XLOCALE_H
#define _XLOCALE_H

#include <ctype.h>
#include <limits.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include <wchar.h>
#include <xstate.h>
#include <xtinfo.h>
#include <xwctype.h>

/* xlocale.h - internal header */

/* macros for __getlocale and friends */
#define ADDR(p, q, ty)      (*(ty *)((char *)p + q->_Offset))
#define NEWADDR(p, q, ty)   (ADDR(p, q, ty) != ADDR(&__c_locale, q, ty))
#define MAXLIN  256
#define TABSIZ  ((UCHAR_MAX + 2) * sizeof(short))

/* type definitions */
#if 0
enum _Lcode {  /* codes for locale parsing tables */
    L_GSTRING, L_NAME, L_NOTE, L_SET, L_STATE,
    L_STRING, L_TABLE, L_VALUE, L_WCTYPE
};
#endif

#if 0
typedef struct _Locitem {  /* parsing table entry */
    const char *name;
    size_t _Offset;
    enum _Lcode _Code;
} _Locitem;
#endif

typedef struct __locinfo {  /* locale description */
    const char *name;  /* must be first */
    struct __locinfo *next;
    /* controlled by LC_COLLATE */
    __fsmtab collatetab, wcollatetab;
    /* controlled by LC_CTYPE */
    const short *ctypetab, *tolowertab, *touppertab;
    char mbcurmax;
    __fsmtab mbtowctab, wctombtab;
    const __wcinfo *wctranstab, *wctypetab;
    /* controlled by LC_MONETARY and LC_NUMERIC */
    struct lconv loc;
    /* controlled by LC_TIME */
    __timeinfo times;
} __locinfo;

/* declarations (removed file locale stuff 2002-10-29) */
/* const char *_Defloc(void); */
/* __locinfo *_Findloc(const char *, size_t); */
/* void __freelocale(__locinfo *); */
__locinfo *__getlocale(const char *, const char *);
/* const char *_Locsum(const char *, unsigned long *); */
/* int _Locterm(const char **, unsigned long *); */
/* int _Locvar(char, unsigned long); */
/* int _Makeloc(FILE *, char *, __locinfo *); */
/* int _Makestab(__locinfo *, const _Locitem *, const char *); */
/* int _Makewct(__locinfo *, const _Locitem *, const char *); */
/* const _Locitem *_Readloc(FILE *, char *, const char **); */
__locinfo *__setlocale(int, __locinfo *);
/* const char *_Skip(const char *); */

/* data declarations */
extern __locinfo __c_locale;
/* extern const _Locitem __localetab[]; */

#endif /* _XLOCALE_H */

