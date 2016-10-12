#ifndef _XWSTDIO_H
#define _XWSTDIO_H

#include <xstdio.h>
#include <xwchar.h>

/* xwstdio.h - internal header */

/* macros for __wscanf and friends */
#define WGET(px)        (++(px)->nchar, (*(px)->pfn)((px)->arg, 0, 1))
#define WGETN(px)       (0 <= --(px)->nget ? WGET(px) : WEOF)
#define WUNGET(px, ch)  (--(px)->nchar, (*(px)->pfn)((px)->arg, ch, 0))
#define WUNGETN(px, ch) do if ((ch) != WEOF) WUNGET(px, ch); else --(px)->nchar; while (0)

/* type definitions */
typedef struct {  /* print formatting information */
    union {
        long long li;
        long double ld;
    } v;
    void *(*pfn)(void *, const wchar_t *, size_t);
    void *arg;
    wchar_t *s;
    int n0, nz0, n1, nz1, n2, nz2, nchar, prec, width;
    unsigned short flags;
    wchar_t qual;
} __wprtinfo;

typedef struct {  /* scan formatting information */
    wint_t (*pfn)(void *, wint_t, int);
    void *arg;
    va_list ap;
    const wchar_t *s;
    int nchar, nget, width;
    wchar_t qual;
    char noconv, stored;
} __wscninfo;

/* declarations */
int __wfread(FILE *);
int __wfwrite(FILE *);
void __wgenld(__wprtinfo *, wchar_t, wchar_t *, short, short);
int __wgetfield(__wscninfo *);
int __wgetflt(__wscninfo *);
int __wgetint(__wscninfo *);
int __wgetstr(__wscninfo *, int);
void __wldtob(__wprtinfo *, wchar_t);
void __wlitob(__wprtinfo *, wchar_t);
int __wprintf(void *(*)(void *, const wchar_t *, size_t), void *, const wchar_t *, va_list);
int __wputstr(__wprtinfo *, const char *);
int __wputfield(__wprtinfo *, va_list *, wchar_t, wchar_t *);
int __wputtxt(__wprtinfo *, const wchar_t *);
int __wscanf(wint_t (*)(void *, wint_t, int), void *, const wchar_t *, va_list);

#endif /* _XWSTDIO_H */

