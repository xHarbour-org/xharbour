#ifndef _WCTYPE_H
#define _WCTYPE_H

/* wctype.h - C99 standard header */

/* macros */
#define WEOF  ((wint_t)(-1))

/* type definitions */
#ifndef _WINT_T_DEFINED
#define _WINT_T_DEFINED
typedef unsigned short wint_t;
#endif

typedef unsigned int wctrans_t;
typedef unsigned int wctype_t;

/* declarations */
int __cdecl iswalnum(wint_t);
int __cdecl iswalpha(wint_t);
int __cdecl iswcntrl(wint_t);
int __cdecl iswdigit(wint_t);
int __cdecl iswgraph(wint_t);
int __cdecl iswlower(wint_t);
int __cdecl iswprint(wint_t);
int __cdecl iswpunct(wint_t);
int __cdecl iswspace(wint_t);
int __cdecl iswblank(wint_t);
int __cdecl iswupper(wint_t);
int __cdecl iswxdigit(wint_t);
int __cdecl iswctype(wint_t, wctype_t);
wint_t __cdecl towlower(wint_t);
wint_t __cdecl towupper(wint_t);
wint_t __cdecl towctrans(wint_t, wctrans_t);
wctrans_t __cdecl wctrans(const char *);
wctype_t __cdecl wctype(const char *);

/* internal stuff */
int __cdecl __iswctype(wint_t, wctype_t);
wint_t __cdecl __towctrans(wint_t, wctrans_t);

/* macro overrides */
#define iswalnum(wc)  __iswctype(wc,1)
#define iswalpha(wc)  __iswctype(wc,2)
#define iswcntrl(wc)  __iswctype(wc,3)
#define iswdigit(wc)  __iswctype(wc,4)
#define iswgraph(wc)  __iswctype(wc,5)
#define iswlower(wc)  __iswctype(wc,6)
#define iswprint(wc)  __iswctype(wc,7)
#define iswpunct(wc)  __iswctype(wc,8)
#define iswspace(wc)  __iswctype(wc,9)
#define iswblank(wc)  __iswctype(wc,12)
#define iswupper(wc)  __iswctype(wc,10)
#define iswxdigit(wc)  __iswctype(wc,11)
#define iswctype(wc,desc)  __iswctype(wc,desc)
#define towlower(wc)  __towctrans(wc,1)
#define towupper(wc)  __towctrans(wc,2)
#define towctrans(wc,desc)  __towctrans(wc,desc)

#endif /* _WCTYPE_H */

