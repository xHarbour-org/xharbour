#ifndef _WCTYPE_H
#define _WCTYPE_H

/* wctype.h - C99 standard header */

/* macros */
#define WEOF  ((wint_t)(-1))

#ifndef _WINCE

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
int __cdecl _iswascii(wint_t);
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
#define _iswascii(wc)  __iswctype(wc,13)
#define iswctype(wc,desc)  __iswctype(wc,desc)
#define towlower(wc)  __towctrans(wc,1)
#define towupper(wc)  __towctrans(wc,2)
#define towctrans(wc,desc)  __towctrans(wc,desc)

#ifdef __POCC__OLDNAMES
int __cdecl iswascii(wint_t);
#define iswascii(c)  __iswctype(wc,13)
#endif /* __POCC__OLDNAMES */

#else /* _WINCE */

/* type definitions */
#ifndef _WCTYPE_T_DEFINED
typedef unsigned short wint_t;
typedef unsigned short wctype_t;
#define _WCTYPE_T_DEFINED
#endif

/* classification bit masks */
#define _DIGIT  0x01    /* decimal digit */
#define _HEX    0x02    /* hexadecimal digit */
#define _LOWER  0x04    /* lower case letter */
#define _UPPER  0x08    /* upper case letter */
#define _CNTRL  0x10    /* control character */
#define _WHITE  0x20    /* white space character */
#define _SPACE  0x40    /* space */
#define _PUNCT  0x80    /* punctuation character */
#define _BLANK  0x100   /* blank (tab) */
#define _ASCII  0x200   /* ascii */

/* declarations */
int iswctype(wint_t, wctype_t);
wint_t __cdecl towlower(wint_t);
wint_t __cdecl towupper(wint_t);

/* macro overrides */
#define iswalnum(c)  (iswctype((c), _ALPHA|_DIGIT))
#define iswalpha(c)  (iswctype((c), _ALPHA))
#define iswcntrl(c)  (iswctype((c), _CNTRL))
#define iswdigit(c)  (iswctype((c), _DIGIT))
#define iswgraph(c)  (iswctype((c), _PUNCT|_ALPHA|_DIGIT))
#define iswlower(c)  (iswctype((c), _LOWER))
#define iswprint(c)  (iswctype((c), _BLANK|_PUNCT|_ALPHA|_DIGIT))
#define iswpunct(c)  (iswctype((c), _PUNCT))
#define iswspace(c)  (iswctype((c), _SPACE))
/* iswblank() */
#define iswupper(c)  (iswctype((c), _UPPER))
#define iswxdigit(c)  (iswctype((c), _HEX))
#define _iswascii(c)  ((unsigned)(c) < 0x80)
/* towctrans() */

#ifdef __POCC__OLDNAMES
#define iswascii  _iswascii
#endif /* __POCC__OLDNAMES */

#endif /* _WINCE */

#endif /* _WCTYPE_H */
