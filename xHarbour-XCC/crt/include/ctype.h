#ifndef _CTYPE_H
#define _CTYPE_H

/* ctype.h - C99 standard header */

#ifdef _MSC_EXTENSIONS
/* to compile with windows.h -- see Microsoft ctype.h */
#ifndef _WCHAR_T_DEFINED
#define _WCHAR_T_DEFINED
typedef unsigned short wchar_t;
#endif
#endif /* _MSC_EXTENSIONS */

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

/* data declarations */
extern const short *__ctypetab;
extern const short *__tolowertab;
extern const short *__touppertab;

/* declarations */
int __cdecl isalnum(int);
int __cdecl isalpha(int);
int __cdecl iscntrl(int);
int __cdecl isdigit(int);
int __cdecl isgraph(int);
int __cdecl islower(int);
int __cdecl isprint(int);
int __cdecl ispunct(int);
int __cdecl isspace(int);
int __cdecl isblank(int);
int __cdecl isupper(int);
int __cdecl isxdigit(int);
int __cdecl tolower(int);
int __cdecl toupper(int);

/* macro overrides */
#define isalnum(c)  (__ctypetab[(int)(c)] & (_DIGIT|_LOWER|_UPPER))
#define isalpha(c)  (__ctypetab[(int)(c)] & (_LOWER|_UPPER))
#define iscntrl(c)  (__ctypetab[(int)(c)] & _CNTRL)
#define isdigit(c)  (__ctypetab[(int)(c)] & _DIGIT)
#define isgraph(c)  (__ctypetab[(int)(c)] & (_DIGIT|_LOWER|_UPPER|_PUNCT))
#define islower(c)  (__ctypetab[(int)(c)] & _LOWER)
#define isprint(c)  (__ctypetab[(int)(c)] & (_DIGIT|_LOWER|_UPPER|_PUNCT|_SPACE))
#define ispunct(c)  (__ctypetab[(int)(c)] & _PUNCT)
#define isspace(c)  (__ctypetab[(int)(c)] & (_SPACE|_WHITE))
#define isblank(c)  (__ctypetab[(int)(c)] & (_SPACE|_BLANK))
#define isupper(c)  (__ctypetab[(int)(c)] & _UPPER)
#define isxdigit(c)  (__ctypetab[(int)(c)] & _HEX)
#define tolower(c)  __tolowertab[(int)(c)]
#define toupper(c)  __touppertab[(int)(c)]

#endif /* _CTYPE_H */

