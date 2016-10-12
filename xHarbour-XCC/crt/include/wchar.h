#ifndef _WCHAR_H
#define _WCHAR_H

/* wchar.h - C99 standard header */

/* macros */
#ifndef NULL
#define NULL  ((void *)0)
#endif

#define WCHAR_MIN  0
#define WCHAR_MAX  0xffff

#define WEOF  ((wint_t)(-1))

/* type definitions */
#ifndef _MBSTATE_T_DEFINED
#define _MBSTATE_T_DEFINED
typedef struct mbstate_t {
    unsigned long wchar;
    unsigned short rsrv, state;
} mbstate_t;
#endif

#ifndef _SIZE_T_DEFINED
#define _SIZE_T_DEFINED
typedef unsigned int size_t;
#endif

#ifndef _VA_LIST_DEFINED
#define _VA_LIST_DEFINED
typedef char *va_list;
#endif

#ifndef _WCHAR_T_DEFINED
#define _WCHAR_T_DEFINED
typedef unsigned short wchar_t;
#endif

#ifndef _WINT_T_DEFINED
#define _WINT_T_DEFINED
typedef unsigned short wint_t;
#endif

struct tm;
struct FILE;

/* formatted wide-character input/output functions */
int __cdecl fwprintf(struct FILE * restrict, const wchar_t * restrict, ...);
int __cdecl fwscanf(struct FILE * restrict, const wchar_t * restrict, ...);
int __cdecl swprintf(wchar_t * restrict, size_t, const wchar_t * restrict, ...);
int __cdecl swscanf(const wchar_t * restrict, const wchar_t * restrict, ...);
int __cdecl vfwprintf(struct FILE * restrict, const wchar_t * restrict, va_list);
int __cdecl vfwscanf(struct FILE * restrict, const wchar_t * restrict, va_list);
int __cdecl vswprintf(wchar_t * restrict, size_t, const wchar_t * restrict, va_list);
int __cdecl vswscanf(const wchar_t * restrict, const wchar_t * restrict, va_list);
int __cdecl vwprintf(const wchar_t * restrict, va_list);
int __cdecl vwscanf(const wchar_t * restrict, va_list);
int __cdecl wprintf(const wchar_t * restrict, ...);
int __cdecl wscanf(const wchar_t * restrict, ...);

/* wide character input/output functions */
wint_t __cdecl fgetwc(struct FILE *);
wchar_t * __cdecl fgetws(wchar_t * restrict, int, struct FILE * restrict);
wint_t __cdecl fputwc(wchar_t, struct FILE *);
int __cdecl fputws(const wchar_t * restrict, struct FILE * restrict);
int __cdecl fwide(struct FILE *, int);
wint_t __cdecl getwc(struct FILE *);
wint_t __cdecl getwchar(void);
wint_t __cdecl putwc(wchar_t, struct FILE *);
wint_t __cdecl putwchar(wchar_t);
wint_t __cdecl ungetwc(wint_t, struct FILE *);

/* general wide-string utilities */
double __cdecl wcstod(const wchar_t * restrict, wchar_t ** restrict);
float __cdecl wcstof(const wchar_t * restrict, wchar_t ** restrict);
long double __cdecl wcstold(const wchar_t * restrict, wchar_t ** restrict);
long __cdecl wcstol(const wchar_t * restrict, wchar_t ** restrict, int);
long long __cdecl wcstoll(const wchar_t * restrict, wchar_t ** restrict, int);
unsigned long __cdecl wcstoul(const wchar_t * restrict, wchar_t ** restrict, int);
unsigned long long __cdecl wcstoull(const wchar_t * restrict, wchar_t ** restrict, int);
wchar_t * __cdecl wcscpy(wchar_t * restrict, const wchar_t * restrict);
wchar_t * __cdecl wcsncpy(wchar_t * restrict, const wchar_t * restrict, size_t);
wchar_t * __cdecl wcscat(wchar_t * restrict, const wchar_t * restrict);
wchar_t * __cdecl wcsncat(wchar_t * restrict, const wchar_t * restrict, size_t);
int __cdecl wcscmp(const wchar_t *, const wchar_t *);
int __cdecl wcscoll(const wchar_t *, const wchar_t *);
int __cdecl wcsncmp(const wchar_t *, const wchar_t *, size_t);
size_t __cdecl wcsxfrm(wchar_t * restrict, const wchar_t * restrict, size_t);
wchar_t * __cdecl wcschr(const wchar_t *, wchar_t);
size_t __cdecl wcscspn(const wchar_t *, const wchar_t *);
size_t __cdecl wcslen(const wchar_t *);
wchar_t * __cdecl wcspbrk(const wchar_t *, const wchar_t *);
wchar_t * __cdecl wcsrchr(const wchar_t *, wchar_t);
size_t __cdecl wcsspn(const wchar_t *, const wchar_t *);
wchar_t * __cdecl wcsstr(const wchar_t *, const wchar_t *);
wchar_t * __cdecl wcstok(wchar_t * restrict, const wchar_t * restrict, wchar_t ** restrict);
wchar_t * __cdecl wmemchr(const wchar_t *, wchar_t, size_t);
int __cdecl wmemcmp(const wchar_t *, const wchar_t *, size_t);
wchar_t * __cdecl wmemcpy(wchar_t * restrict, const wchar_t * restrict, size_t);
wchar_t * __cdecl wmemmove(wchar_t *, const wchar_t *, size_t);
wchar_t * __cdecl wmemset(wchar_t *, wchar_t, size_t);

/* wide-character time conversion functions */
size_t __cdecl wcsftime(wchar_t * restrict, size_t, const wchar_t * restrict, const struct tm * restrict);

/* extended multibyte and wide-character conversion utilities */
wint_t __cdecl btowc(int);
int __cdecl wctob(wint_t);
int __cdecl mbsinit(const mbstate_t *);
size_t __cdecl mbrlen(const char * restrict, size_t, mbstate_t * restrict);
size_t __cdecl mbrtowc(wchar_t * restrict, const char * restrict, size_t, mbstate_t * restrict);
size_t __cdecl wcrtomb(char * restrict, wchar_t, mbstate_t * restrict);
size_t __cdecl mbsrtowcs(wchar_t * restrict, const char ** restrict, size_t, mbstate_t * restrict);
size_t __cdecl wcsrtombs(char * restrict, const wchar_t ** restrict, size_t, mbstate_t * restrict);

/* private extensions to standard C */
int __cdecl _wcsicmp(const wchar_t *, const wchar_t *);
int __cdecl _wcsnicmp(const wchar_t *, const wchar_t *, size_t);
wchar_t * __cdecl _wcsdup(const wchar_t *);
wchar_t * __cdecl _wcsupr(wchar_t *);
wchar_t * __cdecl _wcslwr(wchar_t *);

/* internal stuff */
wint_t __cdecl __btowc(int);
int __cdecl __wctob(wint_t);
double __cdecl __wcstod(const wchar_t *, wchar_t **, long);
float __cdecl __wcstof(const wchar_t *, wchar_t **, long);
long double __cdecl __wcstold(const wchar_t *, wchar_t **, long);
unsigned long __cdecl __wcstoul(const wchar_t *, wchar_t **, int);

/* macro overrides */
#define btowc(c)  __btowc(c)
#define wcstod(nptr,endptr)  __wcstod(nptr,endptr,0)
#define wcstoul(nptr,endptr,base)  __wcstoul(nptr,endptr,base)
#define wcstof(nptr,endptr)  __wcstof(nptr,endptr,0)
#define wcstold(nptr,endptr)  __wcstold(nptr,endptr,0)
#define wctob(wc)  __wctob(wc)

#endif /* _WCHAR_H */

