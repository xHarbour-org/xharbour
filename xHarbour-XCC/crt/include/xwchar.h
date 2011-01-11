#ifndef _XWCHAR_H
#define _XWCHAR_H

#include <wchar.h>
#include <wctype.h>

/* xwchar.h - internal header */

/* declarations */
int __mbtowc(wchar_t *, const char *, size_t, mbstate_t *);
size_t __wcsftime(wchar_t *, size_t, const char *, size_t, const struct tm *);
int __wctomb(char *, wchar_t, mbstate_t *);
long double __cdecl __wcstold(const wchar_t *, wchar_t **, long);
long long __wcstoll(const wchar_t *, wchar_t **, int);
unsigned long __cdecl __wcstoul(const wchar_t *, wchar_t **, int);
unsigned long long __wcstoull(const wchar_t *, wchar_t **, int);

#endif /* _XWCHAR_H */

