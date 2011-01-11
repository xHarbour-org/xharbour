#ifndef _STDLIB_H
#define _STDLIB_H

/* stdlib.h - C99 standard header */

#ifndef _CRTIMP
#ifdef _DLL
#define _CRTIMP  __declspec(dllimport)
#else
#define _CRTIMP
#endif
#endif /* _CRTIMP */

/* macros */
#ifndef NULL
#define NULL  ((void *)0)
#endif

#define EXIT_FAILURE  1
#define EXIT_SUCCESS  0

#ifndef _WINCE
#define MB_CUR_MAX  __mbcurmax
#define RAND_MAX  0x3fffffff
#else /* _WINCE */
#define MB_CUR_MAX  __mb_cur_max
#define RAND_MAX  0x7fff
#endif /* _WINCE */

/* type definitions */
#ifndef _SIZE_T_DEFINED
#define _SIZE_T_DEFINED
typedef unsigned int size_t;
#endif
#ifndef _WCHAR_T_DEFINED
#define _WCHAR_T_DEFINED
typedef unsigned short wchar_t;
#endif

typedef struct {
    int quot;
    int rem;
} div_t;

typedef struct {
    long quot;
    long rem;
} ldiv_t;

#ifndef _WINCE
typedef struct {
    long long quot;
    long long rem;
} lldiv_t;

typedef int __cdecl __cmpfunc(const void *, const void *);
#endif /* _WINCE */

/* declarations */
int __cdecl abs(int);
int __cdecl atexit(void (__cdecl *)(void));
double __cdecl atof(const char *);
int __cdecl atoi(const char *);
long __cdecl atol(const char *);
void * __cdecl calloc(size_t, size_t);
div_t __cdecl div(int, int);
void __cdecl free(void *);
long __cdecl labs(long);
ldiv_t __cdecl ldiv(long, long);
void * __cdecl malloc(size_t);
size_t __cdecl mbstowcs(wchar_t * restrict, const char * restrict, size_t);
int __cdecl rand(void);
void __cdecl srand(unsigned int);
void * __cdecl realloc(void *, size_t);
double __cdecl strtod(const char * restrict, char ** restrict);
long __cdecl strtol(const char * restrict, char ** restrict, int);
unsigned long __cdecl strtoul(const char * restrict, char ** restrict, int);
size_t __cdecl wcstombs(char * restrict, const wchar_t * restrict, size_t);

#ifndef _WINCE
void __declspec(noreturn) __cdecl abort(void);
long long __cdecl atoll(const char *);
void * __cdecl bsearch(const void *, const void *, size_t, size_t, __cmpfunc *);
void __declspec(noreturn) __cdecl _Exit(int);
void __declspec(noreturn) __cdecl exit(int);
char * __cdecl getenv(const char *);
long long __cdecl llabs(long long);
lldiv_t __cdecl lldiv(long long, long long);
int __cdecl mblen(const char *, size_t);
int __cdecl mbtowc(wchar_t * restrict, const char * restrict, size_t);
void __cdecl qsort(void *, size_t, size_t, __cmpfunc *);
float __cdecl strtof(const char * restrict, char ** restrict);
long double __cdecl strtold(const char * restrict, char ** restrict);
long long __cdecl strtoll(const char * restrict, char ** restrict, int);
unsigned long long __cdecl strtoull(const char * restrict, char ** restrict, int);
int __cdecl system(const char *);
int __cdecl wctomb(char *, wchar_t);
#else /* _WINCE */
void __cdecl exit(int);
void __cdecl qsort(void *, size_t, size_t, int (__cdecl *)(const void *, const void *));
#endif /* _WINCE */

/* private extensions to standard C */
void * __cdecl _alloca(size_t);
size_t __cdecl _msize(void *);
char * __cdecl _itoa(int, char *, int);
char * __cdecl _ltoa(long, char *, int);
unsigned long __cdecl _lrotl(unsigned long, int);
unsigned long __cdecl _lrotr(unsigned long, int);
unsigned int __cdecl _rotl(unsigned int, int);
unsigned int __cdecl _rotr(unsigned int, int);
char * __cdecl _ultoa(unsigned long, char *, int);

#ifndef _WINCE
unsigned int __cdecl _bswap(unsigned int);
char * __cdecl _fullpath(char *, const char *, size_t);
int __cdecl _putenv(const char *);
void __cdecl _searchenv(const char *, const char *, char *);
void __cdecl _splitpath(const char *, char *, char *, char *, char *);
size_t __cdecl _set_crt_heap_size(size_t);
#endif /* _WINCE */

/* compatibility names */
#ifdef __POCC__OLDNAMES
char * __cdecl itoa(int, char *, int);
char * __cdecl ltoa(long, char *, int);
int __cdecl putenv(const char *);
char * __cdecl ultoa(unsigned long, char *, int);
#undef alloca
#define alloca  _alloca
#endif /* __POCC__OLDNAMES */

/* internal stuff */
#ifndef _WINCE
double __cdecl __stod(const char *, char **, long);
float __cdecl __stof(const char *, char **, long);
long double __cdecl __stold(const char *, char **, long);
long long __cdecl __stoll(const char *, char **, int);
unsigned long __cdecl __stoul(const char *, char **, int);
unsigned long long __cdecl __stoull(const char *, char **, int);
#endif /* _WINCE */

/* data declarations */
#ifndef _WINCE
extern _CRTIMP char __mbcurmax;
#else /* _WINCE */
extern int __mb_cur_max;
#endif /* _WINCE */

/* private extensions */
extern _CRTIMP int __argc;
extern _CRTIMP char **__argv;
extern _CRTIMP size_t __bheap_threshold;

/* macro overrides */
#ifndef _WINCE
#define atof(s)  __stod(s,0,0)
#define atoi(s)  (int)__stoul(s,0,10)
#define atol(s)  (long)__stoul(s,0,10)
#define atoll(s)  (long long)__stoull(s,0,10)
#define strtof(s,endptr)  __stof(s,endptr,0)
#define strtod(s,endptr)  __stod(s,endptr,0)
#define strtold(s,endptr)  __stold(s,endptr,0)
#define strtoll(s,endptr,base)  __stoll(s,endptr,base)
#define strtoul(s,endptr,base)  __stoul(s,endptr,base)
#define strtoull(s,endptr,base)  __stoull(s,endptr,base)
#endif /* _WINCE */

#endif /* _STDLIB_H */
