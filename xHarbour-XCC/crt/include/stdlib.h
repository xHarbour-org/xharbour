#ifndef _STDLIB_H
#define _STDLIB_H

/* stdlib.h - C99 standard header */

/* macros */
#ifndef NULL
#define NULL  ((void *)0)
#endif

#define EXIT_FAILURE  1
#define EXIT_SUCCESS  0

#define MB_CUR_MAX  __mbcurmax
#define RAND_MAX  0x3fffffff

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

typedef struct {
    long long quot;
    long long rem;
} lldiv_t;

typedef int __cdecl __cmpfunc(const void *, const void *);

/* declaration of low-level functions */
void __cdecl _Exit(int);
void __cdecl exit(int);
char * __cdecl getenv(const char *);
int __cdecl system(const char *);

/* declarations */
void __cdecl abort(void);
int __cdecl abs(int);
int __cdecl atexit(void (__cdecl *)(void));
double __cdecl atof(const char *);
int __cdecl atoi(const char *);
long __cdecl atol(const char *);
long long __cdecl atoll(const char *);
void * __cdecl bsearch(const void *, const void *, size_t, size_t, __cmpfunc *);
void * __cdecl calloc(size_t, size_t);
div_t __cdecl div(int, int);
void __cdecl free(void *);
long __cdecl labs(long);
ldiv_t __cdecl ldiv(long, long);
long long __cdecl llabs(long long);
lldiv_t __cdecl lldiv(long long, long long);
void * __cdecl malloc(size_t);
int __cdecl mblen(const char *, size_t);
size_t __cdecl mbstowcs(wchar_t * restrict, const char * restrict, size_t);
int __cdecl mbtowc(wchar_t * restrict, const char * restrict, size_t);
void __cdecl qsort(void *, size_t, size_t, __cmpfunc *);
int __cdecl rand(void);
void __cdecl srand(unsigned int);
void * __cdecl realloc(void *, size_t);
double __cdecl strtod(const char * restrict, char ** restrict);
float __cdecl strtof(const char * restrict, char ** restrict);
long __cdecl strtol(const char * restrict, char ** restrict, int);
long double __cdecl strtold(const char * restrict, char ** restrict);
long long __cdecl strtoll(const char * restrict, char ** restrict, int);
unsigned long __cdecl strtoul(const char * restrict, char ** restrict, int);
unsigned long long __cdecl strtoull(const char * restrict, char ** restrict, int);
size_t __cdecl wcstombs(char * restrict, const wchar_t * restrict, size_t);
int __cdecl wctomb(char *, wchar_t);

/* private extensions to standard C */
size_t __cdecl _msize(void *);
void * __cdecl _alloca(size_t);
void __cdecl _searchenv(const char *, const char *, char *);
char * __cdecl _fullpath(char *, const char *, size_t);
void __cdecl _splitpath(const char *, char *, char *, char *, char *);

/* internal stuff */
double __cdecl __stod(const char *, char **, long);
float __cdecl __stof(const char *, char **, long);
long double __cdecl __stold(const char *, char **, long);
long long __cdecl __stoll(const char *, char **, int);
unsigned long __cdecl __stoul(const char *, char **, int);
unsigned long long __cdecl __stoull(const char *, char **, int);

/* data declarations */
extern char __mbcurmax;

/* macro overrides */
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

/*
 * Sizes for buffers used by the _makepath() and _splitpath() functions.
 * note that the sizes include space for 0-terminator
 */
#define _MAX_PATH   260 /* max. length of full pathname */
#define _MAX_DRIVE  3   /* max. length of drive component */
#define _MAX_DIR    256 /* max. length of path component */
#define _MAX_FNAME  256 /* max. length of file name component */
#define _MAX_EXT    256 /* max. length of extension component */

/*
 * Argument values for _set_error_mode().
 */
#define _OUT_TO_DEFAULT 0
#define _OUT_TO_STDERR  1
#define _OUT_TO_MSGBOX  2
#define _REPORT_ERRMODE 3

#endif /* _STDLIB_H */

