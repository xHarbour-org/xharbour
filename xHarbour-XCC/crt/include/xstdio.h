#ifndef _XSTDIO_H
#define _XSTDIO_H

#include <limits.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdint.h>

/* xstdio.h - internal header */

#define _FILE_OP_LOCKS  0  /* 0 for no file atomic locks, 1 for atomic */

#define _FD_VALID(fd)  ((fd) >= 0)  /* fd is signed integer */
#define _FD_INVALID    (-1)

/* bits for mode in FILE */
#define _MOPENR  0x1
#define _MOPENW  0x2
#define _MOPENA  0x4
#define _MTRUNC  0x8
#define _MCREAT  0x10
#define _MBIN    0x20
#define _MALBUF  0x40
#define _MALFIL  0x80
#define _MEOF    0x100
#define _MERR    0x200
#define _MLBF    0x400
#define _MNBF    0x800
#define _MREAD   0x1000
#define _MWRITE  0x2000
#define _MBYTE   0x4000
#define _MWIDE   0x8000

/* codes for __printf and __scanf */
#define _FSP   0x01
#define _FPL   0x02
#define _FMI   0x04
#define _FNO   0x08
#define _FZE   0x10
#define _WMAX  (INT_MAX-9/10)

/* macros for __scanf and friends */
#define GET(px)         (++(px)->nchar, (*(px)->pfn)((px)->arg, 0, 1))
#define GETN(px)        (0 <= --(px)->nget ? GET(px) : (++(px)->nchar, EOF))
#define UNGET(px, ch)   (--(px)->nchar, (*(px)->pfn)((px)->arg, ch, 0))
#define UNGETN(px, ch)  do if ((ch) != EOF) UNGET(px, ch); else --(px)->nchar; while (0)

/* for parsing numerics */
#define _MAX_EXP_DIG  8
#define _MAX_INT_DIG  32
#define _MAX_SIG_DIG  36

/* macros for atomic file locking */
#if _FILE_OP_LOCKS
#define _Lockfileatomic(str)    _Lockfile(str)
#define _Unlockfileatomic(str)  _Unlockfile(str)
#else /* _FILE_OP_LOCKS */
#define _Lockfileatomic(str)    (void)0
#define _Unlockfileatomic(str)  (void)0
#endif /* _FILE_OP_LOCKS */

/* type definitions */
typedef struct {  /* print formatting information */
    union {
        long long li;
        long double ld;
    } v;
    void *(*pfn)(void *, const char *, size_t);
    void *arg;
    char *s;
    int n0, nz0, n1, nz1, n2, nz2, prec, nchar, width;
    unsigned short flags;
    char qual;
} __prtinfo;

typedef struct {  /* scan formatting information */
    int (*pfn)(void *, int, int);
    void *arg;
    va_list ap;
    const char *s;
    int nchar, nget, width;
    char noconv, qual, stored;
} __scninfo;

/* declarations */
void __init_closeall(void);
FILE * __fslot(void);
FILE * __fopen(const char *, const char *, FILE *, int);
int ___fopen(const char *, unsigned int, const char *);
int __fread(FILE *);
int __fwrite(FILE *);
void __genld(__prtinfo *, char, char *, short, short);
int __getfield(__scninfo *);
int __getflt(__scninfo *);
int __getint(__scninfo *);
int __getstr(__scninfo *, int);
void __ldtob(__prtinfo *, char);
void __litob(__prtinfo *, char);
int __printf(void *(*)(void *, const char *, size_t), void *, const char *, va_list);
int __putfield(__prtinfo *, va_list *, char, char *);
int __putstr(__prtinfo *, const wchar_t *);
int __puttxt(__prtinfo *, const char *);
int __scanf(int (*)(void *, int, int), void *, const char *, va_list);

#endif /* _XSTDIO_H */

