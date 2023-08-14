#ifndef _XTHREAD_H
#define _XTHREAD_H

#ifdef __MT__
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <process.h>

/* xthread.h - internal header */

#define _HEAP_LOCK      0       /* lock for heap allocator routines */
#define _SIGNAL_LOCK    1       /* lock for signal() */
#define _EXIT_LOCK      2       /* lock for exit code */
#define _LOCKTAB_LOCK   3       /* lock to protect semaphore lock table */
#define _OSFHND_LOCK    4       /* lock to protect _osfhnd array */
#define _LOCALE_LOCK    5       /* lock for locale */
#define _STREAM_LOCK    6       /* lock for file streams */
#define _NLOCKS         7       /* one more than last index */

/* structure for each thread's data */
typedef struct {
    unsigned long tid;          /* thread ID */
    unsigned long thandle;      /* thread handle */
    int terrno;                 /* errno value */
    int randinit;               /* rand() initialization flag */
    unsigned long randseed;     /* rand() seed value */
    unsigned long randidx;      /* rand() value */
    char *token;                /* ptr to strtok() token */
    /*
     * following pointers get malloc'd at runtime.
     */
    unsigned long *randarr;     /* ptr to rand() array */
    char *errbuf;               /* ptr to strerror() buffer */
    char *tmpnambuf;            /* ptr to tmpnam() buffer */
    char *asctimebuf;           /* ptr to asctime() buffer */
    void *timebuf;              /* ptr to __ttotm() buffer */
    void *sigtab;               /* ptr to signal table */
    /*
     * following fields are needed by _beginthread code.
     */
    void *initaddr;             /* initial user thread address */
    void *initarg;              /* initial user thread argument */
} tiddata;

/* declarations */
int __mtinit(void);
void __mtterm(void);
void __init_mtd(tiddata *);
tiddata *__get_mtd(void);
void __free_mtd(tiddata *);
void __mtinitlocks(void);
void __mttermlocks(void);
void __mtlock(int);
void __mtunlock(int);

extern unsigned long __tlsindex;

#endif /* __MT__ */
#endif /* _XTHREAD_H */

