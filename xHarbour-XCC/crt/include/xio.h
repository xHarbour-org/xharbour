#ifndef _XIO_H
#define _XIO_H

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <io.h>
#include <errno.h>

/* xio.h - internal header */

/*
 * Control structure for lowio file handles.
 */
typedef struct {
    HANDLE osfhnd;  /* underlying OS file handle */
    char osfile;    /* attributes of file (e.g., open in text mode?) */
    char pipech;    /* one char buffer for handles opened on pipes */
#ifdef __MT__
    int lockinit;   /* lock initialization flag */
    CRITICAL_SECTION cslock;
#endif /* __MT__ */
} ioinfo;

/* log base 2 of the number of elements in each array of ioinfo structs */
#define IOINFO_L2E  5

/* number of elements in ioinfo array */
#define IOINFO_ARRAY_ELEMS  (1 << IOINFO_L2E)

/* maximum number of supported ioinfo arrays */
#define IOINFO_ARRAYS  64

/* maximum number of supported handles */
#define IOINFO_MAX_HANDLES  (IOINFO_ARRAYS * IOINFO_ARRAY_ELEMS)

/* array of arrays of control structures for lowio files */
extern ioinfo * __iotab[];

/* current number of allocated ioinfo structures (IOINFO_MAX_HANDLES is the upper limit) */
extern int __iolim;

/* __osfile flag values for DOS file handles */
#define FOPEN       0x01    /* file handle open */
#define FEOFLAG     0x02    /* end of file has been encountered */
#define FCRLF       0x04    /* CR-LF across read buffer (in text mode) */
#define FPIPE       0x08    /* file handle refers to a pipe */
#define FNOINHERIT  0x10    /* file handle opened _O_NOINHERIT */
#define FAPPEND     0x20    /* file handle opened O_APPEND */
#define FDEV        0x40    /* file handle refers to device */
#define FTEXT       0x80    /* file handle is in text mode */

/* declarations */
void __ioinit(void);
void __ioterm(void);
int __new_osfhnd(void);
int __free_osfhnd(int);
int __set_osfhnd(int, HANDLE);
HANDLE __get_osfhnd(int);
#ifdef __MT__
void __lock_osfhnd(int);
void __unlock_osfhnd(int);
#endif /* __MT__ */

/* macros for getting at an ioinfo struct */
#define _pioinfo(i) (__iotab[(i) >> IOINFO_L2E] + ((i) & (IOINFO_ARRAY_ELEMS - 1)))
#define _osfhnd(i)  (_pioinfo(i)->osfhnd)
#define _osfile(i)  (_pioinfo(i)->osfile)
#define _pipech(i)  (_pioinfo(i)->pipech)

#endif /* _XIO_H */

