#ifndef _ERRNO_H
#define _ERRNO_H

/* errno.h - C99 standard header */

/* error codes */
#define ENOENT   2
#define ENOEXEC  8
#define EBADF    9
#define ECHILD   10
#define EAGAIN   11
#define ENOMEM   12
#define EACCES   13
#define EEXIST   17
#define EXDEV    18
#define EINVAL   22
#define EMFILE   24
#define ENOSPC   28
#define EPIPE    32
#define EDOM     33
#define ERANGE   34
#define EFPOS    35
#define EDEADLK  36
#define EILSEQ   42

#ifndef _WINCE

#if defined(__MT__) || defined(_DLL)
extern int * __cdecl __errno(void);
#define errno  (*__errno())
#else
extern int errno;
#endif /* __MT__ */

#endif /* _WINCE */

#endif /* _ERRNO_H */

