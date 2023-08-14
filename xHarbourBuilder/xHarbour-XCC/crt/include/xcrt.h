#ifndef _XCRT_H
#define _XCRT_H

#include <io.h>
#include <excpt.h>
#include <signal.h>
#include <stddef.h>

/* xcrt.h - internal header */

#define _NSIG  44

void _Atexit(void (*)(void));

#define __INITEXIT__  /* Ron Pinkas */
#ifdef __INITEXIT__
typedef void (__cdecl *INITEXIT)(void);
#endif

/* declarations */
int __xcptfilter(unsigned long, struct _EXCEPTION_POINTERS *);
char *__wincmdln(void);
void __clockinit(void);
void __setargv(void);
void __setenvp(void);
char *__getpath(const char *, char *, size_t);
int __cenvarg(/*const*/ char * const *, /*const*/ char * const *, char **, char **);
int __spawn(int, const char *, char *, char *);
void __maposerr(unsigned long);

/* data declarations */
extern int __argc;
extern char **__argv;
extern char *__envp;
extern __sigfunc *__sigtab[_NSIG];

#endif /* _XCRT_H */

