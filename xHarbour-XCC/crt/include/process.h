#ifndef _PROCESS_H
#define _PROCESS_H

/* process.h - private header for process control */

#ifndef _WINCE

/* modeflag values used by _spawn* routines */
#define _P_WAIT     0
#define _P_NOWAIT   1
#define _P_OVERLAY  2
#define _P_NOWAITO  3
#define _P_DETACH   4

/* declarations */
#ifdef __MT__
unsigned long __cdecl _beginthread(void (__cdecl *)(void *), unsigned int, void *);
void __cdecl _endthread(void);
#ifdef _MSC_EXTENSIONS  /* __stdcall requires this */
unsigned long __cdecl _beginthreadex(void *, unsigned int, unsigned int (__stdcall *)(void *), void *, unsigned int, unsigned int *);
void __cdecl _endthreadex(unsigned int);
#endif /* _MSC_EXTENSIONS */
#endif /* __MT__ */

int __cdecl _cwait(int *, int, int);
int __cdecl _getpid(void);
int __cdecl _spawnl(int, const char *, const char *, ...);
int __cdecl _spawnle(int, const char *, const char *, ...);
int __cdecl _spawnlp(int, const char *, const char *, ...);
int __cdecl _spawnlpe(int, const char *, const char *, ...);
int __cdecl _spawnv(int, const char *, /*const*/ char * const *);
int __cdecl _spawnve(int, const char *, /*const*/ char * const *, /*const*/ char * const *);
int __cdecl _spawnvp(int, const char *, /*const*/ char * const *);
int __cdecl _spawnvpe(int, const char *, /*const*/ char * const *, /*const*/ char * const *);

/* compatibility names */
#ifdef __POCC__OLDNAMES
#define P_WAIT  _P_WAIT
#define P_NOWAIT  _P_NOWAIT
#define P_OVERLAY  _P_OVERLAY
#define P_NOWAITO  _P_NOWAITO
#define P_DETACH  _P_DETACH

int __cdecl cwait(int *, int, int);
int __cdecl getpid(void);
int __cdecl spawnl(int, const char *, const char *, ...);
int __cdecl spawnle(int, const char *, const char *, ...);
int __cdecl spawnlp(int, const char *, const char *, ...);
int __cdecl spawnlpe(int, const char *, const char *, ...);
int __cdecl spawnv(int, const char *, /*const*/ char * const *);
int __cdecl spawnve(int, const char *, /*const*/ char * const *, /*const*/ char * const *);
int __cdecl spawnvp(int, const char *, /*const*/ char * const *);
int __cdecl spawnvpe(int, const char *, /*const*/ char * const *, /*const*/ char * const *);
#endif /* __POCC__OLDNAMES */

#endif /* _WINCE */

#endif /* _PROCESS_H */

