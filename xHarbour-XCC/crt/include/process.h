#ifndef _PROCESS_H
#define _PROCESS_H

/* process.h - private header for process control */

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
unsigned long __cdecl _beginthreadex(void *, unsigned int, unsigned int (__stdcall *)(void *), void *, unsigned int, unsigned int *);
void __cdecl _endthreadex(unsigned int);
#endif /* __MT__ */

int __cdecl _cwait(int *, int, int);
int __cdecl _spawnl(int, const char *, const char *, ...);
int __cdecl _spawnle(int, const char *, const char *, ...);
int __cdecl _spawnlp(int, const char *, const char *, ...);
int __cdecl _spawnlpe(int, const char *, const char *, ...);
int __cdecl _spawnv(int, const char *, /*const*/ char * const *);
int __cdecl _spawnve(int, const char *, /*const*/ char * const *, /*const*/ char * const *);
int __cdecl _spawnvp(int, const char *, /*const*/ char * const *);
int __cdecl _spawnvpe(int, const char *, /*const*/ char * const *, /*const*/ char * const *);

#endif /* _PROCESS_H */

