#ifndef _SETJMP_H
#define _SETJMP_H

/* setjmp.h - C99 standard header */

#ifndef _WINCE

/* macros */
#define setjmp(env)  _setjmp3((env),0)

/* type definitions */
typedef int jmp_buf[16];

/* declarations */
int __cdecl _setjmp3(jmp_buf, int);  /* MS compatible */
void __declspec(noreturn) __cdecl longjmp(jmp_buf, int);

#else /* _WINCE */

/* macros */
#define setjmp  _setjmp

/* type definitions */
typedef int jmp_buf[16];

/* declarations */
int __cdecl setjmp(jmp_buf);
void __declspec(noreturn) __cdecl longjmp(jmp_buf, int);

#endif /* _WINCE */

#endif /* _SETJMP_H */

