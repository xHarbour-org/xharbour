#ifndef _SETJMP_H
#define _SETJMP_H

/* setjmp.h - C99 standard header */

/* macros */
#define setjmp(env)  _setjmp3((env),0)

/* type definitions */
typedef int jmp_buf[16];

/* declarations */
int __cdecl _setjmp3(jmp_buf, int);  /* MS compatible */
void __cdecl longjmp(jmp_buf, int);

#endif /* _SETJMP_H */

