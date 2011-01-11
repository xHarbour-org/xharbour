#ifndef _CONIO_H
#define _CONIO_H

/* conio.h - private header for console I/O definitions */

#ifndef _WINCE

/* declarations */
int __cdecl _getch(void);
int __cdecl _getche(void);
int __cdecl _kbhit(void);
int __cdecl _putch(int);
int __cdecl _ungetch(int);

int __cdecl _inp(unsigned short);
unsigned short __cdecl _inpw(unsigned short);
unsigned long __cdecl _inpd(unsigned short);
int __cdecl _outp(unsigned short, int);
unsigned short __cdecl _outpw(unsigned short, unsigned short);
unsigned long __cdecl _outpd(unsigned short, unsigned long);

/* compatibility names */
#ifdef __POCC__OLDNAMES
int __cdecl getch(void);
int __cdecl getche(void);
int __cdecl kbhit(void);
int __cdecl putch(int);
int __cdecl ungetch(int);
int __cdecl inp(unsigned short);
unsigned short __cdecl inpw(unsigned short);
int __cdecl outp(unsigned short, int);
unsigned short __cdecl outpw(unsigned short, unsigned short);
#endif /* __POCC__OLDNAMES */

#endif  /* _WINCE */

#endif /* _CONIO_H */
