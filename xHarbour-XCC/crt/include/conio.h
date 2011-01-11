#ifndef _CONIO_H
#define _CONIO_H

/* conio.h - private header for console I/O definitions */

/* declarations */
int __cdecl _getch(void);
int __cdecl _getche(void);
int __cdecl _kbhit(void);
int __cdecl _putch(int);
int __cdecl _ungetch(int);

#endif /* _CONIO_H */
