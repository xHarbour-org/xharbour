/****************************************************************************
 *                                                                          *
 * File    : putch.c                                                        *
 *                                                                          *
 * Purpose : console write function -- win32 version.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-05-25  Created                                              *
 *                                                                          *
 ****************************************************************************/

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <conio.h>
#include <stdio.h>

/* declaration for console handle */
static int _confh = -2;


/* one time initialization of console input */
static void init_conout(void)
{
    _confh = (int)CreateFile("CONOUT$", GENERIC_WRITE,
        FILE_SHARE_READ|FILE_SHARE_WRITE, 0, OPEN_EXISTING, 0, 0);
}


/* direct console output */
int __cdecl (_putch)(int c)
{
    unsigned char ch = (unsigned char)c;
    unsigned long nwritten;

    if (_confh == -2)
        init_conout();

    /* write character to console */
    if (_confh == -1 || !WriteConsole((HANDLE)_confh, (void *)&ch, 1, &nwritten, 0))
        return EOF;  /* return error indicator */

    return ch;
}

