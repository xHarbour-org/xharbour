/****************************************************************************
 *                                                                          *
 * File    : _exit.c                                                        *
 *                                                                          *
 * Purpose : _Exit function -- win32 version.                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

/* kill the program immediately */
void __cdecl (_Exit)(int status)
{
    ExitProcess((unsigned int)status);
}


