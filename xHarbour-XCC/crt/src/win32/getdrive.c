/****************************************************************************
 *                                                                          *
 * File    : getdrive.c                                                     *
 *                                                                          *
 * Purpose : _getdrive function -- win32 version.                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-01-25  Created                                              *
 *                                                                          *
 ****************************************************************************/

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <stdio.h>
#include "xcrt.h"

/* return the current disk drive */
int __cdecl (_getdrive)(void)
{
    unsigned long drivenum = 0;
    char abspath[FILENAME_MAX+1];

    if (GetCurrentDirectoryA(sizeof(abspath), abspath) && abspath[1] == ':')
        drivenum = toupper(abspath[0]) - 'A' + 1;

    return drivenum;
}

