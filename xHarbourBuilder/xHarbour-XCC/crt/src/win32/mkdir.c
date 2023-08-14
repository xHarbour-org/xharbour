/****************************************************************************
 *                                                                          *
 * File    : mkdir.c                                                        *
 *                                                                          *
 * Purpose : _mkdir function -- win32 version.                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-01-25  Created                                              *
 *                                                                          *
 ****************************************************************************/

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include "xcrt.h"

/* make a directory */
int __cdecl (_mkdir)(const char *path)
{
    if (!CreateDirectoryA(path, (SECURITY_ATTRIBUTES *)0))
    {
        /* error occured -- map error code and return */
        __maposerr(GetLastError());
        return -1;
    }

    return 0;
}

