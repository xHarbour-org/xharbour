/****************************************************************************
 *                                                                          *
 * File    : rmdir.c                                                        *
 *                                                                          *
 * Purpose : _rmdir function -- win32 version.                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-01-25  Created                                              *
 *                                                                          *
 ****************************************************************************/

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include "xcrt.h"

/* remove a directory */
int __cdecl (_rmdir)(const char *path)
{
    if (!RemoveDirectoryA(path))
    {
        /* error occured -- map error code and return */
        __maposerr(GetLastError());
        return -1;
    }

    return 0;
}

