/****************************************************************************
 *                                                                          *
 * File    : chdrive.c                                                      *
 *                                                                          *
 * Purpose : _chdrive function -- win32 version.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           02-10-21  Created                                              *
 *                                                                          *
 ****************************************************************************/

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <errno.h>
#include "xcrt.h"

/* change current disk drive */
int __cdecl (_chdrive)(int drivenum)
{
    char newdrive[3];

    if (drivenum < 1 || drivenum > 31)
    {
        errno = EACCES;
        return -1;
    }

    newdrive[0] = (char)('A' + drivenum - 1);
    newdrive[1] = ':';
    newdrive[2] = '\0';

    /*
     * Set new drive. If current directory on new drive exists, it
     * will become the cwd. Otherwise defaults to root directory.
     */
    if (!SetCurrentDirectory(newdrive))
    {
        __maposerr(GetLastError());
        return -1;
    }

    return 0;
}

