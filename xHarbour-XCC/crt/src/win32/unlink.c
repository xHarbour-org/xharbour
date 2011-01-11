/****************************************************************************
 *                                                                          *
 * File    : unlink.c                                                       *
 *                                                                          *
 * Purpose : _unlink function -- win32 version.                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include "xstdio.h"
#include "xcrt.h"

/* unlink a file */
int __cdecl (_unlink)(const char *fname)
{
    if (!DeleteFileA(fname))
    {
        /* error occured -- map error code and return */
        __maposerr(GetLastError());
        return -1;
    }

    return 0;
}


