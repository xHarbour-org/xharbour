/****************************************************************************
 *                                                                          *
 * File    : rename.c                                                       *
 *                                                                          *
 * Purpose : rename function -- win32 version.                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include "xstdio.h"
#include "xcrt.h"

/* rename a file */
int __cdecl (rename)(const char *oldnm, const char *newnm)
{
    if (!MoveFileA(oldnm, newnm))
    {
        /* error occured -- map error code and return */
        __maposerr(GetLastError());
        return -1;
    }

    return 0;
}


