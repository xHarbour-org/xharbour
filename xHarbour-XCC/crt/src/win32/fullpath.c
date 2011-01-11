/****************************************************************************
 *                                                                          *
 * File    : fullpath.c                                                     *
 *                                                                          *
 * Purpose : _fullpath function -- win32 version.                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-04-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <direct.h>
#include <errno.h>
#include "xalloc.h"
#include "xcrt.h"

/* combines the current directory with path to form an absolute path */
char * __cdecl (_fullpath)(char *buffer, const char *path, size_t maxlen)
{
    unsigned long len;
    char *buf;
    char *fnamep;

    if (path == 0 || *path == '\0')
        return _getcwd(buffer, maxlen);

    /* allocate buffer if necessary */
    if (buffer == 0)
    {
        if ((buf = malloc(FILENAME_MAX * sizeof(char))) == 0)
        {
            errno = ENOMEM;
            return 0;
        }
        else
        {
            maxlen = FILENAME_MAX;
        }
    }
    else
    {
        buf = buffer;
    }

    len = GetFullPathName(path, maxlen, buf, &fnamep);

    if (len >= maxlen)
    {
        if (buffer == 0) free(buf);
        errno = ERANGE;
        return 0;
    }
    else if (len == 0)
    {
        unsigned long oserr = GetLastError();
        if (buffer == 0) free(buf);
        __maposerr(oserr);
        return 0;
    }

    return buf;
}

