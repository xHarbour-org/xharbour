/****************************************************************************
 *                                                                          *
 * File    : getcwd.c                                                       *
 *                                                                          *
 * Purpose : _getcwd function -- win32 version.                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-01-25  Created                                              *
 *                                                                          *
 ****************************************************************************/

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <stdio.h>
#include <errno.h>
#include "xalloc.h"
#include "xcrt.h"

/* get current working directory */
char * __cdecl (_getcwd)(char *buffer, size_t maxlen)
{
    unsigned long len;
    char abspath[FILENAME_MAX+1];
    char *p;

    len = GetCurrentDirectoryA(sizeof(abspath), abspath);

    /* API call failed, or buffer not large enough */
    if (len == 0 || ++len > sizeof(abspath))
        return 0;

    /* set up the buffer */
    if ((p = buffer) == 0)
    {
        /* allocate a buffer for the user */
        p = (char *)malloc(len > maxlen ? len : maxlen);
        if (p == 0)
        {
            errno = ENOMEM;
            return 0;
        }
    }
    else if (len > maxlen)
    {
        /* won't fit in the user-supplied buffer! */
        errno = ERANGE;
        return 0;
    }

    return strcpy(p, abspath);
}

