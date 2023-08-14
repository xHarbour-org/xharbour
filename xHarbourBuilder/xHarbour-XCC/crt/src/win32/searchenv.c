/****************************************************************************
 *                                                                          *
 * File    : searchenv.c                                                    *
 *                                                                          *
 * Purpose : _searchenv function -- win32 version.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-04-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "xcrt.h"

/* search for file along paths from environment variable */
void __cdecl (_searchenv)(const char *fname, const char *envname, char *path)
{
    char pathbuf[FILENAME_MAX+4];
    char *envp;

    if (_access(fname, 0) == 0)
    {
        /* exists, convert it to a fully qualified pathname and return */
        if (_fullpath(path, fname, FILENAME_MAX) == 0)
            *path = '\0';
        return;
    }

    if ((envp = getenv(envname)) == 0)
    {
        /* no such environment var. and not in cwd, so return empty string */
        *path = '\0';
        return;
    }

    while ((envp = __getpath(envp, pathbuf, FILENAME_MAX)) && *pathbuf)
    {
        /* path now holds nonempty pathname from envp, concatenate the file name and go */
        size_t len = strlen(pathbuf);
        char *p = pathbuf + len;
        int c;

        if (((c = p[-1]) != '/') && c != '\\' && c != ':')
        {
            /* add a trailing '\' */
            *p++ = '\\';
            len++;
        }
        /* p now points to character following trailing '/', '\' or ':' */
        if ((len + strlen(fname)) <= FILENAME_MAX)
        {
            strcpy(p, fname);
            if (_access(pathbuf, 0) == 0)
            {
                /* found a match, copy the full pathname into the caller's buffer */
                strcpy(path, pathbuf);
                return;
            }
        }
    }

    /* if we get here, we never found it, return empty string */
    *path = '\0';
}

