/****************************************************************************
 *                                                                          *
 * File    : splitpath.c                                                    *
 *                                                                          *
 * Purpose : _splitpath function -- win32 version.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-05-25  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "xcrt.h"

/* split a path name into its individual components */
void __cdecl (_splitpath)(const char *path, char *drive, char *dir, char *fname, char *ext)
{
    char *p;
    char *last_slash = 0, *dot = 0;
    size_t len;

    /*
     * we assume that the path argument has the following form, where any
     * or all of the components may be missing.
     *
     * <drive><dir><fname><ext>
     *
     * and each of the components has the following expected form(s)
     *
     * drive:
     *   0 to 2 characters, the last of which, if any, is a ':'
     *
     * dir:
     *   0 to FILENAME_MAX-1 characters in the form of an absolute path
     *   (leading '/' or '\') or relative path, the last of which, if
     *   any, must be a '/' or '\'.  E.g -
     *   absolute path:
     *     \top\next\last\     ; or
     *     /top/next/last/
     *   relative path:
     *     top\next\last\  ; or
     *     top/next/last/
     *   Mixed use of '/' and '\' within a path is also tolerated
     *
     * fname:
     *   0 to FILENAME_MAX-1 characters not including the '.' character
     *
     * ext:
     *   0 to FILENAME_MAX-1 characters where, if any, the first must be a '.'
     */

    /* extract drive letter and :, if any */

    if (strlen(path) >= 2 && *(path + 1) == ':')
    {
        if (drive)
        {
            strncpy(drive, path, 2);
            *(drive + 2) = '\0';
        }
        path += 2;
    }
    else if (drive)
    {
        *drive = '\0';
    }

    /*
     * Extract path string, if any. Path now points to the first character
     * of the path, if any, or the filename or extension, if no path was
     * specified. Scan ahead for the last occurence, if any, of a '/' or
     * '\' path separator character. If none is found, there is no path.
     * We will also note the last '.' character found, if any, to aid in
     * handling the extension.
     */
    for (last_slash = 0, p = (char *)path; *p; p++)
    {
        if (*p == '/' || *p == '\\')
            last_slash = p + 1;  /* point to one beyond for later copy */
        else if (*p == '.')
            dot = p;
    }

    if (last_slash)
    {
        /*
         * Found a path - copy up through last_slash or maximum
         * characters allowed, whichever is smaller.
         */
        if (dir)
        {
            len = last_slash - path;
            if (len >= FILENAME_MAX) len = FILENAME_MAX - 1;
            strncpy(dir, path, len);
            *(dir + len) = '\0';
        }
        path = last_slash;
    }
    else if (dir)
    {
        /* no path found */
        *dir = '\0';
    }

    /*
     * Extract file name and extension, if any. Path now points to the
     * first character of the file name, if any, or the extension if no
     * file name was given. Dot points to the '.' beginning the extension,
     * if any.
     */

    if (dot && dot >= path)
    {
        /* found the marker for an extension - copy the file name up to the '.' */
        if (fname)
        {
            len = dot - path;
            if (len >= FILENAME_MAX) len = FILENAME_MAX - 1;
            strncpy(fname, path, len);
            *(fname + len) = '\0';
        }
        /* Now we can get the extension - remember that p still points
         * to the terminating nul character of path.
         */
        if (ext)
        {
            len = p - dot;
            if (len >= FILENAME_MAX) len = FILENAME_MAX - 1;
            strncpy(ext, dot, len);
            *(ext + len) = '\0';
        }
    }
    else
    {
        /*
         * Found no extension, give empty extension and copy rest of
         * string into fname.
         */
        if (fname)
        {
            len = p - path;
            if (len >= FILENAME_MAX) len = FILENAME_MAX - 1;
            strncpy(fname, path, len);
            *(fname + len) = '\0';
        }
        if (ext)
        {
            *ext = '\0';
        }
    }
}

