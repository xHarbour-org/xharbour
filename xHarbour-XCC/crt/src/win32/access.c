/****************************************************************************
 *                                                                          *
 * File    : access.c                                                       *
 *                                                                          *
 * Purpose : lowio _access function -- win32 version.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xio.h"
#include "xcrt.h"

/*
 * mode:
 * 00 - existence only
 * 02 - write permission
 * 04 - read permission
 * 06 - read and write permission
 */

/* check if file can be accessed under mode */
int __cdecl (_access)(const char *fname, int mode)
{
    unsigned long attr;

    attr = GetFileAttributesA(fname);
    if (attr == 0xffffffff)
    {
        /* error occured -- map error code and return */
        __maposerr(GetLastError());
        return -1;
    }

    /* no error; see if returned premission settings OK */
    if ((attr & FILE_ATTRIBUTE_READONLY) && (mode & 2))
    {
        /* no write permission on file, return error */
        errno = EACCES;
        return -1;
    }

    /* file exists and has requested permission setting */
    return 0;
}

