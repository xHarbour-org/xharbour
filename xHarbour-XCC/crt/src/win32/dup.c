/****************************************************************************
 *                                                                          *
 * File    : dup.c                                                          *
 *                                                                          *
 * Purpose : lowio _dup function -- win32 version.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xio.h"
#include "xcrt.h"

/* duplicate a file handle */
int __cdecl (_dup)(int fh)
{
    int newfh;
    char fileinfo;
    HANDLE new_osfhandle;

    /* validate handle */
    if ((unsigned)fh >= (unsigned)__iolim || !(_osfile(fh) & FOPEN))
    {
        errno = EBADF;
        return -1;
    }

    fileinfo = _osfile(fh);  /* get file info for file */

    /* create duplicate handle */
    if ((newfh = __new_osfhnd()) == -1)
    {
        errno = EMFILE;  /* too many files error */
        return -1;
    }

    /* duplicate the file handle */
    if (!(DuplicateHandle(GetCurrentProcess(), __get_osfhnd(fh),
        GetCurrentProcess(), &new_osfhandle, 0L, TRUE, DUPLICATE_SAME_ACCESS)))
    {
        __maposerr(GetLastError());
        return -1;
    }
    else
    {
        __set_osfhnd(newfh, new_osfhandle);
    }

    /* copy _osfile value, with the FNOINHERIT bit cleared */
    _osfile(newfh) = fileinfo & ~FNOINHERIT;

    return newfh;
}

