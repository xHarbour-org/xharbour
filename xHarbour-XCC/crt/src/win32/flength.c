/****************************************************************************
 *                                                                          *
 * File    : flength.c                                                      *
 *                                                                          *
 * Purpose : lowio _filelength function -- win32 version.                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stdio.h>
#include "xio.h"
#include "xcrt.h"

/* find length of a file */
long __cdecl (_filelength)(int fh)
{
    long length;
    long here;

    /* validate handle */
    if ((unsigned)fh >= (unsigned)__iolim || !(_osfile(fh) & FOPEN))
    {
        errno = EBADF;
        return -1;
    }

    /* seek to end to get length of file */
    if ((here = _lseek(fh, 0L, SEEK_CUR)) == -1L)
        length = -1L;  /* return error */
    else
    {
        length = _lseek(fh, 0L, SEEK_END);
        if (here != length)
            _lseek(fh, here, SEEK_SET);
    }

    return length;
}

