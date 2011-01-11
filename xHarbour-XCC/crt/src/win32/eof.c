/****************************************************************************
 *                                                                          *
 * File    : eof.c                                                          *
 *                                                                          *
 * Purpose : lowio _eof function -- win32 version.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stdio.h>
#include "xio.h"
#include "xcrt.h"

/* test for end-of-file */
int __cdecl (_eof)(int fh)
{
    long here;
    long end;

    /* validate handle */
    if ((unsigned)fh >= (unsigned)__iolim || !(_osfile(fh) & FOPEN))
    {
        errno = EBADF;
        return -1;
    }

    /* See if the current position equals the end of the file */
    if ((here = _lseek(fh, 0L, SEEK_CUR)) == -1L || (end = _lseek(fh, 0L, SEEK_END)) == -1L)
        return -1;
    else if (here == end)
        return 1;
    else
    {
        _lseek(fh, here, SEEK_SET);
        return 0;
    }
}

