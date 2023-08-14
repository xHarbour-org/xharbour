/****************************************************************************
 *                                                                          *
 * File    : lseek.c                                                        *
 *                                                                          *
 * Purpose : lowio _lseek function -- win32 version.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stdio.h>
#include "xio.h"
#include "xcrt.h"

#if SEEK_SET != FILE_BEGIN || SEEK_CUR != FILE_CURRENT || SEEK_END != FILE_END
#error C and Win32 seek constants not compatible.
#endif

/* move the file pointer */
long __cdecl (_lseek)(int fh, long pos, int mthd)
{
    unsigned long newpos;
    HANDLE osfhnd;

    /* validate handle */
    if ((unsigned)fh >= (unsigned)__iolim || !(_osfile(fh) & FOPEN))
    {
        errno = EBADF;
        return -1;
    }

    if ((osfhnd = __get_osfhnd(fh)) == INVALID_HANDLE_VALUE)
    {
        errno = EBADF;
        return -1;
    }

    /*
     * Moves the file pointer associated with fh to a new position.
     * The new position is pos bytes (pos may be negative) away
     * from the origin specified by mthd.
     *
     * If mthd == SEEK_SET, the origin in the beginning of file
     * If mthd == SEEK_CUR, the origin is the current file position
     * If mthd == SEEK_END, the origin is the end of the fil
     */
    if ((newpos = SetFilePointer(osfhnd, pos, 0, mthd)) == -1)
    {
        __maposerr(GetLastError());
        return -1;
    }

    _osfile(fh) &= ~FEOFLAG;  /* clear the ctrl-z flag on the file */
    return newpos;
}

