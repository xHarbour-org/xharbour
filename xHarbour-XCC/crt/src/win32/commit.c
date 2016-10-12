/****************************************************************************
 *                                                                          *
 * File    : commit.c                                                       *
 *                                                                          *
 * Purpose : lowio _commit function -- win32 version.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xio.h"
#include "xcrt.h"

/* Flushes cache buffers for the file handle to disk */
int __cdecl (_commit)(int fh)
{
    /* validate handle */
    if ((unsigned)fh >= (unsigned)__iolim || !(_osfile(fh) & FOPEN))
    {
        errno = EBADF;
        return -1;
    }

    if (!FlushFileBuffers(__get_osfhnd(fh)))
    {
        /* error occured -- map error code and return */
        __maposerr(GetLastError());
        return -1;
    }

    return 0;
}

