/****************************************************************************
 *                                                                          *
 * File    : close.c                                                        *
 *                                                                          *
 * Purpose : lowio _close function -- win32 version.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xio.h"
#include "xcrt.h"

/* close a file handle */
int __cdecl (_close)(int fh)
{
    unsigned long oserr;

    /* validate handle */
    if ((unsigned)fh >= (unsigned)__iolim || !(_osfile(fh) & FOPEN))
    {
        errno = EBADF;
        return -1;
    }

    /*
     * Close the underlying OS file handle. Special cases:
     *   1. If __get_osfhnd(fh) is INVALID_HANDLE_VALUE, don't try
     *      to actually close it. Just reset the lowio info so the
     *      handle can be reused. The standard handles are setup like
     *      this in Windows app, or a background app.
     *   2. If fh is STDOUT or STDERR, and if STDOUT and STDERR are
     *      mapped to the same OS file handle, skip the CloseHandle.
     *      STDOUT and STDERR are the only handles for which this
     *      support is provided. Other handles are mapped to the same
     *      OS file handle only at the programmer's risk.
     */
    if (__get_osfhnd(fh) == INVALID_HANDLE_VALUE ||
        ((fh == 1 || fh == 2) && __get_osfhnd(1) == __get_osfhnd(2)) ||
        CloseHandle(__get_osfhnd(fh)))
    {
        oserr = 0;
    }
    else
    {
        oserr = GetLastError();
    }

    __free_osfhnd(fh);

    _osfile(fh) = 0;  /* clear file flags */

    if (oserr)
    {
        __maposerr(oserr);
        return -1;
    }

    return 0;
}

