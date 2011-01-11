/****************************************************************************
 *                                                                          *
 * File    : locking.c                                                      *
 *                                                                          *
 * Purpose : lowio _locking function -- win32 version.                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xio.h"
#include "xcrt.h"

/* locks or unlocks nbytes of a file */
int __cdecl (_locking)(int fh, int mode, long nbytes)
{
    unsigned long oserr;
    long lockoffset;
    int retry;

    /* validate handle */
    if ((unsigned)fh >= (unsigned)__iolim || !(_osfile(fh) & FOPEN))
    {
        errno = EBADF;
        return -1;
    }

    /* obtain current position in file by calling _lseek */
    lockoffset = _lseek(fh, 0L, 1);
    if (lockoffset == -1)
        return -1;

    /* set retry count based on mode */
    retry = (mode == _LK_LOCK || mode == _LK_RLCK) ? 9 : 0;

    /* ask OS to lock the file until success or retry count finished */
    for (;;)
    {
        oserr = 0;
        if (mode == _LK_UNLCK)
        {
            if (!UnlockFile(__get_osfhnd(fh), lockoffset, 0L, nbytes, 0L))
                oserr = GetLastError();
        }
        else
        {
            if (!LockFile(__get_osfhnd(fh), lockoffset, 0L, nbytes, 0L))
                oserr = GetLastError();
        }

        /* exit loop on success or retry exhausted */
        if (retry <= 0 || oserr == 0)
            break;

        Sleep(1000L);

        --retry;
    }

    if (oserr != 0)
    {
        /*
         * error occured -- file was already locked; if a blocking call,
         * then return EDEADLOCK, otherwise map error normally.
         */
        if (mode == _LK_LOCK || mode == _LK_RLCK)
            errno = EDEADLK;
        else
            __maposerr(oserr);
        return -1;
    }

    return 0;
}

