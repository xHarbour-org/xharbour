/****************************************************************************
 *                                                                          *
 * File    : chsize.c                                                       *
 *                                                                          *
 * Purpose : lowio _chsize function -- win32 version.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stdio.h>
#include <fcntl.h>
#include "xio.h"

#define _INTERNAL_BUFSIZ  4096

/* Change size of a file */
int __cdecl _chsize(int fh, long size)
{
    long filend;
    long extend;
    long place;
    int cnt;
    int oldmode;
    int retval = 0;  /* assume good return */

    /* validate handle */
    if ((unsigned)fh >= (unsigned)__iolim || !(_osfile(fh) & FOPEN))
    {
        errno = EBADF;
        return -1;
    }

    /* Get current file position and seek to end */
    if ((place = _lseek(fh, 0L, SEEK_CUR)) == -1L || (filend = _lseek(fh, 0L, SEEK_END)) == -1L)
        return -1;

    extend = size - filend;

    /* Grow or shrink the file as necessary */
    if (extend > 0L)
    {
        /*
         * Extending the file.
         */
        char blanks[_INTERNAL_BUFSIZ], *bl = blanks;

        memset(bl, '\0', _INTERNAL_BUFSIZ);
        oldmode = _setmode(fh, _O_BINARY);

        /* pad out with nulls */
        do
        {
            cnt = (extend >= (long)_INTERNAL_BUFSIZ) ? _INTERNAL_BUFSIZ : (int)extend;
            if ((cnt = _write(fh, bl, (extend >= (long)_INTERNAL_BUFSIZ) ? _INTERNAL_BUFSIZ : (int)extend)) == -1)
            {
                if (GetLastError() == ERROR_ACCESS_DENIED)
                    errno = EACCES;

                retval = cnt;
                break;  /* leave write loop */
            }
        } while ((extend -= (long)cnt) > 0L);

        _setmode(fh, oldmode);
        /* retval set correctly */
    }
    else if (extend < 0L)
    {
        /*
         * Shortening the file.
         */
        /*
         * Set file pointer to new eof...and truncate it there.
         */
        _lseek(fh, size, SEEK_SET);

        if ((retval = SetEndOfFile(__get_osfhnd(fh)) ? 0 : -1) == -1)
            errno = EACCES;
    }

    _lseek(fh, place, SEEK_SET);
    return retval;
}

