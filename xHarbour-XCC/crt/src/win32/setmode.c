/****************************************************************************
 *                                                                          *
 * File    : setmode.c                                                      *
 *                                                                          *
 * Purpose : lowio setmode function -- win32 version.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <fcntl.h>
#include "xio.h"

/*
 * Changes file mode to text/binary, depending on mode arg.
 * This affects whether read's and write's on the file
 * translate between CRLF and LF or is untranslated.
 */

/* set file translation mode */
int __cdecl (_setmode)(int fh, int mode)
{
    int oldmode;

    /* validate handle */
    if ((unsigned)fh >= (unsigned)__iolim || !(_osfile(fh) & FOPEN))
    {
        errno = EBADF;
        return -1;
    }

    oldmode = _osfile(fh) & FTEXT;

    if (mode == _O_BINARY)
        _osfile(fh) &= ~FTEXT;
    else if (mode == _O_TEXT)
        _osfile(fh) |= FTEXT;
    else
    {
        errno = EINVAL;
        return -1;
    }

    return (oldmode) ? _O_TEXT : _O_BINARY;
}

