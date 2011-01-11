/****************************************************************************
 *                                                                          *
 * File    : isatty.c                                                       *
 *                                                                          *
 * Purpose : lowio _isatty function -- win32 version.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xio.h"
#include "xcrt.h"

/* check if file handle refers to a device */
int __cdecl (_isatty)(int fh)
{
    /* see if file handle is valid */
    if ((unsigned)fh >= (unsigned)__iolim)
        return 0;

    /* check file handle database to see if device bit set */
    return (int)(_osfile(fh) & FDEV);
}

