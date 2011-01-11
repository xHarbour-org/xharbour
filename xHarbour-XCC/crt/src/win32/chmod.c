/****************************************************************************
 *                                                                          *
 * File    : chmod.c                                                        *
 *                                                                          *
 * Purpose : lowio _chmod function -- win32 version.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xio.h"
#include "xcrt.h"

/* change file mode */
int __cdecl (_chmod)(const char *fname, int mode)
{
    unsigned long attr;

    attr = GetFileAttributesA(fname);
    if (attr  == 0xffffffff)
    {
        /* error occured -- map error code and return */
        __maposerr(GetLastError());
        return -1;
    }

    if (mode & _S_IWRITE)
    {
        /* clear read only bit */
        attr &= ~FILE_ATTRIBUTE_READONLY;
    }
    else
    {
        /* set read only bit */
        attr |= FILE_ATTRIBUTE_READONLY;
    }

    /* set new attribute */
    if (!SetFileAttributesA(fname, attr))
    {
        /* error occured -- map error code and return */
        __maposerr(GetLastError());
        return -1;
    }

    return 0;
}

