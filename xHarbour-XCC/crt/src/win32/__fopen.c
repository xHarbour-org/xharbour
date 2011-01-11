/****************************************************************************
 *                                                                          *
 * File    : __fopen.c                                                      *
 *                                                                          *
 * Purpose : ___fopen function -- win32 version.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <io.h>
#include <fcntl.h>
#include "xstdio.h"

#if (_MOPENR|_MOPENW) != 0x03
#error Bad assumption about _MOPENR|_MOPENW
#endif

#define DEF_PERMS  0x180

/* open a file */
int ___fopen(const char *fname, unsigned int smode, const char *mods)
{
    static const unsigned int rwacc[] = { 0, _O_RDONLY, _O_WRONLY, _O_RDWR };
    unsigned int oflag;

    oflag = rwacc[smode & 0x03];
    if (smode & _MOPENA)
        oflag |= _O_APPEND;
    if (smode & _MTRUNC)
        oflag |= _O_TRUNC;
    if (smode & _MCREAT)
        oflag |= _O_CREAT;
    if (smode & _MBIN)
        oflag |= _O_BINARY;
    else
        oflag |= _O_TEXT;

    return _open(fname, oflag, DEF_PERMS);
}

