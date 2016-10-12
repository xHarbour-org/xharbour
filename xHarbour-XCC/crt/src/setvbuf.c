/****************************************************************************
 *                                                                          *
 * File    : setvbuf.c                                                      *
 *                                                                          *
 * Purpose : setvbuf function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-05  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>
#include "xstdio.h"

/* set up buffer for a stream */
int __cdecl (setvbuf)(FILE * restrict str, char * restrict abuf, int smode, size_t size)
{
    unsigned char *buf = (unsigned char *)abuf;
    int mode;

    mode = (smode == _IOFBF) ? 0 : (smode == _IOLBF) ? _MLBF : (smode == _IONBF) ? _MNBF : -1;
    if (mode == -1)
        return -1;

    _Lockfileatomic(str);

    if (str->mode & (_MREAD|_MWRITE))
    {
        /* file operation has already occurred */
        _Unlockfileatomic(str);
        return -1;
    }

    if (size == 0)
        buf = &str->cbuf, size = 1, mode = _MNBF;
    else if (size > INT_MAX)
        size = INT_MAX;

    if (buf)
        ;
    else if ((buf = (unsigned char *)malloc(size)) == 0)
    {
        /* can't allocate space */
        _Unlockfileatomic(str);
        return -1;
    }
    else
        mode |= _MALBUF;

    if (str->mode & _MALBUF)
        free(str->buf), str->mode &= ~_MALBUF;

    str->mode = str->mode & ~(_MLBF|_MNBF)|mode;
    str->buf = buf;
    str->bufend = buf + size;
    str->ptr = buf;
    str->getend = buf, str->wgetend = buf;
    str->putend = buf, str->wputend = buf;

    __init_closeall();
    _Unlockfileatomic(str);
    return 0;
}

