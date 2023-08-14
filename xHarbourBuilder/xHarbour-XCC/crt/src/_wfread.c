/****************************************************************************
 *                                                                          *
 * File    : _wfread.c                                                      *
 *                                                                          *
 * Purpose : __wfread function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-10  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>
#include "xwstdio.h"
#include "xcrt.h"

/* prepare stream for reading */
int __wfread(FILE *str)
{
    if (str->ptr < str->wgetend)
        return 1;
    else if (str->mode & _MEOF)
        return 0;
    else if ((str->mode & (_MOPENR|_MWRITE|_MBYTE)) != _MOPENR)
    {
        /* can't read after write */
        str->mode |= (str->mode & _MBYTE) ? _MERR : _MERR|_MWIDE;
        return -1;
    }

    if ((str->mode & (_MNBF | _MLBF)) != 0 || str->buf != &str->cbuf)
        ;
    else if ((str->buf = (unsigned char *)malloc(BUFSIZ)) == 0)
    {
        /* use 1-char cbuf */
        str->buf = &str->cbuf;
        str->bufend = str->buf + 1;
    }
    else
    {
        /* use allocated buffer */
        str->mode |= _MALBUF;
        str->bufend = str->buf + BUFSIZ;
        str->getend = str->buf;
        str->putend = str->buf;
    }
    str->ptr = str->buf;
    str->wgetend = str->buf;
    str->wputend = str->buf;

    /* try to read into buffer */
    {
        int n = _read(str->fh, str->buf, str->bufend - str->buf);
        if (n < 0)
        {
            /* report error and fail */
            str->mode |= _MERR|_MWIDE;
            return -1;
        }
        else if (n == 0)
        {
            /* report end of file */
            str->mode = (str->mode & ~_MREAD)|_MEOF|_MWIDE;
            return 0;
        }
        else
        {
            /* set up data read */
            str->mode |= _MREAD|_MWIDE;
            str->wgetend += n;
            return 1;
        }
    }
}

