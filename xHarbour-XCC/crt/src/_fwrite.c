/****************************************************************************
 *                                                                          *
 * File    : _fwrite.c                                                      *
 *                                                                          *
 * Purpose : __fwrite function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-06  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>
#include "xstdio.h"

/* prepare stream for writing */
int __fwrite(FILE *str)
{
    if (str->ptr < str->putend)
        return 0;
    else if ((str->mode & (_MOPENW|_MREAD|_MWIDE)) != _MOPENW)
    {
        /* can't write after read */
        str->mode |= (str->mode & _MWIDE) ? _MERR : _MERR|_MBYTE;
        return -1;
    }
    else if ((str->mode & (_MWRITE|_MBYTE)) != (_MWRITE|_MBYTE))
        ;  /* haven't been writing */
    else if (str->ptr < str->bufend)
        ;  /* open up rest of existing buffer */
    else if (fflush(str))
        return -1;  /* failed to flush full buffer */

    if ((str->mode & (_MNBF|_MLBF)) != 0 || str->buf != &str->cbuf)
        ;  /* no need to buy a buffer */
    else if ((str->buf = (unsigned char *)malloc(BUFSIZ)) == 0)
    {
        /* use 1-char cbuf */
        str->buf = &str->cbuf;
        str->ptr = str->buf;
        str->bufend = str->buf + 1;
        __init_closeall();
    }
    else
    {
        /* use allocated buffer */
        str->mode |= _MALBUF;
        str->ptr = str->buf;
        str->bufend = str->buf + BUFSIZ;
        str->wgetend = str->buf;
        str->wputend = str->buf;
        __init_closeall();
    }
    str->getend = str->buf;
    str->putend = str->bufend;
    str->mode |= _MWRITE|_MBYTE;

    return 0;
}

