/****************************************************************************
 *                                                                          *
 * File    : _wfwrite.c                                                     *
 *                                                                          *
 * Purpose : __wfwrite function.                                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-10  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>
#include "xwstdio.h"

/* prepare wide stream for writing */
int __wfwrite(FILE *str)
{
    if (str->ptr < str->wputend)
        return 0;
    else if ((str->mode & (_MOPENW|_MREAD|_MBYTE)) != _MOPENW)
    {
        /* can't write after read */
        str->mode |= (str->mode & _MBYTE) ? _MERR : _MERR|_MWIDE;
        return -1;
    }
    else if ((str->mode & (_MWRITE|_MWIDE)) != (_MWRITE|_MWIDE))
        ;  /* haven't been writing */
    else if (str->ptr < str->bufend)
        ;  /* open up rest of existing buffer */
    else if (fflush(str))
        return -1;  /* failed to flush full buffer */

    if ((str->mode & (_MNBF|_MLBF)) != 0 || str->buf != &str->cbuf)
        ;
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
        str->getend = str->buf;
        str->putend = str->buf;
        __init_closeall();
    }
    str->wgetend = str->buf;
    str->wputend = str->bufend;
    str->mode |= _MWRITE|_MWIDE;

    return 0;
}

