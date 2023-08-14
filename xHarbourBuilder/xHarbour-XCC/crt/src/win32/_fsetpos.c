/****************************************************************************
 *                                                                          *
 * File    : _fsetpos.c                                                     *
 *                                                                          *
 * Purpose : __fsetpos function.                                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xstdio.h"
#include "xio.h"
#include "xcrt.h"

int _Nnl(FILE *, unsigned char *, unsigned char *);

/* position a file */
int __fsetpos(FILE *str, const fpos_t *ptr, long loff, int way)
{
    if (!(str->mode & (_MOPENR|_MOPENW)) || fflush(str))
    {
        /* not-open or write error */
        errno = EFPOS;
        return EOF;
    }

    if (ptr)
        loff += ptr->off;  /* fsetpos */

    if (way == SEEK_CUR && str->mode & _MREAD)
    {
        loff -= _Nnl(str, str->backptr,
                    str ->backbuf + sizeof(str->backbuf))
               +_Nnl(str, str->ptr,
                    str->getback != 0 ? str->getback : str->getend)
               +_Nnl(str, str->ptr, str->wgetend);
    }

    if (way == SEEK_CUR && loff != 0 || way == SEEK_END || way == SEEK_SET && loff != -1)
        loff = _lseek(str->fh, loff, way);

    if (loff == -1)
    {
        /* request failed */
        errno = EFPOS;
        return EOF;
    }
    else
    {
        /* success */
        if (str->mode & (_MREAD|_MWRITE))
        {
            /* empty buffer */
            str->ptr = str->buf;
            str->getend = str->buf, str->wgetend = str->buf;
            str->putend = str->buf, str->wputend = str->buf;
            str->backptr = str->backbuf + sizeof(str->backbuf);
            str->wbackptr = str->wbackbuf + sizeof(str->wbackbuf) / sizeof(wchar_t);
            str->getback = 0;
        }

        if (ptr)
            str->wstate = ptr->wstate;

        str->mode &= ~(_MEOF|_MREAD|_MWRITE);
        return 0;
    }
}

