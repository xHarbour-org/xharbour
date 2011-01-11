/****************************************************************************
 *                                                                          *
 * File    : _fgetpos.c                                                     *
 *                                                                          *
 * Purpose : __fgetpos function -- win32 version.                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xstdio.h"
#include "xio.h"
#include "xcrt.h"

/* number of text bytes in nonempty buffer */
int _Nnl(FILE *str, unsigned char *p1, unsigned char *p2)
{
    int n;

    if (str->mode & _MBIN)
        return (p1 < p2) ? p2 - p1 : 0;

    for (n = 0; p1 < p2; ++p1, ++n)
        if (*p1 == '\n') ++n;

    return n;
}

/* get file position */
long __fgetpos(FILE *str, fpos_t *ptr)
{
    long loff = _lseek(str->fh, 0L, 1);

    if (!(str->mode & (_MOPENR|_MOPENW)) || loff == -1)
    {
        /* query failed */
        errno = EFPOS;
        return EOF;
    }

    if (str->mode & _MWRITE)
        loff += _Nnl(str, str->buf, str->ptr);
    else if (str->mode & _MREAD)
        loff -= _Nnl(str, str->backptr, str ->backbuf + sizeof(str->backbuf)) +
                _Nnl(str, str->ptr, str->getback != 0 ? str->getback : str->getend) +
                _Nnl(str, str->ptr, str->wgetend);

    if (ptr == 0)
    {
        /* ftell */
        return loff;
    }
    else
    {
        /* fgetpos */
        ptr->off = loff;
        ptr->wstate = str->wstate;
        return 0;
    }
}

