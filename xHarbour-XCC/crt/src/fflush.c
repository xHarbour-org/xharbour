/****************************************************************************
 *                                                                          *
 * File    : fflush.c                                                       *
 *                                                                          *
 * Purpose : fflush function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-02  Multi-threading support added.                       *
 *                                                                          *
 ****************************************************************************/

#include "xstdio.h"
#include "xthread.h"
#include "xcrt.h"

/* flush an output stream */
int __cdecl (fflush)(FILE *str)
{
    unsigned char *s;
    int n;

    if (str == 0)
    {
        /* recurse on all streams */
        int nf, stat;

#ifdef __MT__
        __mtlock(_STREAM_LOCK);
#endif
        for (stat = 0, nf = 0; nf < FOPEN_MAX; ++nf)
        {
            if (__filetab[nf] != 0 && fflush(__filetab[nf]) < 0)
                stat = EOF;
        }
#ifdef __MT__
        __mtunlock(_STREAM_LOCK);
#endif

        return stat;
    }

    _Lockfileatomic(str);

    if (!(str->mode & _MWRITE))
    {
        /* stream not writable */
        _Unlockfileatomic(str);
        return 0;
    }

    for (s = str->buf; s < str->ptr; s += n)
    {
        /* try to write buffer */
        n = _write(str->fh, s, str->ptr - s);
        if (n <= 0)
        {
            /* report error and fail */
            str->ptr = str->buf;
            str->putend = str->buf, str->wputend = str->buf;
            str->mode |= _MERR;

            _Unlockfileatomic(str);
            return EOF;
        }
    }

    str->ptr = str->buf;
    if ((str->mode & (_MNBF|_MLBF)) != 0)
        s = str->buf;
    else
        s = str->bufend;

    if (str->mode & _MBYTE)
        str->putend = s;
    else
        str->wputend = s;

    _Unlockfileatomic(str);
    return 0;
}


