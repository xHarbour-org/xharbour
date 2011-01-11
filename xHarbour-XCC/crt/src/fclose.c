/****************************************************************************
 *                                                                          *
 * File    : fclose.c                                                       *
 *                                                                          *
 * Purpose : fclose function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-02  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>
#include "xstdio.h"
#include "xthread.h"
#include "xcrt.h"

/* close all files at exit */
static void closeall(void)
{
    size_t i;

    for (i = 0; i < FOPEN_MAX; i++)
    {
        if (__filetab[i] != 0)
            fclose(__filetab[i]);
    }
}

/* register closeall with _Atexit */
void __init_closeall(void)
{
#ifdef __MT__
    __mtlock(_STREAM_LOCK);
    __try
    {
#endif /* __MT__ */
        static int init = 0;
        if (!init)
        {
            init = 1;
            _Atexit(&closeall);
        }
#ifdef __MT__
    }
    __finally
    {
        __mtunlock(_STREAM_LOCK);
    }
#endif /* __MT__ */
}

/* close a stream */
#ifdef __MT__
static int _mt_fclose(FILE *str)
#else
int __cdecl (fclose)(FILE *str)
#endif /* __MT__ */
{
    int stat = fflush(str);

    if (str->mode & _MALBUF)
        free(str->buf);

    str->buf = 0;

    if (_FD_VALID(str->fh) && _close(str->fh))
        stat = EOF;

    if (str->tmpnam != 0)
    {
        /* remove temp file */
        if (remove(str->tmpnam))
            stat = EOF;
        free(str->tmpnam), str->tmpnam = 0;
    }

    if (str->mode & _MALFIL)
    {
        /* find __filetab[i] entry and free */
        size_t i;

        for (i = 0; i < FOPEN_MAX; ++i)
        {
            if (__filetab[i] == str)
            {
                /* found entry */
                __filetab[i] = 0;
                break;
            }
        }

        free(str);
    }
    else
    {
        str->mode = 0;
        str->fh = _FD_INVALID;
        str->buf = &str->cbuf;
        str->ptr = &str->cbuf;
        str->getend = &str->cbuf, str->wgetend = &str->cbuf;
        str->putend = &str->cbuf, str->wputend = &str->cbuf;
        str->backptr = str->backbuf + sizeof(str->backbuf);
        str->wbackptr = str->wbackbuf + sizeof(str->wbackbuf) / sizeof(wchar_t);
    }

    return stat;
}

#ifdef __MT__
/* close a stream */
int __cdecl (fclose)(FILE *str)
{
    int stat;

    __mtlock(_STREAM_LOCK);
    __try
    {
        stat = _mt_fclose(str);
    }
    __finally
    {
        __mtunlock(_STREAM_LOCK);
    }

    return stat;
}
#endif /* __MT__ */

