/****************************************************************************
 *                                                                          *
 * File    : fputc.c                                                        *
 *                                                                          *
 * Purpose : fputc function.                                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-02  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include "xstdio.h"

/* put a character to stream */
int __cdecl (fputc)(int ci, FILE *str)
{
    unsigned char c = ci;

    _Lockfileatomic(str);

    if (str->ptr < str->putend)
        ;
    else if (__fwrite(str) < 0)
    {
        /* noplace to write */
        _Unlockfileatomic(str);
        return EOF;
    }

    *str->ptr++ = c;
    if (((str->mode & _MNBF) != 0 || (str->mode & _MLBF) != 0 && c == '\n') && fflush(str))
    {
        /* write failed */
        _Unlockfileatomic(str);
        return EOF;
    }

#if !defined(__MT__) || !_FILE_OP_LOCKS
    if ((str->mode & (_MNBF|_MLBF)) != 0)
        str->putend = str->ptr;  /* disable buffering */
#endif

    _Unlockfileatomic(str);
    return c;
}

