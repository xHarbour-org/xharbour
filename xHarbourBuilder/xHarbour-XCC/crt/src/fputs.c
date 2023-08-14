/****************************************************************************
 *                                                                          *
 * File    : fputs.c                                                        *
 *                                                                          *
 * Purpose : fputs function.                                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-02  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <string.h>
#include "xstdio.h"

/* put a string to stream */
int __cdecl (fputs)(const char * restrict s, FILE * restrict str)
{
    _Lockfileatomic(str);

    while (*s != '\0')
    {
        /* ensure room in buffer */
        if (str->ptr < str->putend)
            ;
        else if (__fwrite(str) < 0)
        {
            /* noplace to write */
            _Unlockfileatomic(str);
            return EOF;
        }

        /* copy in as many as possible */
        {
            const char *s1 = (str->mode & _MLBF) ? strrchr(s, '\n') : 0;
            size_t m = (s1 != 0) ? s1 - s + 1 : strlen(s);
            size_t n;

            n = str->putend - str->ptr;
            if (n < m)
                s1 = 0, m = n;
            memcpy(str->ptr, s, m);
            s += m;
            str->ptr += m;
            if (s1 != 0 && fflush(str))
            {
                /* write failed */
                _Unlockfileatomic(str);
                return EOF;
            }
        }
    }

    if ((str->mode & _MNBF) != 0 && fflush(str))
    {
        /* write failed */
        _Unlockfileatomic(str);
        return EOF;
    }

#if !defined(__MT__) || !_FILE_OP_LOCKS
    if ((str->mode & (_MNBF|_MLBF)) != 0)
        str->putend = str->ptr;   /* disable buffering */
#endif

    _Unlockfileatomic(str);
    return 0;
}

