/****************************************************************************
 *                                                                          *
 * File    : fwrite.c                                                       *
 *                                                                          *
 * Purpose : fwrite function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-02  Added _Lockfileatomic() and _Unlockfileatomic().     *
 *                                                                          *
 ****************************************************************************/

#include <string.h>
#include "xstdio.h"

/* write to stream from array */
size_t __cdecl (fwrite)(const void * restrict ptr, size_t size, size_t nelem, FILE * restrict str)
{
    char *s = (char *)ptr;
    size_t ns = size * nelem;

    if (ns == 0)
        return 0;
    else if (size == 0)
        return nelem;

    _Lockfileatomic(str);

    /* ensure room in buffer */
    while (ns > 0)
    {
        if (str->ptr < str->putend)
            ;
        else if (__fwrite(str) < 0)
            break;

        /* copy in as many as possible */
        {
            char *s1 = (str->mode & _MLBF) ? (char *)memchr((void *)s, '\n', ns) : 0;
            size_t m = (s1) ? s1 - s + 1 : ns;
            size_t n = str->putend - str->ptr;

            if (n < m)
                s1 = 0, m = n;
            memcpy(str->ptr, s, m);
            s += m, ns -= m;
            str->ptr += m;

            if (s1 && fflush(str))
                break;
        }
    }

    if (str->mode & _MNBF)
        fflush(str);

#if !defined(__MT__) || !_FILE_OP_LOCKS
    if ((str->mode & (_MNBF|_MLBF)) != 0)
        str->putend = str->ptr;  /* disable buffering */
#endif

    _Unlockfileatomic(str);

    return (size * nelem - ns) / size;
}

