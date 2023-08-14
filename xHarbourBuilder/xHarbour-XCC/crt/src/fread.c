/****************************************************************************
 *                                                                          *
 * File    : fread.c                                                        *
 *                                                                          *
 * Purpose : fread function.                                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-02  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <string.h>
#include "xstdio.h"

/* read into array from stream */
size_t __cdecl (fread)(void * restrict ptr, size_t size, size_t nelem, FILE * restrict str)
{
    size_t ns = size * nelem;
    unsigned char *s = (unsigned char *)ptr;

    if (ns == 0)
        return 0;

    _Lockfileatomic(str);

    if ((str->mode & _MBYTE) != 0)
    {
        for (; ns > 0 && str->backptr < str->backbuf + sizeof(str->backbuf); --ns)
            *s++ = *str->backptr++;
    }

    while (ns > 0)
    {
        /* ensure chars in buffer */
        if (str->getback != 0)
            str->getend = str->getback, str->getback = 0;

        if (str->ptr < str->getend)
            ;
        else if (__fread(str) <= 0)
            break;

        /* deliver as many as possible */
        {
            size_t m = str->getend - str->ptr;

            if (ns < m)
                m = ns;

            memcpy(s, str->ptr, m);
            s += m, ns -= m;
            str->ptr += m;
        }
    }

    _Unlockfileatomic(str);

    return (size * nelem - ns) / size;
}

