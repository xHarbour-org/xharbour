/****************************************************************************
 *                                                                          *
 * File    : fgets.c                                                        *
 *                                                                          *
 * Purpose : fgets function.                                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-02  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <string.h>
#include "xstdio.h"

/* get a line from stream */
char * __cdecl (fgets)(char * restrict buf, int n, FILE * restrict str)
{
    unsigned char *s = (unsigned char *)buf;  /* bugfix 01-10-18! */

    if (n <= 1)
        return 0;

    --n;

    _Lockfileatomic(str);

    if ((str->mode & _MBYTE) != 0)
    {
        for (s = (unsigned char *)buf; n > 0 && str->backptr < str->backbuf + sizeof(str->backbuf); --n)
        {
            /* deliver pushed back chars */
            *s = *str->backptr++;
            if (*s++ == '\n')
                n = 1;  /* terminate full line */
        }
    }

    while (n > 0)
    {
        /* ensure buffer has chars */
        if (str->getback != 0)
            str->getend = str->getback, str->getback = 0;
        if (str->ptr < str->getend)
            ;
        else if (__fread(str) < 0)
        {
            /* nothing to read */
            _Unlockfileatomic(str);
            return 0;
        }
        else if (str->mode & _MEOF)
            break;

        /* copy as many as possible */
        {
            unsigned char *s1 = (unsigned char *)memchr((void *)str->ptr, '\n', str->getend - str->ptr);
            size_t m = (s1 ? s1 + 1 : str->getend) - str->ptr;

            if (n < m)
                s1 = 0, m = n;

            memcpy(s, str->ptr, m);
            s += m, n -= m;
            str->ptr += m;

            if (s1 != 0)
                break;  /* full line, quit */
        }
    }

    if (s == (unsigned char *)buf)
        buf = 0;  /* nothing read, report failure */
    else
        *s = '\0';  /* terminate partial line */

    _Unlockfileatomic(str);
    return buf;
}

