/****************************************************************************
 *                                                                          *
 * File    : gets.c                                                         *
 *                                                                          *
 * Purpose : gets function.                                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-05  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <string.h>
#include "xstdio.h"

/* get a line from stdio */
char * __cdecl (gets)(char *buf)
{
    unsigned char *s = (unsigned char *)buf;  /* bugfix 02-08-27! */
    /* unsigned char *s; */

    _Lockfileatomic(stdin);

    if ((stdin->mode & _MBYTE) != 0)
    {
        /* deliver pushed back chars */
        for (s = (unsigned char *)buf; stdin->backptr < stdin->backbuf + sizeof(stdin->backbuf); )
        {
            *s = *stdin->backptr++;
            if (*s++ == '\n')
            {
                /* terminate full line */
                s[-1] = '\0';
                _Unlockfileatomic(stdin);
                return buf;
            }
        }
    }

    /* ensure chars in buffer */
    for (;;)
    {
        if (stdin->getback != 0)
            stdin->getend = stdin->getback, stdin->getback = 0;
        if (stdin->ptr < stdin->getend)
            ;
        else if (__fread(stdin) < 0)
        {
            /* nothing to read */
            _Unlockfileatomic(stdin);
            return 0;
        }
        else if (stdin->mode & _MEOF)
            break;

        /* deliver as many as possible */
        {
            unsigned char *s1 = (unsigned char *)memchr((void *)stdin->ptr, '\n', stdin->getend - stdin->ptr);
            size_t m = (s1 ? s1 + 1 : stdin->getend) - stdin->ptr;

            memcpy(s, stdin->ptr, m);
            s += m; stdin->ptr += m;
            if (s1 != 0)
            {
                /* terminate full line */
                s[-1] = '\0';
                _Unlockfileatomic(stdin);
                return buf;
            }
        }
    }

    if (s == (unsigned char *)buf)
        buf = 0;
    else
        *s = '\0';

    _Unlockfileatomic(stdin);
    return buf;
}

