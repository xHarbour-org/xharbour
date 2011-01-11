/****************************************************************************
 *                                                                          *
 * File    : ungetc.c                                                       *
 *                                                                          *
 * Purpose : ungetc function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-03  Added _Lockfileatomic() and _Unlockfileatomic().     *
 *                                                                          *
 ****************************************************************************/

#include "xstdio.h"

/* push character back on stream */
int __cdecl (ungetc)(int c, FILE *str)
{
    _Lockfileatomic(str);

    if (c == EOF || str->backptr <= str->backbuf || (str->mode & (_MOPENR|_MWRITE|_MWIDE)) != _MOPENR)
    {
        c = EOF;
    }
    else
    {
        /* pushback permitted, do it */
        str->mode = str->mode & ~_MEOF|(_MREAD|_MBYTE);
        if (str->getback == 0)
            str->getback = str->getend, str->getend = str->buf;
        *--str->backptr = c;
    }

    _Unlockfileatomic(str);
    return c;
}

