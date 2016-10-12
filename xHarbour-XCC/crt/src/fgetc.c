/****************************************************************************
 *                                                                          *
 * File    : fgetc.c                                                        *
 *                                                                          *
 * Purpose : fgetc function.                                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-02  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include "xstdio.h"

/* get a character from stream */
int __cdecl (fgetc)(FILE *str)
{
    int ch;

    _Lockfileatomic(str);

    if (str->backptr < str->backbuf + sizeof(str->backbuf) && (str->mode & _MBYTE) != 0)
    {
        /* deliver putback character */
        ch = *str->backptr++;

        _Unlockfileatomic(str);
        return ch;
    }

    if (str->getback != 0)
        str->getend = str->getback, str->getback = 0;

    if (str->ptr < str->getend)
        ch = *str->ptr++;
    else if (__fread(str) <= 0)
        ch = EOF;
    else
        ch = *str->ptr++;

    _Unlockfileatomic(str);
    return ch;
}

