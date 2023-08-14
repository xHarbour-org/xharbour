/****************************************************************************
 *                                                                          *
 * File    : _fopen.c                                                       *
 *                                                                          *
 * Purpose : __fopen function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-06  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include "xstdio.h"

/* open a stream */
FILE *__fopen(const char *name, const char *mods, FILE *str, int fd)
{
    /* make str safe for fclose, macros */
    unsigned short alloced;
    static const FILE init = { 0, _FD_INVALID };

    if (str == 0)
        return 0;

    alloced = str->mode & _MALFIL;
    *str = init;

    str->buf = &str->cbuf;
    str->ptr = &str->cbuf;
    str->getend = &str->cbuf, str->wgetend = &str->cbuf;
    str->putend = &str->cbuf, str->wputend = &str->cbuf;
    str->backptr = str->backbuf + sizeof(str->backbuf);
    str->wbackptr = str->wbackbuf + sizeof(str->wbackbuf) / sizeof(wchar_t);
    str->mode = alloced | ((*mods == 'r') ? _MOPENR : (*mods == 'w') ? _MCREAT|_MOPENW|_MTRUNC : (*mods == 'a') ? _MCREAT|_MOPENW|_MOPENA : 0);

    if ((str->mode & (_MOPENR|_MOPENW)) == 0)
    {
        /* bad mods */
        fclose(str);
        return 0;
    }

    while (*++mods == 'b' || *mods == '+')
    {
        if (*mods == 'b')
        {
            if (str->mode & _MBIN)
                break;
            else
                str->mode |= _MBIN;
        }
        else
        {
            if ((str->mode & (_MOPENR|_MOPENW)) == (_MOPENR|_MOPENW))
                break;
            else
                str->mode |= _MOPENR|_MOPENW;
        }
    }

    if (name != 0)
    {
        /* not an existing file, open by name */
        str->fh = ___fopen(name, str->mode, mods);
        if (!_FD_VALID(str->fh))
        {
            /* open failed */
            fclose(str);
            return 0;
        }
    }
    else if (!_FD_VALID(fd))
    {
        /* bad fd */
        fclose(str);
        return 0;
    }
    else
    {
        str->fh = fd;
    }

    __init_closeall();
    return str;
}

