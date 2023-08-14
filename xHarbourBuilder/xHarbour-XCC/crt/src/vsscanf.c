/****************************************************************************
 *                                                                          *
 * File    : vsscanf.c                                                      *
 *                                                                          *
 * Purpose : vsscanf function [new C99].                                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xstdio.h"

/* get or put a character */
static int scin(void *str, int ch, int getfl)
{
    char *s = *(char **)str;

    if (!getfl)
    {
        /* back up a char */
        *(char **)str = s - 1;
        return ch;
    }
    else if (*s == '\0')
    {
        return EOF;
    }
    else
    {
        /* deliver a char */
        *(char **)str = s + 1;
        return *s;
    }
}

/* read formatted from string to arg list */
int __cdecl (vsscanf)(const char * restrict buf, const char * restrict fmt, va_list ap)
{
    return __scanf(&scin, (void **)&buf, fmt, ap);
}

