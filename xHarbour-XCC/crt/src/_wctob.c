/****************************************************************************
 *                                                                          *
 * File    : _wctob.c                                                       *
 *                                                                          *
 * Purpose : __wctob function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <limits.h>
#include <stdio.h>
#include "xwchar.h"

/* internal function to translate wint_t */
int __cdecl (__wctob)(wint_t wc)
{
    if (wc == WEOF)
    {
        return EOF;
    }
    else
    {
        /* translate wc into buffer */
        char buf[MB_LEN_MAX];
        mbstate_t mbst = {0};

        return (__wctomb(buf, wc, &mbst) != 1) ? EOF : (unsigned char)buf[0];
    }
}

