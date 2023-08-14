/****************************************************************************
 *                                                                          *
 * File    : _btowc.c                                                       *
 *                                                                          *
 * Purpose : __btowc function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stdio.h>
#include "xwchar.h"

/* internal function to convert single byte */
wint_t __cdecl __btowc(int c)
{
    if (c == EOF)
    {
        return WEOF;
    }
    else
    {
        /* convert as one-byte string */
        mbstate_t mbst = {0};
        char ch;
        wchar_t wc;

        ch = c;
        return (__mbtowc(&wc, &ch, 1, &mbst) < 0) ? WEOF : wc;
    }
}

