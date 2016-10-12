/****************************************************************************
 *                                                                          *
 * File    : ungetwc.c                                                      *
 *                                                                          *
 * Purpose : ungetwc function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-02  Added _Lockfileatomic() and _Unlockfileatomic().     *
 *                                                                          *
 ****************************************************************************/

#include "xwstdio.h"

/* push character back on wide stream */
wint_t __cdecl (ungetwc)(wint_t c, FILE *str)
{
    _Lockfileatomic(str);

    if (c == WEOF || str->wbackptr <= str->wbackbuf || (str->mode & (_MOPENR|_MWRITE|_MBYTE)) != _MOPENR)
    {
        c = WEOF;
    }
    else
    {
        /* pushback permitted, do it */
        str->mode = str->mode & ~_MEOF|(_MREAD|_MWIDE);
        *--str->wbackptr = c;
    }

    _Unlockfileatomic(str);
    return c;
}

