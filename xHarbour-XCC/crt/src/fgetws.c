/****************************************************************************
 *                                                                          *
 * File    : fgetws.c                                                       *
 *                                                                          *
 * Purpose : fgetws function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-02  Added _Lockfileatomic() and _Unlockfileatomic().     *
 *                                                                          *
 ****************************************************************************/

#include "xwstdio.h"

/* get a wchar_t line from wide stream */
wchar_t * __cdecl (fgetws)(wchar_t * restrict buf, int n, FILE * restrict str)
{
    wchar_t *s = buf;

    if (n <= 1)
        return 0;

    _Lockfileatomic(str);

    /* get a wide character */
    while (--n > 0)
    {
        wint_t wc = fgetwc(str);

        if (wc == WEOF)
            break;

        *s++ = wc;

        if (wc == L'\n')
            break;
    }

    if (s == buf)
        buf = 0;
    else
        *s = L'\0';

    _Unlockfileatomic(str);

    return buf;
}

