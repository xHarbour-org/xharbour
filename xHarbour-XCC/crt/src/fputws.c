/****************************************************************************
 *                                                                          *
 * File    : fputws.c                                                       *
 *                                                                          *
 * Purpose : fputws function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-02  Added _Lockfileatomic() and _Unlockfileatomic().     *
 *                                                                          *
 ****************************************************************************/

#include "xwstdio.h"

/* put a string to wide stream */
int __cdecl (fputws)(const wchar_t * restrict s, FILE * restrict str)
{
    _Lockfileatomic(str);

    for (; *s != '\0'; ++s)
    {
        if (fputwc(*s, str) == WEOF)
        {
            /* write failed */
            _Unlockfileatomic(str);
            return EOF;
        }
    }

    _Unlockfileatomic(str);
    return 0;
}

