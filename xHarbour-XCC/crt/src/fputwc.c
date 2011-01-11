/****************************************************************************
 *                                                                          *
 * File    : fputwc.c                                                       *
 *                                                                          *
 * Purpose : fputwc function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-02  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>
#include <string.h>
#include "xwstdio.h"

/* put a wchar_t to wide stream */
wint_t __cdecl (fputwc)(wchar_t c, FILE *str)
{
    int n;

    _Lockfileatomic(str);

    for (n = -1; n != 0;)
    {
        /* loop until all chars put */
        char buf[MB_LEN_MAX], *s;
        int m;

        if (str->ptr < str->wputend)
            ;
        else if (__wfwrite(str) < 0)
        {
            /* noplace to write */
            _Unlockfileatomic(str);
            return WEOF;
        }

        m = str->wputend - str->ptr;
        if (m == 0)
            m = 1;  /* __wfwrite supplies at least one place if successful */

        if (n > 0)
            ;  /* chars leftover from last pass */
        else if (m >= MB_CUR_MAX)
        {
            if ((n = __wctomb((char *)str->ptr, c, &str->wstate)) < 0)
            {
                /* bad conversion */
                _Unlockfileatomic(str);
                return WEOF;
            }
            else
            {
                /* count delivered characters */
                str->ptr += n;
                break;
            }
        }
        else if ((n = __wctomb(s = buf, c, &str->wstate)) < 0)
        {
            /* bad conversion */
            _Unlockfileatomic(str);
            return WEOF;
        }

        if (n < m)  /* deliver leftover chars */
            m = n;

        memcpy(str->ptr, s, m);
        s += m, n -= m;
        str->ptr += m;
    }

    if (((str->mode & _MNBF) != 0 || (str->mode & _MLBF) != 0 && c == L'\n') && fflush(str))
    {
        /* write failed */
        _Unlockfileatomic(str);
        return WEOF;
    }

#if !defined(__MT__) || !_FILE_OP_LOCKS
    if ((str->mode & (_MNBF|_MLBF)) != 0)
        str->wputend = str->ptr;  /* disable buffering */
#endif

    _Unlockfileatomic(str);
    return c;
}

