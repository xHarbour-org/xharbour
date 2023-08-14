/****************************************************************************
 *                                                                          *
 * File    : fgetwc.c                                                       *
 *                                                                          *
 * Purpose : fgetwc function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-02  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <string.h>
#include "xwstdio.h"

/* get a wchar_t from wide stream */
wint_t __cdecl (fgetwc)(FILE *str)
{
    _Lockfileatomic(str);

    if (str->wbackptr < str->wbackbuf + sizeof(str->wbackbuf) / sizeof(wchar_t))
    {
        /* deliver putback character */
        wint_t ch = *str->wbackptr++;
        _Unlockfileatomic(str);
        return ch;
    }

    for (;;)
    {
        /* loop until wide char built */
        int nbuf, nc;
        int nback = str->backbuf + sizeof(str->backbuf) - str->backptr;
        unsigned char *pbuf;
        wchar_t wc;

        if (nback > 0 && (str->mode & _MWIDE) != 0)
            pbuf = str->backptr, nbuf = nback;
        else if (str->ptr < str->wgetend || __wfread(str) > 0)
            pbuf = str->ptr, nbuf = str->wgetend - str->ptr;
        else
        {
            /* nothing to read */
            _Unlockfileatomic(str);
            return WEOF;
        }

        switch (nc = __mbtowc(&wc, (const char *)pbuf, nbuf, &str->wstate))
        {
            /* check completion code */
            case -2:  /* not done yet */
                if (sizeof(str->backbuf) <= nbuf)
                    nback = 0;  /* more chars won't help, signal failure */
                else if (nback == 0)
                {
                    /* set up buffer in str->backbuf area */
                    str->backptr = str->backbuf + sizeof(str->backbuf) - nbuf;
                    memcpy(str->backptr, str->ptr, nbuf);
                    str->ptr += nbuf;
                    nback = nbuf;
                }

                if (nback == 0)
                    ;  /* report failure */
                else if (__wfread(str) > 0)
                {
                    /* add chars to backbuf buffer and retry */
                    nbuf = str->wgetend - str->ptr;
                    if (sizeof(str->backbuf) - nback < nbuf)
                        nbuf = sizeof(str->backbuf) - nback;
                    pbuf = str->backbuf + sizeof(str->backbuf) - nbuf - nback;
                    memmove(pbuf, str->backptr, nback);
                    memcpy(pbuf + nback, str->ptr, nbuf);
                    str->backptr = pbuf;
                    str->ptr += nbuf;
                    break;
                }
                /* fall through */

            case -1:  /* bad multibyte character */
                str->mode |= _MERR;
                _Unlockfileatomic(str);
                return WEOF;

            case 0:  /* may be null character */
                if (wc == L'\0')
                    nc = strlen((const char *)pbuf) + 1;
                /* fall through */

            default:  /* got a wide character */
                if (nback > 0)
                    str->backptr += nc;
                else
                    str->ptr += nc;

                _Unlockfileatomic(str);
                return wc;
        }
    }
}

