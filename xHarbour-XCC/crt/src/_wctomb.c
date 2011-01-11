/****************************************************************************
 *                                                                          *
 * File    : _wctomb.c                                                      *
 *                                                                          *
 * Purpose : __wctomb function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-10  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <errno.h>
#include <limits.h>
#include <stdlib.h>
#include "xstate.h"
#include "xwchar.h"

/* translate widechar to multibyte */
int __wctomb(char *s, wchar_t wc, mbstate_t *pst)
{
    static const mbstate_t initial = {0};

    if (__mbtowctab.tab[0] == 0)
    {
        /* no table, convert to UTF8 */
        unsigned char *su = (unsigned char *)s;
        int nextra;

        if (s == 0)
        {
            /* set initial state */
            *pst = initial;
            return 0;
        }

#if WCHAR_MAX <= 0xff
        *su++ = (unsigned char)wc;
        return 1;
#else /* WCHAR_MAX <= 0xff */
        if ((wc & (wchar_t)~0x7f) == 0)
        {
            /* generate a single byte */
            *su++ = (unsigned char)wc;
            nextra = 0;
        }
        else if ((wc & (wchar_t)~0x7ff) == 0)
        {
            /* generate two bytes */
            *su++ = (unsigned char)(0xc0 | wc >> 6);
            nextra = 1;
        }
#if WCHAR_MAX <= 0xffff
        else
        {
            /* generate three bytes */
            *su++ = (unsigned char)(0xe0 | (wc >> 12) & 0x0f);
            nextra = 2;
        }
#else /* WCHAR_MAX <= 0xffff */
        else if ((wc & (wchar_t)~0xffff) == 0)
        {
            /* generate three bytes */
            *su++ = (unsigned char)(0xe0 | wc >> 12);
            nextra = 2;
        }
        else if ((wc & (wchar_t)~0x1fffff) == 0)
        {
            /* generate four bytes */
            *su++ = (unsigned char)(0xf0 | wc >> 18);
            nextra = 3;
        }
        else if ((wc & (wchar_t)~0x3ffffff) == 0)
        {
            /* generate five bytes */
            *su++ = (unsigned char)(0xf8 | wc >> 24);
            nextra = 4;
        }
        else
        {
            /* generate six bytes */
            *su++ = (unsigned char)(0xfc | (wc >> 30) & 0x03);
            nextra = 5;
        }
#endif /* WCHAR_MAX > 0xffff */

        for (; 0 < nextra; )
            *su++ = (unsigned char)(0x80 | (wc >> 6 * --nextra) & 0x3f);

        return (char *)su - s;
#endif /* WCHAR_MAX <= 0xff */
    }
    else
    {
        /* run finite state machine */
        char state = pst->state;
        int leave = 0;
        int limit = 0;
        int nout = 0;

        if (s == 0)
        {
            /* set initial state */
            *pst = initial;
            return __mbtowctab.tab[0][0] & _ST_STATE;
        }

        for (;;)
        {
            /* perform a state transformation */
            unsigned short code;
            const unsigned short *stab;

            if (state >= _NSTATE || (stab = __wctombtab.tab[state]) == 0 || nout >= MB_CUR_MAX || (_NSTATE*UCHAR_MAX) <= ++limit || (code = stab[wc & UCHAR_MAX]) == 0)
            {
                /* report invalid sequence */
                errno = EILSEQ;
                return -1;
            }

            state = (code & _ST_STATE) >> _ST_STOFF;

            if (code & _ST_FOLD)
                wc = wc & ~UCHAR_MAX | code & _ST_CH;

            if (code & _ST_ROTATE)
                wc = wc << CHAR_BIT | UCHAR_MAX & wc >> CHAR_BIT * (sizeof(wchar_t) - 1);

            if (code & _ST_OUTPUT)
            {
                /* produce an output char */
                if ((s[nout++] = code & _ST_CH ? code : wc) == '\0')
                    leave = 1;
                limit = 0;
            }

            if (code & _ST_INPUT || leave)
            {
                /* consume input */
                pst->state = state;
                return nout;
            }
        }
    }
}

