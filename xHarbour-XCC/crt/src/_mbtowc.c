/****************************************************************************
 *                                                                          *
 * File    : _mbtowc.c                                                      *
 *                                                                          *
 * Purpose : __mbtowc function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-09  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <errno.h>
#include <limits.h>
#include <stdlib.h>
#include "xstate.h"
#include "xwchar.h"

/* translate multibyte to widechar */
int __mbtowc(wchar_t *pwc, const char *s, size_t nin, mbstate_t *pst)
{
    char state = pst->state;
    unsigned char *su = (unsigned char *)s;
    wchar_t wc = pst->wchar;
    static const mbstate_t initial = {0};

    if (__mbtowctab.tab[0] == 0)
    {
        /* no table, convert from UTF8 */
        if (s == 0)
        {
            /* set initial state */
            *pst = initial;
            return 0;
        }

#if WCHAR_MAX <= 0xff
        if (nin == 0)
        {
            return -2;
        }
        else
        {
            /* return a single byte */
            if (pwc != 0) *pwc = *su;
            return (*su == 0) ? 0 : 1;
        }
#else /* WCHAR_MAX <= 0xff */
        for (;; ++su, --nin)
        {
            /* consume an input byte */
            if (nin == 0)
            {
                /* report incomplete conversion */
                pst->wchar = wc;
                pst->state = state;
                return -2;
            }
            else if (state > 0)
            {
                /* fold in a successor byte */
                if ((*su & 0xc0) != 0x80)
                {
                    /* report invalid sequence */
                    errno = EILSEQ;
                    return -1;
                }

                wc = (wc << 6) | (*su & 0x3f);
                --state;
            }
            else if ((*su & 0x80) == 0)
            {
                wc = *su;  /* consume a single byte */
            }
            else if ((*su & 0xe0) == 0xc0)
            {
                /* consume first of two bytes */
                wc = *su & 0x1f;
                state = 1;
            }
            else if ((*su & 0xf0) == 0xe0)
            {
                /* consume first of three bytes */
                wc = *su & 0x0f;
                state = 2;
            }
#if WCHAR_MAX > 0xffff
            else if ((*su & 0xf8) == 0xf0)
            {
                /* consume first of four bytes */
                wc = *su & 0x07;
                state = 3;
            }
            else if ((*su & 0xfc) == 0xf8)
            {
                /* consume first of five bytes */
                wc = *su & 0x03;
                state = 4;
            }
            else if ((*su & 0xfc) == 0xfc)
            {
                /* consume first of six bytes */
                wc = *su & 0x03;
                state = 5;
            }
#endif /* WCHAR_MAX > 0xffff */
            else
            {
                /* report invalid sequence */
                errno = EILSEQ;
                return -1;
            }

            if (state == 0)
            {
                /* produce an output wchar */
                if (pwc != 0) *pwc = wc;
                pst->state = 0;
                return (wc == 0) ? 0 : (const char *)++su - s;
            }
        }
#endif /* WCHAR_MAX <= 0xff */
    }
    else
    {
        /* run finite state machine */
        int limit = 0;

        if (s == 0)
        {
            /* set initial state */
            *pst = initial;
            return (__mbtowctab.tab[0][0] & _ST_STATE);
        }

        for (;;)
        {
            /* perform a state transformation */
            unsigned short code;
            const unsigned short *stab;

            if (nin == 0)
            {
                /* report incomplete conversion */
                pst->wchar = wc;
                pst->state = state;
                return -2;
            }
            else if (state >= _NSTATE || (stab = __mbtowctab.tab[state]) == 0 || (_NSTATE*UCHAR_MAX) <= ++limit || (code = stab[*su]) == 0)
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

            if (code & _ST_INPUT && *su != '\0')
                ++su, --nin, limit = 0;

            if (code & _ST_OUTPUT)
            {
                /* produce an output wchar */
                if (pwc != 0) *pwc = wc;
                pst->wchar = wc;
                pst->state = state;
                return (wc == 0) ? 0 : (const char *)su - s;
            }
        }
    }
}

