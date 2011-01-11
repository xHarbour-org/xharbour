/****************************************************************************
 *                                                                          *
 * File    : _wcsxfrm.c                                                     *
 *                                                                          *
 * Purpose : __wcsxfrm function.                                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-10  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <limits.h>
#include "xwcsxfrm.h"

/* translate wchar_t string to collatable form */
size_t __wcsxfrm(wchar_t *sout, const wchar_t **psin, size_t size, mbstate_t *pst)
{
    const wchar_t *sin = *psin;
    int nout = 0;

    if (__wcollatetab.tab[0] == 0)
    {
        /* no table, convert 1-to-1 */
        for (; nout < size; ++sin, ++sout)
        {
            /* count and deliver a char */
            ++nout;
            if ((*sout = *sin) == L'\0')
                break;
        }

        *psin = sin;  /* continue where we left off */
        return nout;
    }
    else
    {
        /* run finite state machine */
        unsigned char state = pst->state;
        int leave = 0;
        int limit = 0;
        unsigned short wc = pst->wchar ? pst->wchar : *sin;

        for (;;)
        {
            /* perform a state transformation */
            unsigned short code;
            const unsigned short *stab;

            if (state >= _NSTATE || (stab = __wcollatetab.tab[state]) == 0 || size <= nout || (_NSTATE*UCHAR_MAX) <= ++limit || (code = stab[wc & UCHAR_MAX]) == 0)
                break;

            state = (code & _ST_STATE) >> _ST_STOFF;

            if (code & _ST_FOLD)
                wc = wc & ~WCHAR_MAX | code & _ST_CH;

            if (code & _ST_ROTATE)
                wc = wc << CHAR_BIT | UCHAR_MAX & wc >> CHAR_BIT * (sizeof(wchar_t) - 1);

            if (code & _ST_OUTPUT && ((sout[nout++] = wc) == L'\0' || size <= nout))
                leave = 1;

            if (code & _ST_INPUT)
            {
                if (*sin != L'\0')
                    wc = *++sin, limit = 0;
                else
                    wc = L'\0', leave = 1;
            }

            if (leave)
            {
                /* return for now */
                *psin = sin;
                pst->state = state;
                pst->wchar = wc;
                return nout;
            }
        }

        sout[nout++] = L'\0';  /* error return */
        *psin = sin;
        pst->state = _NSTATE;
        return nout;
    }
}

