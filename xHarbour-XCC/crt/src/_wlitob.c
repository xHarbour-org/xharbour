/****************************************************************************
 *                                                                          *
 * File    : _wlitob.c                                                      *
 *                                                                          *
 * Purpose : __wlitob function [new C99].                                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-10  Rewritten.                                           *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>
#include <wchar.h>
#include "xmath.h"
#include "xwstdio.h"

static const wchar_t ldigs[] = {
    L'0', L'1', L'2', L'3',
    L'4', L'5', L'6', L'7',
    L'8', L'9', L'a', L'b',
    L'c', L'd', L'e', L'f'
};
static const wchar_t udigs[] = {
    L'0', L'1', L'2', L'3',
    L'4', L'5', L'6', L'7',
    L'8', L'9', L'A', L'B',
    L'C', L'D', L'E', L'F'
};

/* convert unsigned long to wide text */
void __wlitob(__wprtinfo *px, wchar_t code)
{
    wchar_t ac[24];  /* safe for 64-bit integers */
    const wchar_t *digs = (code == L'X') ? udigs : ldigs;
    int base = (code == L'o') ? 8 : (code != L'x' && code != L'X') ? 10 : 16;
    int i = sizeof(ac) / sizeof(wchar_t);
    unsigned long long ulval = px->v.li;

    if ((code == L'd' || code == L'i') && px->v.li < 0)
        ulval = -ulval;  /* safe against overflow */

    if (ulval != 0 || px->prec != 0)
        ac[--i] = digs[ulval % base];

    px->v.li = (long long)(ulval / (unsigned long long)base);

    while (px->v.li > 0 && i > 0)
    {
        /* convert digits */
        long long quot = px->v.li / (long long)base;

        ac[--i] = digs[(int)(px->v.li - quot * (long long)base)];
        px->v.li = quot;
    }

    if (base == 8 && px->flags & _FNO && i < sizeof(ac) / sizeof(wchar_t) && ac[i] != L'0')
        ac[--i] = L'0';

    px->n1 = sizeof(ac) / sizeof(wchar_t) - i;
    wmemcpy(px->s, &ac[i], px->n1);

    if (px->n1 < px->prec)
        px->nz0 = px->prec - px->n1, px->flags &= ~_FZE;
    else if (px->prec < 0 && (px->flags & (_FMI|_FZE)) == _FZE && (i = px->width - px->n0 - px->nz0 - px->n1) > 0)
        px->nz0 = i;
}

