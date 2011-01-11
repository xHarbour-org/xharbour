/****************************************************************************
 *                                                                          *
 * File    : _litob.c                                                       *
 *                                                                          *
 * Purpose : __litob function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-10  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>
#include <string.h>
#include "xmath.h"
#include "xstdio.h"

static const char ldigs[] = "0123456789abcdef";
static const char udigs[] = "0123456789ABCDEF";

/* convert unsigned long to text */
void __litob(__prtinfo *px, char code)
{
    char ac[24];    /* safe for 64-bit integers */
    const char *digs = (code == 'X') ? udigs : ldigs;
    int base = (code == 'o') ? 8 : (code != 'x' && code != 'X') ? 10 : 16;
    int i = sizeof(ac);
    unsigned long long ulval = px->v.li;

    if ((code == 'd' || code == 'i') && px->v.li < 0)
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

    if (base == 8 && px->flags & _FNO && i < sizeof(ac) && ac[i] != '0')
        ac[--i] = '0';
    px->n1 = sizeof(ac) - i;
    memcpy(px->s, &ac[i], px->n1);

    if (px->n1 < px->prec)
        px->nz0 = px->prec - px->n1, px->flags &= ~_FZE;
    else if (px->prec < 0 && (px->flags & (_FMI|_FZE)) == _FZE && (i = px->width - px->n0 - px->nz0 - px->n1) > 0)
        px->nz0 = i;
}

