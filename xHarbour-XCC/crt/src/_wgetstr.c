/****************************************************************************
 *                                                                          *
 * File    : _wgetstr.c                                                     *
 *                                                                          *
 * Purpose : __wgetstr function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-10  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <string.h>
#include "xwstdio.h"

/* convert %[] (sfl < 0), %c (sfl == 0), else %s */
int __wgetstr(__wscninfo *px, int sfl)
{
    char buf[MB_LEN_MAX], *s;
    char seen = 0;
    int n, nset;
    int wfl = px->qual == L'l';
    wchar_t comp, *p;
    const wchar_t *t;
    wint_t ch;
    mbstate_t mbst = {0};

    if (sfl < 0)
    {
        /* parse [] in format */
        comp = (*++px->s == L'^') ? *px->s++ : L'\0';
        t = wcschr(*px->s == L']' ? px->s + 1 : px->s, L']');
        if (t == 0) return 0;

        nset = t - px->s;
    }

    px->nget = (px->width > 0) ? px->width : (sfl != 0) ? INT_MAX : 1;
    if (px->noconv)
        ;
    else if (wfl)
        p = va_arg(px->ap, wchar_t *);
    else
        s = va_arg(px->ap, char *);

    for (; (ch = WGETN(px)) != WEOF; seen = 1)
    {
        if (sfl > 0 && iswspace(ch) || sfl < 0 && (!comp && !wmemchr(px->s, ch, nset) || comp && wmemchr(px->s, ch, nset)))
            break;
        else if (px->noconv)
            ;
        else if (wfl)
            *p++ = ch, px->stored = 1;
        else if ((n = __wctomb(s, ch, &mbst)) < 0)
            return 0;
        else if (n > 0)
            s += n, px->stored = 1;
    }

    if (wfl)
        ;
    else if ((n = __wctomb(buf, L'\0', &mbst)) <= 0)
        return 0;
    else
        memcpy(s, buf, n - 1);
    WUNGETN(px, ch);

    if (sfl == 0)
        return (seen) ? 1 : EOF;

    if (!seen)
        return (ch == WEOF) ? EOF : 0;

    if (px->noconv)
        ;
    else if (wfl)
        *p = L'\0';
    else
        *s = '\0';

    if (sfl < 0)
        px->s = t;

    return 1;
}

