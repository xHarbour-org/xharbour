/****************************************************************************
 *                                                                          *
 * File    : _getstr.c                                                      *
 *                                                                          *
 * Purpose : __getstr function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-06  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <ctype.h>
#include <string.h>
#include "xwchar.h"
#include "xstdio.h"

/* look for c in [] with ranges */
static int rangematch(const char *s, int c, size_t n)
{
    const unsigned char uc = c;
    const unsigned char *su = (const unsigned char *)s;

    while (n >= 3)
    {
        if (su[1] == '-')
        {
            if (su[0] <= uc && uc <= su[2])
                return 1;
            else
                su += 3, n -= 3;
        }
        else if (su[0] == uc)
        {
            return 1;
        }
        else
        {
            ++su, --n;
        }
    }

    for (; n > 0; ++su, --n)
        if (su[0] == uc) return 1;

    return 0;
}

/* convert %[] (sfl < 0), %c (sfl == 0), else %s */
int __getstr(__scninfo *px, int sfl)
{
    char comp, *s;
    char range = 0;
    char seen = 0;
    const char *t;
    int ch, nset;
    int wfl = px->qual == 'l';
    wchar_t *p;
    mbstate_t mbst = {0};

    if (sfl < 0)
    {
        /* parse [] in format */
        comp = (*++px->s == '^') ? *px->s++ : '\0';
        t = strchr(*px->s == ']' ? px->s + 1 : px->s, ']');
        if (t == 0) return 0;

        nset = t - px->s;
        if (nset >= 3 && memchr(px->s + 1, '-', nset - 2))
            range = 1;
    }

    px->nget = (px->width > 0) ? px->width : (sfl != 0) ? INT_MAX : 1;
    if (px->noconv)
            ;
    else if (wfl)
        p = va_arg(px->ap, wchar_t *);
    else
        s = va_arg(px->ap, char *);

    while ((ch = GETN(px)) != EOF)
    {
        if (sfl > 0 && isspace(ch) || sfl < 0 && (!range && !comp && !memchr(px->s, ch, nset)
            || !range && comp && memchr(px->s, ch, nset)
            || !comp && !rangematch(px->s, ch, nset)
            || comp && rangematch(px->s, ch, nset)))
            break;
        else if (!wfl)
        {
            /* deliver a single byte */
            seen = 2;
            if (!px->noconv)
                *s++ = ch, px->stored = 1;
        }
        else
        {
            /* build a wchar_t */
            char buf[MB_LEN_MAX];
            int n, nc;
            wchar_t wc;

            buf[0] = ch;
            for (nc = 1;;)
            {
                switch (n = __mbtowc(&wc, buf, nc, &mbst))
                {
                    /* try to build a wchar_t */
                    case -2:
                        seen |= 1;  /* partial wchar_t */
                        if (++nc <= sizeof(buf) && (ch = GETN(px)) != EOF)
                            break;  /* try again with more chars */

                    case -1:
                        for (; nc > 0; )
                            UNGETN(px, buf[--nc]);  /* stop on conversion error */
                        return (seen) ? 0 : EOF;

                    case 0:
                        if (wc == L'\0')  /* may be null wide char */
                            n = strlen(buf) + 1;

                    default:  /* fall through */
                        for (; n < nc;)  /* got a wchar_t, count and deliver */
                            UNGETN(px, buf[--nc]);  /* return unused chars */
                        if (!px->noconv)
                            *p++ = wc, px->stored = 1;
                        --px->width;
                        seen = 2;  /* whole wchar_t */
                }
            }
        }
    }
    UNGETN(px, ch);

    if (sfl == 0 || seen != 2)
        return (seen & 1) ? 0 : (seen & 2) ? 1 : EOF;

    if (!seen)
        return (ch == EOF) ? EOF : 0;

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

