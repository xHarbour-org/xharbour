/****************************************************************************
 *                                                                          *
 * File    : _getint.c                                                      *
 *                                                                          *
 * Purpose : __getint function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-06  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>
#include <string.h>
#include "xstdio.h"

#define ACSIZE  32  /* holds only prefix, m.s. digits */

/* get an integer value for __scanf */
int __getint(__scninfo *px)
{
    char ac[ACSIZE], *p, seen;
    int base, ch, dlen;
    static const char digits[] = "0123456789abcdefABCDEF";
    static const char flit[] = "diouxXp";
    static const char barr[] = {10, 0, 8, 10, 16, 16, 16};

    px->nget = (px->width > 0) ? px->width : INT_MAX;
    p = ac, ch = GETN(px);

    if (ch == '+' || ch == '-')
        *p++ = ch, ch = GETN(px);

    seen = 0;
    base = barr[(const char *)strchr(&flit[0], *px->s) - flit];
    if (ch == '0')
    {
        /* match possible prefix and strip it */
        seen = 1;
        ch = GETN(px);
        if ((ch == 'x' || ch == 'X') && (base == 0 || base == 16))
            base = 16, ch = GETN(px), seen = 0;
        else if (base == 0)
            base = 8;
    }

    dlen = (base == 0 || base == 10) ? 10 : (base == 8) ? 8 : 16 + 6;

    for (; ch == '0'; seen = 1)
        ch = GETN(px);

    if (seen)
        *p++ = '0';

    for (; ch != EOF && memchr(&digits[0], ch, dlen); ch = GETN(px), seen = 1)
        if (p < &ac[ACSIZE - 1]) *p++ = ch;
    UNGETN(px, ch);

    if (!seen)
        return (p == ac && ch == EOF) ? EOF : 0;

    *p = '\0';
    if (px->noconv)
        ;
    else if (*px->s == 'd' || *px->s == 'i')
    {
        /* deliver a signed integer */
        const long long lval = __stoll(ac, 0, base);

        px->stored = 1;
        switch (px->qual)
        {
            /* store in specified integer type */
            case 'b':
                *va_arg(px->ap, signed char *) = lval;
                break;

            case 'q':
                *va_arg(px->ap, long long *) = lval;
                break;

            case 'j':
                *va_arg(px->ap, intmax_t *) = lval;
                break;

            case 't':
                *va_arg(px->ap, ptrdiff_t *) = lval;
                break;

            case 'z':
                *va_arg(px->ap, ptrdiff_t *) = lval;
                break;

            case 'h':
                *va_arg(px->ap, short *) = lval;
                break;

            case 'l':
                *va_arg(px->ap, long *) = lval;
                break;

            default:
                *va_arg(px->ap, int *) = lval;
                break;
        }
    }
    else
    {
        /* deliver an unsigned integer */
        const unsigned long long ulval = __stoull(ac, 0, base);

        px->stored = 1;
        if (*px->s == 'p')
            *va_arg(px->ap, void **) = (void *)((char *)0 + ulval);
        else switch (px->qual)
        {
            /* store in specified integer type */
            case 'b':
                *va_arg(px->ap, unsigned char *) = ulval;
                break;

            case 'q':
                *va_arg(px->ap, unsigned long long *) = ulval;
                break;

            case 'j':
                *va_arg(px->ap, uintmax_t *) = ulval;
                break;

            case 't':
                *va_arg(px->ap, size_t *) = ulval;
                break;

            case 'z':
                *va_arg(px->ap, size_t *) = ulval;
                break;

            case 'h':
                *va_arg(px->ap, unsigned short *) = ulval;
                break;

            case 'l':
                *va_arg(px->ap, unsigned long *) = ulval;
                break;

            default:
                *va_arg(px->ap, unsigned int *) = ulval;
                break;
        }
    }

    return 1;
}

