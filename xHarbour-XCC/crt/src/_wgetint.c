/****************************************************************************
 *                                                                          *
 * File    : _wgetint.c                                                     *
 *                                                                          *
 * Purpose : __wgetint function.                                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-10  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include "xwstdio.h"

#define ACSIZE  32  /* holds only prefix, m.s. digits */

/* get an integer value for __wscanf */
int __wgetint(__wscninfo *px)
{
    wchar_t ac[ACSIZE], *p;
    char seen = 0;
    wint_t ch;
    static const wchar_t digits[] = {
        L'0', L'1', L'2', L'3', L'4', L'5',
        L'6', L'7', L'8', L'9', L'a', L'b',
        L'c', L'd', L'e', L'f', L'A', L'B',
        L'C', L'D', L'E', L'F'
    };
    static const wchar_t flit[] = {
        L'd', L'i', L'o', L'u', L'x', L'X',
        L'p', L'\0'
    };
    static const char barr[] = { 10, 0, 8, 10, 16, 16, 16 };
    int base = barr[(const wchar_t *)wcschr(&flit[0], *px->s) - flit];
    int dlen;

    px->nget = (px->width > 0) ? px->width : INT_MAX;
    p = ac, ch = WGETN(px);

    if (ch == L'+' || ch == L'-')
        *p++ = ch, ch = WGETN(px);

    if (ch == L'0')
    {
        /* match possible prefix */
        seen = 1;

        *p++ = ch, ch = WGETN(px);
        if ((ch == L'x' || ch == L'X') && (base == 0 || base == 16))
            base = 16, *p++ = ch, ch = WGETN(px), seen = 0;
        else if (base == 0)
            base = 8;
    }

    dlen = (base == 0 || base == 10) ? 10 : (base == 8) ? 8 : 16 + 6;

    for (; ch == L'0'; seen = 1)
        ch = WGETN(px);

    if (seen)
        *p++ = L'0';

    for (; ch != WEOF && wmemchr(&digits[0], ch, dlen); ch = WGETN(px), seen = 1)
        if (p < &ac[ACSIZE - 1]) *p++ = ch;
    WUNGETN(px, ch);

    if (!seen)
        return (p == ac && ch == WEOF) ? EOF : 0;

    *p = L'\0';
    if (px->noconv)
        ;
    else if (*px->s == L'd' || *px->s == L'i')
    {
        /* deliver a signed integer */
        const long long lval = __wcstoll(ac, 0, base);

        px->stored = 1;
        switch (px->qual)
        {
            /* store in specified integer type */
            case L'b':
                *va_arg(px->ap, signed char *) = lval;
                break;

            case L'q':
                *va_arg(px->ap, long long *) = lval;
                break;

            case L'j':
                *va_arg(px->ap, intmax_t *) = lval;
                break;

            case L't':
                *va_arg(px->ap, ptrdiff_t *) = lval;
                break;

            case L'z':
                *va_arg(px->ap, ptrdiff_t *) = lval;
                break;

            case L'h':
                *va_arg(px->ap, short *) = lval;
                break;

            case L'l':
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
        const unsigned long long ulval = __wcstoull(ac, 0, base);

        px->stored = 1;
        if (*px->s == L'p')
            *va_arg(px->ap, void **) = (void *)((char *)0 + ulval);
        else switch (px->qual)
        {
            /* store in specified integer type */
            case L'b':
                *va_arg(px->ap, unsigned char *) = ulval;
                break;

            case L'q':
                *va_arg(px->ap, unsigned long long *) = ulval;
                break;

            case L'j':
                *va_arg(px->ap, uintmax_t *) = ulval;
                break;

            case L't':
                *va_arg(px->ap, size_t *) = ulval;
                break;

            case L'z':
                *va_arg(px->ap, size_t *) = ulval;
                break;

            case L'h':
                *va_arg(px->ap, unsigned short *) = ulval;
                break;

            case L'l':
                *va_arg(px->ap, unsigned long *) = ulval;
                break;

            default:
                *va_arg(px->ap, unsigned int *) = ulval;
                break;
        }
    }

    return 1;
}

