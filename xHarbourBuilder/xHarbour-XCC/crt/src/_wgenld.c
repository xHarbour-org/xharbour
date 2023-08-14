/****************************************************************************
 *                                                                          *
 * File    : _wgenld.c                                                      *
 *                                                                          *
 * Purpose : __wgenld function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-09  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <locale.h>
#include <stdlib.h>
#include "xwstdio.h"

/* generate long double wide text */
void __wgenld(__wprtinfo *px, wchar_t code, wchar_t *p, short nsig, short xexp)
{
    wint_t wpoint = __btowc(localeconv()->decimal_point[0]);
    static const wchar_t lzero[1] = {L'0'};

    if (wpoint == WEOF)
        wpoint = L'.';

    if (nsig <= 0)
        nsig = 1, p = (wchar_t *)lzero;

    if (code == L'f' || code == L'F' || (code == L'g' || code == L'G') && xexp >= -4 && xexp < px->prec)
    {
        /* 'f' format */
        ++xexp; /* change to leading digit count */
        if (code != L'f' && code != L'F')
        {
            /* fixup for 'g' */
            if (!(px->flags & _FNO) && nsig < px->prec)
                    px->prec = nsig;
            if ((px->prec -= xexp) < 0)
                    px->prec = 0;
        }

        if (xexp <= 0)
        {
            /* digits only to right of point */
            px->s[px->n1++] = L'0';

            if (0 < px->prec || px->flags & _FNO)
                px->s[px->n1++] = wpoint;

            if (px->prec < -xexp)
                xexp = -px->prec;

            px->nz1 = -xexp;
            px->prec += xexp;
            if (px->prec < nsig)
                nsig = px->prec;

            wmemcpy(&px->s[px->n1], p, px->n2 = nsig);
            px->nz2 = px->prec - nsig;
        }
        else if (nsig < xexp)
        {
            /* zeros before point */
            wmemcpy(&px->s[px->n1], p, nsig);
            px->n1 += nsig, px->nz1 = xexp - nsig;
            if (px->prec > 0 || px->flags & _FNO)
                px->s[px->n1] = wpoint, ++px->n2;
            px->nz2 = px->prec;
        }
        else
        {
            /* enough digits before point */
            wmemcpy(&px->s[px->n1], p, xexp);
            px->n1 += xexp;
            nsig -= xexp;

            if (px->prec > 0 || px->flags & _FNO)
                px->s[px->n1++] = wpoint;

            if (px->prec < nsig)
                nsig = px->prec;

            wmemcpy(&px->s[px->n1], p + xexp, nsig);
            px->n1 += nsig, px->nz1 = px->prec - nsig;
        }
    }
    else
    {
        /* 'a' or 'e' format */
        if (code == L'g' || code == L'G')
        {
            /* fixup for 'g' */
            if (nsig < px->prec && !(px->flags & _FNO))
                px->prec = nsig;

            if (--px->prec < 0)
                px->prec = 0;

            code = code == (L'g') ? L'e' : L'E';
        }
        else if (code == L'a')
            code = L'p';
        else if (code == L'A')
            code = L'P';

        px->s[px->n1++] = *p++;

        if (px->prec > 0 || px->flags & _FNO)
            px->s[px->n1++] = wpoint;

        if (px->prec > 0)
        {
            /* put fraction digits */
            if (px->prec < --nsig)
                nsig = px->prec;

            wmemcpy(&px->s[px->n1], p, nsig);
            px->n1 += nsig, px->nz1 = px->prec - nsig;
        }

        p = &px->s[px->n1];  /* put exponent */
        *p++ = code;
        if (xexp >= 0)
        {
            *p++ = L'+';
        }
        else
        {
            /* negative exponent */
            *p++ = L'-';
            xexp = -xexp;
        }

        /* put decimal exponent */
        {
            char buf[10];
            int n;

            for (n = 0; xexp > 0; ++n)
            {
                /* pick off least-significant digit */
                div_t qr = div(xexp, 10);
                buf[n] = qr.rem;
                xexp = qr.quot;
            }

            if (n < 2 && (code == 'e' || code == 'E'))
                *p++ = L'0';  /* at least two digits after 'e' or 'E' */

            if (n == 0)
                *p++ = L'0';  /* at least one digit */

            for (; n > 0;)
                *p++ = buf[--n] + L'0'; /* deliver significant digits */
        }

        px->n2 = p - &px->s[px->n1];
    }

    if ((px->flags & (_FMI|_FZE)) == _FZE)
    {
        /* pad with leading zeros */
        int n = px->n0 + px->n1 + px->nz1 + px->n2 + px->nz2;

        if (n < px->width)
            px->nz0 = px->width - n;
    }
}

