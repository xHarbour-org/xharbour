/****************************************************************************
 *                                                                          *
 * File    : _genld.c                                                       *
 *                                                                          *
 * Purpose : __genld function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-02  Modified for C99.                                    *
 *                                                                          *
 ****************************************************************************/

#include <locale.h>
#include <stdlib.h>
#include <string.h>
#include "xstdio.h"

/* generate long double text */
void __genld(__prtinfo *px, char code, char *p, short nsig, short xexp)
{
    const char point = localeconv()->decimal_point[0];

    if (nsig <= 0)
        nsig = 1, p = "0";

    if (code == 'f' || code == 'F' || (code == 'g' || code == 'G') && xexp >= -4 && xexp < px->prec)
    {
        /* 'f' format */
        ++xexp;  /* change to leading digit count */
        if (code == 'g' || code == 'G')
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
            px->s[px->n1++] = '0';
            if (px->prec > 0 || px->flags & _FNO)
                px->s[px->n1++] = point;
            if (px->prec < -xexp)
                xexp = -px->prec;
            px->nz1 = -xexp;
            px->prec += xexp;
            if (px->prec < nsig)
                nsig = px->prec;
            memcpy(&px->s[px->n1], p, px->n2 = nsig);
            px->nz2 = px->prec - nsig;
        }
        else if (nsig < xexp)
        {
            /* zeros before point */
            memcpy(&px->s[px->n1], p, nsig);
            px->n1 += nsig;
            px->nz1 = xexp - nsig;
            if (px->prec > 0 || px->flags & _FNO)
                px->s[px->n1] = point, ++px->n2;
            px->nz2 = px->prec;
        }
        else
        {
            /* enough digits before point */
            memcpy(&px->s[px->n1], p, xexp);
            px->n1 += xexp;
            nsig -= xexp;
            if (px->prec > 0 || px->flags & _FNO)
                px->s[px->n1++] = point;
            if (px->prec < nsig)
                nsig = px->prec;
            memcpy(&px->s[px->n1], p + xexp, nsig);
            px->n1 += nsig;
            px->nz1 = px->prec - nsig;
        }
    }
    else
    {
        /* 'a' or 'e' format */
        if (code == 'g' || code == 'G')
        {
            /* fixup for 'g' */
            if (nsig < px->prec && !(px->flags & _FNO))
                px->prec = nsig;
            if (--px->prec < 0)
                px->prec = 0;
            code = (code == 'g') ? 'e' : 'E';
        }
        else if (code == 'a')
            code = 'p';
        else if (code == 'A')
            code = 'P';

        px->s[px->n1++] = *p++;
        if (px->prec > 0 || px->flags & _FNO)
            px->s[px->n1++] = point;
        if (px->prec > 0)
        {
            /* put fraction digits */
            if (px->prec < --nsig)
                nsig = px->prec;
            memcpy(&px->s[px->n1], p, nsig);
            px->n1 += nsig;
            px->nz1 = px->prec - nsig;
        }

        p = &px->s[px->n1];  /* put exponent */
        *p++ = code;
        if (xexp >= 0)
        {
            *p++ = '+';
        }
        else
        {
            /* negative exponent */
            *p++ = '-';
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
                *p++ = '0';  /* at least two digits after 'e' or 'E' */
            if (n == 0)
                *p++ = '0';  /* at least one digit */

            while (n > 0)
                *p++ = buf[--n] + '0';  /* deliver significant digits */
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

