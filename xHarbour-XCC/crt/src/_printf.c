/****************************************************************************
 *                                                                          *
 * File    : _printf.c                                                      *
 *                                                                          *
 * Purpose : __printf function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-06  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <ctype.h>
#include <string.h>
#include "xstdio.h"
#include "xwchar.h"

/* print formatted */
int __printf(void *(*pfn)(void *, const char *, size_t), void *arg, const char *fmt, va_list ap)
{
    mbstate_t mbst = {0};
    __prtinfo x;

    x.pfn = pfn, x.arg = arg, x.nchar = 0;
    for (;;)
    {
        /* scan format string */
        for (;;)
        {
            /* copy any literal text */
            int m, n;
            wchar_t wc = L'\0';

            if ((n = __mbtowc(&wc, fmt, INT_MAX, &mbst)) <= 0)
                n = (*fmt == '\0') ? 0 : 1;  /* bad parse, eat one char */

            if ((m = (wc == L'%') ? n - 1 : n) <= 0)
                ;
            else if ((x.arg = (*x.pfn)(x.arg, fmt, m)) == 0)
                return EOF;
            else
                x.nchar += m;

            fmt += n;
            if (wc == L'%')
                break;
            else if (wc == L'\0')
                return x.nchar;
        }

        /* process a conversion specifier */
        {
            const char *t;
            static const char fchar[] = " +-#0";
            static const unsigned int fbit[] = { _FSP, _FPL, _FMI, _FNO, _FZE, 0 };

            x.n0 = x.nz0 = x.n1 = x.nz1 = x.n2 = x.nz2 = 0;

            for (x.flags = 0; (t = strchr(&fchar[0], *fmt)) != 0; ++fmt)
                x.flags |= fbit[t - &fchar[0]];

            if (*fmt == '*')
            {
                /* get width argument */
                x.width = va_arg(ap, int);
                if (x.width < 0)
                {
                    /* same as '-' flag */
                    x.width = -x.width;
                    x.flags |= _FMI;
                }
                ++fmt;
            }
            else
            {
                /* accumulate width digits */
                for (x.width = 0; isdigit(*fmt); ++fmt)
                {
                    if (x.width < _WMAX)
                        x.width = x.width * 10 + *fmt - '0';
                }
            }

            if (*fmt != '.')
            {
                x.prec = -1;
            }
            else if (*++fmt == '*')
            {
                /* get precision argument */
                x.prec = va_arg(ap, int);
                ++fmt;
            }
            else
            {
                /* accumulate precision digits */
                for (x.prec = 0; isdigit(*fmt); ++fmt)
                {
                    if (x.prec < _WMAX)
                        x.prec = x.prec * 10 + *fmt - '0';
                }
            }

            x.qual = strchr("hjltzL", *fmt) ? *fmt++ : '\0';  /* was hlL */
            if (x.qual == 'h' && *fmt == 'h')
                x.qual = 'b', ++fmt;
            else if (x.qual == 'l' && *fmt == 'l')
                x.qual = 'q', ++fmt;
        }

        /* do the conversion */
        {
            char ac[_MAX_SIG_DIG + _MAX_EXP_DIG + 16];

            if (__putfield(&x, &ap, *fmt++, ac) || __puttxt(&x, ac) < 0)
                return EOF;
        }
    }
}

