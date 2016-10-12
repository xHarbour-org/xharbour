/****************************************************************************
 *                                                                          *
 * File    : _wscanf.c                                                      *
 *                                                                          *
 * Purpose : __wscanf function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-10  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <string.h>
#include "xwstdio.h"

/* read formatted */
int __wscanf(wint_t (*pfn)(void *, wint_t, int), void *arg, const wchar_t *fmt, va_list ap)
{
    int nconv = 0;
    __wscninfo x;

    x.pfn = pfn;
    x.arg = arg;
    memcpy(&x.ap, &ap, sizeof(va_list));
    x.nchar = 0;

    for (x.s = fmt; ; ++x.s)
    {
        /* parse format string */
        wint_t ch;

        for (; *x.s != L'%'; ++x.s)
        {
            if (*x.s == L'\0')
                return nconv;
            else if (iswspace(*x.s))
            {
                /* match any white-space */
                while (iswspace(ch = WGET(&x)))
                    ;
                WUNGETN(&x, ch);
            }
            else if (*x.s != (ch = WGET(&x)))
            {
                /* bad literal match */
                WUNGETN(&x, ch);
                return nconv;
            }
        }

        /* process a conversion specifier */
        {
            int code;
            static const wchar_t qchar[] = { L'h', L'j', L'l', L't', L'z', L'L', L'\0' };
            static const wchar_t schar[] = { L'c', L'C', L'n', L'[', L'\0' };

            x.noconv = (*++x.s == L'*') ? *x.s++ : L'\0';
            for (x.width = 0; iswdigit(*x.s); ++x.s)
            {
                if (x.width < _WMAX)
                    x.width = x.width * 10 + *x.s - L'0';
            }

            x.qual = wcschr(&qchar[0], *x.s) ? *x.s++ : L'\0';

            if (x.qual == L'h' && *x.s == L'h')
                x.qual = L'b', ++x.s;
            else if (x.qual == L'l' && *x.s == L'l')
                x.qual = L'q', ++x.s;

            if (!wcschr(&schar[0], *x.s))
            {
                /* match leading white-space */
                while (iswspace(ch = WGET(&x)))
                    ;
                WUNGETN(&x, ch);
            }

            if ((code = __wgetfield(&x)) <= 0)
                return (nconv == 0) ? code : nconv;

            if (x.stored)
                ++nconv;
        }
    }
}

