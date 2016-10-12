/****************************************************************************
 *                                                                          *
 * File    : _scanf.c                                                       *
 *                                                                          *
 * Purpose : __scanf function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-09  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include "xstdio.h"
#include "xwchar.h"

/* read formatted */
int __scanf(int (*pfn)(void *, int, int), void *arg, const char *fmt, va_list ap)
{
    mbstate_t mbst = {0};
    int nconv = 0;
    __scninfo x;

    x.pfn = pfn;
    x.arg = arg;
    memcpy(&x.ap, &ap, sizeof(va_list));
    x.nchar = 0;

    for (x.s = fmt;; ++x.s)
    {
        /* parse format string */
        int ch;

        for (;;)
        {
            /* match any literal text or white-space */
            int n;
            wchar_t wc = L'\0';

            if ((n = __mbtowc(&wc, x.s, INT_MAX, &mbst)) <= 0)
                n = (*x.s == '\0') ? 0 : 1;  /* bad parse, eat one char */

            if (isspace(__wctob(wc)))
            {
                /* match any white-space */
                while (isspace(ch = GET(&x)))
                    ;
                UNGETN(&x, ch);
            }
            else
            {
                /* match any literal text */
                int m = (wc == L'%') ? n - 1 : n;
                const char *s;

                for (s = x.s; 0 <= --m; ++s)
                {
                    if ((ch = GET(&x)) != *s)
                    {
                        /* bad match */
                        UNGETN(&x, ch);
                        return nconv;
                    }
                }
            }

            x.s += n;
            if (wc == L'%')
                break;
            else if (wc == L'\0')
                return nconv;
        }

        /* process a conversion specifier */
        {
            int code;

            x.noconv = (*x.s == '*') ? *x.s++ : '\0';
            for (x.width = 0; isdigit(*x.s); ++x.s)
            {
                if (x.width < _WMAX)
                    x.width = x.width * 10 + *x.s - '0';
            }

            x.qual = strchr("hjltzL", *x.s) ? *x.s++ : '\0';        /* was hlL */

            if (x.qual == 'h' && *x.s == 'h')
                x.qual = 'b', ++x.s;
            else if (x.qual == 'l' && *x.s == 'l')
                x.qual = 'q', ++x.s;

            if (!strchr("cCn[", *x.s))
            {
                /* match leading white-space */
                while (isspace(ch = GET(&x)))
                    ;
                UNGETN(&x, ch);
            }

            if ((code = __getfield(&x)) <= 0)
                return (nconv == 0) ? code : nconv;

            if (x.stored)
                ++nconv;
        }
    }
}

