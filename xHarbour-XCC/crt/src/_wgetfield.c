/****************************************************************************
 *                                                                          *
 * File    : _wgetfield.c                                                   *
 *                                                                          *
 * Purpose : __wgetfield function.                                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-09  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include "xwstdio.h"

/* convert a wide field */
int __wgetfield(__wscninfo *px)
{
    px->stored = 0;
    switch (*px->s)
    {
        /* switch on conversion specifier */
        case L'c':  /* convert an array of chars */
            return __wgetstr(px, 0);

        case L'p':  /* convert a pointer */
        case L'd': case L'i': case L'o':
        case L'u': case L'x': case L'X':
            return __wgetint(px);  /* convert an integer */

        case L'e': case L'E':
        case L'g': case L'G':
        case L'f': case L'F':
        case L'a': case L'A':
            return __wgetflt(px);  /* convert a floating */

        case L'n':  /* return input count */
            if (!px->noconv)
            {
                switch (px->qual)
                {
                    /* store in specified integer type */
                    case L'b':
                        *va_arg(px->ap, signed char *) = px->nchar;
                        break;

                    case L'q':
                        *va_arg(px->ap, long long *) = px->nchar;
                        break;

                    case L'j':
                        *va_arg(px->ap, intmax_t *) = px->nchar;
                        break;

                    case L't':
                        *va_arg(px->ap, ptrdiff_t *) = px->nchar;
                        break;

                    case L'z':
                        *va_arg(px->ap, size_t *) = px->nchar;
                        break;

                    case L'h':
                        *va_arg(px->ap, short *) = px->nchar;
                        break;

                    case L'l':
                        *va_arg(px->ap, long *) = px->nchar;
                        break;

                    default:
                        *va_arg(px->ap, int *) = px->nchar;
                        break;
                }
            }
            return 1;

        case L's':  /* convert a multibyte string */
            return __wgetstr(px, 1);

        case L'%':  /* match a '%' */
        {
            wint_t ch;

            if ((ch = WGET(px)) == L'%')
                return 1;

            WUNGETN(px, ch);
            return (ch == WEOF) ? EOF : 0;
        }

        case L'[':  /* convert a scan set */
            return __wgetstr(px, -1);

        default:   /* undefined specifier, quit */
            return 0;
    }
}

