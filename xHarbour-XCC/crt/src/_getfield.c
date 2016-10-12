/****************************************************************************
 *                                                                          *
 * File    : _getfield.c                                                    *
 *                                                                          *
 * Purpose : __getfield function.                                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-06  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <ctype.h>
#include <string.h>
#include "xstdio.h"

/* convert a field */
int __getfield(__scninfo *px)
{
    px->stored = 0;
    switch (*px->s)
    {
        /* switch on conversion specifier */
        case 'c':  /* convert an array of char */
            return __getstr(px, 0);

        case 'p':  /* convert a pointer */
        case 'd': case 'i': case 'o':
        case 'u': case 'x': case 'X':
            return __getint(px);  /* convert an integer */

        case 'e': case 'E':
        case 'g': case 'G':
        case 'f': case 'F':
        case 'a': case 'A':
            return __getflt(px);  /* convert a floating */

        case 'n':  /* return input count */
            if (!px->noconv) switch (px->qual)
            {
                /* store in specified integer type */
                case 'b':
                    *va_arg(px->ap, signed char *) = px->nchar;
                    break;

                case 'q':
                    *va_arg(px->ap, long long *) = px->nchar;
                    break;

                case 'j':
                    *va_arg(px->ap, intmax_t *) = px->nchar;
                    break;

                case 't':
                    *va_arg(px->ap, ptrdiff_t *) = px->nchar;
                    break;

                case 'z':
                    *va_arg(px->ap, size_t *) = px->nchar;
                    break;

                case 'h':
                    *va_arg(px->ap, short *) = px->nchar;
                    break;

                case 'l':
                    *va_arg(px->ap, long *) = px->nchar;
                    break;

                default:
                    *va_arg(px->ap, int *) = px->nchar;
            }
            return 1;

        case 's':  /* convert a string */
            return __getstr(px, 1);

        case '%':
        {
            /* match a '%' */
            int ch;

            if ((ch = GET(px)) == '%')
                return 1;
            UNGETN(px, ch);
            return (ch == EOF) ? EOF : 0;
        }

        case '[':  /* convert a scan set */
            return __getstr(px, -1);

        default:  /* undefined specifier, quit */
            return 0;
    }
}

