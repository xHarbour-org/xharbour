/****************************************************************************
 *                                                                          *
 * File    : _putfield.c                                                    *
 *                                                                          *
 * Purpose : __putfield function.                                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-06  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <string.h>
#include <wchar.h>
#include "xmath.h"
#include "xstdio.h"

/* convert a field for __printf */
int __putfield(__prtinfo *px, va_list *pap, char code, char *ac)
{
    switch (code)
    {
        /* switch on conversion specifier */
        case 'c':  /* convert a single character */
            if (px->qual != 'l')
            {
                ac[px->n0++] = va_arg(*pap, int);
            }
            else
            {
                /* convert as wide string */
                wchar_t wc[2];
#if WCHAR_MAX < INT_MAX
                wint_t wi = (wint_t)va_arg(*pap, int);
#else /* WCHAR_MAX < INT_MAX */
                wint_t wi = va_arg(*pap, wint_t);
#endif /* WCHAR_MAX < INT_MAX */

                wc[0] = wi, wc[1] = L'\0';
                px->prec = -1;
                if (__putstr(px, (const wchar_t *)wc) < 0)
                    return EOF;
            }
            break;

        case 'd': case 'i':  /* convert a signed decimal */
            px->v.li = px->qual == 'l' ? va_arg(*pap, long) :
                       px->qual == 'q' ? va_arg(*pap, long long) :
                       px->qual == 'j' ? va_arg(*pap, intmax_t) : va_arg(*pap, int);

            if (px->qual == 'h')
                px->v.li = (short)px->v.li;
            else if (px->qual == 'b')
                px->v.li = (signed char)px->v.li;
            else if (px->qual == 't' || px->qual == 'z')
                px->v.li = (ptrdiff_t)px->v.li;

            if (px->v.li < 0)   /* negate safely in __litob */
                ac[px->n0++] = '-';
            else if (px->flags & _FPL)
                ac[px->n0++] = '+';
            else if (px->flags & _FSP)
                ac[px->n0++] = ' ';

            px->s = &ac[px->n0];
            __litob(px, code);
            break;

        case 'o': case 'u':
        case 'x': case 'X':  /* convert unsigned */
            px->v.li = px->qual == 'l' ? va_arg(*pap, unsigned long) :
                       px->qual == 'q' ? va_arg(*pap, unsigned long long) :
                       px->qual == 'j' ? va_arg(*pap, uintmax_t) : va_arg(*pap, unsigned int);

            if (px->qual == 'h')
                px->v.li = (unsigned short)px->v.li;
            else if (px->qual == 'b')
                px->v.li = (unsigned char)px->v.li;
            else if (px->qual == 't' || px->qual == 'z')
                px->v.li = (size_t)px->v.li;

            if (px->flags & _FNO && px->v.li != 0 && (code == 'x' || code == 'X'))
                ac[px->n0++] = '0', ac[px->n0++] = code;

            px->s = &ac[px->n0];
            __litob(px, code);
            break;

        case 'e': case 'E':  /* convert floating */
        case 'g': case 'G':
        case 'f': case 'F':
        case 'a': case 'A':
            px->v.ld = (px->qual == 'L') ? va_arg(*pap, long double) : va_arg(*pap, double);

            if (__fptestl(&px->v.ld) == FP_NAN)
                ;  /* NaN */
            else if (LSIGN(px->v.ld))
                ac[px->n0++] = '-';
            else if (px->flags & _FPL)
                ac[px->n0++] = '+';
            else if (px->flags & _FSP)
                ac[px->n0++] = ' ';

            px->s = &ac[px->n0];
            __ldtob(px, code);
            break;

        case 'n':  /* return output count */
            switch (px->qual)
            {
                /* store in specified integer type */
                case 'b':
                    *va_arg(*pap, signed char *) = px->nchar;
                    break;

                case 'q':
                    *va_arg(*pap, long long *) = px->nchar;
                    break;

                case 'j':
                    *va_arg(*pap, intmax_t *) = px->nchar;
                    break;

                case 't':
                    *va_arg(*pap, ptrdiff_t *) = px->nchar;
                    break;

                case 'z':
                    *va_arg(*pap, size_t *) = px->nchar;
                    break;

                case 'h':
                    *va_arg(*pap, short *) = px->nchar;
                    break;

                case 'l':
                    *va_arg(*pap, long *) = px->nchar;
                    break;

                default:
                    *va_arg(*pap, int *) = px->nchar;
                    break;
            }
            break;

        case 'p':  /* convert a pointer, hex long version */
            px->v.li = (long long)((char *)va_arg(*pap, void *) - (char *)0);

            if (sizeof(void *) == sizeof(unsigned long))
                px->v.li &= ULONG_MAX;

            px->width = 8; px->flags |= _FZE;
            px->s = &ac[px->n0];
            __litob(px, 'x');
            break;

        case 's':  /* convert a string */
            if (px->qual != 'l')
            {
                /* determine length safely */
                char *s1;

                px->s = va_arg(*pap, char *);
                if (px->s == 0) px->s = "(null)";
                px->n1 = (px->prec < 0) ? strlen(px->s) : (s1 = (char *)memchr((void *)px->s, '\0', px->prec)) != 0 ? s1 - px->s : px->prec;
            }
            else if (__putstr(px, va_arg(*pap, const wchar_t *)) < 0)
                return EOF;
            break;

        case '%':  /* put a '%' */
            ac[px->n0++] = '%';
            break;

        default:  /* undefined specifier, print it out */
            ac[px->n0++] = (code != '\0') ? code : '%';
    }

    return 0;
}

