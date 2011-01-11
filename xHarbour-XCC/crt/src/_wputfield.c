/****************************************************************************
 *                                                                          *
 * File    : _wputfield.c                                                   *
 *                                                                          *
 * Purpose : __wputfield function.                                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-10  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"
#include "xwstdio.h"

/* convert a field for __wprintf */
int __wputfield(__wprtinfo *px, va_list *pap, wchar_t code, wchar_t *ac)
{
    switch (code)
    {
        /* switch on conversion specifier */
        case L'c':  /* convert a single character */
            if (px->qual == L'l')
#if WCHAR_MAX < INT_MAX
                ac[px->n0++] = va_arg(*pap, int);
#else /* WCHAR_MAX < INT_MAX */
                ac[px->n0++] = va_arg(*pap, wint_t);
#endif /* WCHAR_MAX < INT_MAX */
            else
            {
                /* check conversion before storing */
                wint_t wc = __btowc(va_arg(*pap, int));
                if (wc == WEOF)
                    return EOF;
                ac[px->n0++] = wc;
            }
            break;

        case L'd': case L'i':  /* convert a signed decimal */
            px->v.li = px->qual == L'l' ? va_arg(*pap, long) :
                       px->qual == L'q' ? va_arg(*pap, long long) :
                       px->qual == L'j' ? va_arg(*pap, intmax_t) : va_arg(*pap, int);

            if (px->qual == L'h')
                px->v.li = (short)px->v.li;
            else if (px->qual == L'b')
                px->v.li = (signed char)px->v.li;
            else if (px->qual == L't' || px->qual == L'z')
                px->v.li = (ptrdiff_t)px->v.li;

            if (px->v.li < 0)  /* negate safely in __wlitob */
                ac[px->n0++] = L'-';
            else if (px->flags & _FPL)
                ac[px->n0++] = L'+';
            else if (px->flags & _FSP)
                ac[px->n0++] = L' ';

            px->s = &ac[px->n0];
            __wlitob(px, code);
            break;

        case L'o': case L'u':
        case L'x': case L'X':  /* convert unsigned */
            px->v.li = px->qual == L'l' ? va_arg(*pap, unsigned long) :
                       px->qual == L'q' ? va_arg(*pap, unsigned long long) :
                       px->qual == L'j' ? va_arg(*pap, uintmax_t) : va_arg(*pap, unsigned int);

            if (px->qual == L'h')
                px->v.li = (unsigned short)px->v.li;
            else if (px->qual == L'b')
                px->v.li = (unsigned char)px->v.li;
            else if (px->qual == L't' || px->qual == L'z')
                px->v.li = (size_t)px->v.li;

            if (px->flags & _FNO && px->v.li != 0 && (code == L'x' || code == L'X'))
                ac[px->n0++] = L'0', ac[px->n0++] = code;

            px->s = &ac[px->n0];
            __wlitob(px, code);
            break;

        case L'e': case L'E':  /* convert floating */
        case L'g': case L'G':
        case L'f': case L'F':
        case L'a': case L'A':
            px->v.ld = px->qual == L'L' ? va_arg(*pap, long double) : va_arg(*pap, double);
            if (LSIGN(px->v.ld))
                ac[px->n0++] = L'-';
            else if (px->flags & _FPL)
                ac[px->n0++] = L'+';
            else if (px->flags & _FSP)
                ac[px->n0++] = L' ';
            px->s = &ac[px->n0];
            __wldtob(px, code);
            break;

        case L'n':  /* return output count */
            switch (px->qual)
            {
                /* store in specified integer type */
                case L'b':
                    *va_arg(*pap, signed char *) = px->nchar;
                    break;

                case L'q':
                    *va_arg(*pap, long long *) = px->nchar;
                    break;

                case L'j':
                    *va_arg(*pap, intmax_t *) = px->nchar;
                    break;

                case L't':
                    *va_arg(*pap, ptrdiff_t *) = px->nchar;
                    break;

                case L'z':
                    *va_arg(*pap, size_t *) = px->nchar;
                    break;

                case L'h':
                    *va_arg(*pap, short *) = px->nchar;
                    break;

                case L'l':
                    *va_arg(*pap, long *) = px->nchar;
                    break;

                default:
                    *va_arg(*pap, int *) = px->nchar;
                    break;
            }
            break;

        case L'p':  /* convert a pointer, hex long version */
            px->v.li = (long long)((char *)va_arg(*pap, void *) - (char *)0);
            if (sizeof(void *) == sizeof(unsigned long))
                px->v.li &= ULONG_MAX;
            px->s = &ac[px->n0];
            __wlitob(px, L'x');
            break;

        case L's':  /* convert a string */
            if (px->qual == L'l')
            {
                /* determine length safely */
                wchar_t *s1;

                px->s = va_arg(*pap, wchar_t *);
                px->n1 = (px->prec < 0) ? wcslen(px->s) : (s1 = (wchar_t *)wmemchr(px->s, L'\0', px->prec)) != 0 ? s1 - px->s : px->prec;
            }
            else if (__wputstr(px, va_arg(*pap, const char *)) == EOF)
                return EOF;
            break;

        case L'%':  /* put a '%' */
            ac[px->n0++] = L'%';
            break;

        default:  /* undefined specifier, print it out */
            ac[px->n0++] = code != L'\0' ? code : L'%';
            break;
    }

    return 0;
}

