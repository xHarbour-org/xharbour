/****************************************************************************
 *                                                                          *
 * File    : output.c                                                       *
 *                                                                          *
 * Purpose : ISO C Compiler; Output functions.                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "lcc.h"

/* Static function prototypes */
static char *outs(const char *, FILE *, char *);
static char *outd(intmax_t, FILE *, char *);
static char *outu(uintmax_t, unsigned int, FILE *, char *);
static void outcurtok(FILE *, char *);

/****************************************************************************
 *                                                                          *
 * Function: print                                                          *
 *                                                                          *
 * Purpose : Formatted output to the output file.                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void print(const char *fmt, ...)
{
    if (OF)
    {
        static char cache[BUFSIZE], *cp = cache;
        va_list ap;
        char *p;

        va_start(ap, fmt);
        vfprint(NULL, cp, fmt, ap);
        cp += strlen(cp);
        va_end(ap);

        assert(cp - cache < BUFSIZE);

        /* add only complete lines to the assembly list */
        while ((p = strchr(cache, '\n')) != NULL)
        {
            aslist = listappend(stringn(cache, p - cache), aslist);
            memmove(cache, p + 1, cp - p);
            cp -= p - cache + 1;
        }
    }
    else
    {
        va_list ap;

        va_start(ap, fmt);
        vfprint(stdout, NULL, fmt, ap);
        va_end(ap);
    }
}

/****************************************************************************
 *                                                                          *
 * Function: fprint                                                         *
 *                                                                          *
 * Purpose : Formatted output to file f or string bp.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           03-10-12  Added argument bp, for DLL mode.                     *
 *                                                                          *
 ****************************************************************************/

void fprint(FILE *f, char *bp, const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    vfprint(f, bp, fmt, ap);
    va_end(ap);
}

/****************************************************************************
 *                                                                          *
 * Function: vfprint                                                        *
 *                                                                          *
 * Purpose : Formatted output to file f or string bp.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#define outchr(ch)  if (f) (void)putc((ch), (f)); else *bp++ = (ch);

void vfprint(FILE *f, char *bp, const char *fmt, va_list ap)
{
    for (; *fmt; fmt++)
    {
        if (*fmt == '%')
        {
            if (fmt[1] == '%')
                fmt++;

            switch (*++fmt)
            {
                case 'd': bp = outd(va_arg(ap, int), f, bp); break;
                case 'D': bp = outd(va_arg(ap, intmax_t), f, bp); break;
                case 'U': bp = outu(va_arg(ap, uintmax_t), 10, f, bp); break;
                case 'u': bp = outu(va_arg(ap, uint_t), 10, f, bp); break;
                case 'o': bp = outu(va_arg(ap, uint_t), 8, f, bp); break;
                case 'X': bp = outu(va_arg(ap, uintmax_t), 16, f, bp); break;
                case 'x': bp = outu(va_arg(ap, uint_t), 16, f, bp); break;
                case 'f':
                case 'e':
                case 'g':
                {
                    static char format[] = "%f";
                    char buf[128];
                    format[1] = *fmt;
                    sprintf(buf, format, va_arg(ap, double));
                    bp = outs(buf, f, bp);
                    break;
                }
                case 's':
                {
                    bp = outs(va_arg(ap, char *), f, bp);
                    break;
                }
                case 'S':
                {
                    char *s = va_arg(ap, char *);
                    int n = va_arg(ap, int);
                    if (s) for (; n-- > 0; s++)
                        outchr(*s);
                    break;
                }
                case 'p':
                {
                    void *p = va_arg(ap, void *);
                    if (p) bp = outs("0x", f, bp);
                    bp = outu((uintmax_t)p, 16, f, bp);
                    break;
                }
                case 'c':
                {
                    outchr(va_arg(ap, int));
                    break;
                }
                case 'k':
                {
                    int t = va_arg(ap, int);
                    static char *tokens[] = {
#define xx(a,b,c,d,e,f,g) g,
#define yy(a,b,c,d,e,f,g) g,
#include "token.h"
                    };
                    assert(tokens[t & 0x7F]);
                    bp = outs(tokens[t & 0x7F], f, bp);
                    break;
                }
                case 't':
                {
                    TYPE *ty = va_arg(ap, TYPE *);
                    /* outs(typestring(ty ? ty : voidtype, ""), f, bp); */
                    outtype(ty ? ty : voidtype, f, bp);
                    break;
                }
                case 'w':
                {
                    COORDINATE *p = va_arg(ap, COORDINATE *);
                    if (p->file && *p->file)
                        bp = outs(p->file, f, bp);
                    bp = outs("(", f, bp);
                    bp = outd(p->y, f, bp);
                    bp = outs(")", f, bp);
                    break;
                }
                case 'I':
                {
                    int n = va_arg(ap, int);
                    while (--n >= 0)
                        outchr(' ');
                    break;
                }
                case '!':
                {
                    outcurtok(f, bp);
                    break;
                }
                default:
                {
                    outchr(*fmt);
                    break;
                }
            }
        }
        else
        {
            outchr(*fmt);
        }
    }

    if (!f) *bp = '\0';
}
#undef outchr

/****************************************************************************
 *                                                                          *
 * Function: outs                                                           *
 *                                                                          *
 * Purpose : Output a nul-terminated string, to file f or string bp.        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static char *outs(const char *str, FILE *f, char *bp)
{
    if (str)
    {
        if (f)
            fputs(str, f);
        else
            while ((*bp = *str++) != 0) bp++;
    }

    return bp;
}

/****************************************************************************
 *                                                                          *
 * Function: outd                                                           *
 *                                                                          *
 * Purpose : Output a signed (intmax_t) integer, to file f or string bp.    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#define MAXSTR  32
static char *outd(intmax_t n, FILE *f, char *bp)
{
    char buf[MAXSTR], *s = &buf[MAXSTR];
    uintmax_t m;

    m = (n < 0) ? -n : n;

    *--s = '\0';

    do
        *--s = (char)(m%10 + '0');
    while ((m /= 10) != 0);

    if (n < 0)
        *--s = '-';

    return outs(s, f, bp);
}
#undef MAXSTR

/****************************************************************************
 *                                                                          *
 * Function: outu                                                           *
 *                                                                          *
 * Purpose : Output a unsigned (intmax_t) integer, to file f or string bp.  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#define MAXSTR  32
static char *outu(uintmax_t n, unsigned int base, FILE *f, char *bp)
{
    char buf[MAXSTR], *s = &buf[MAXSTR];

    *--s = '\0';
    do
        *--s = "0123456789abcdef"[n%base];
    while ((n /= base) != 0);

    return outs(s, f, bp);
}
#undef MAXSTR

/****************************************************************************
 *                                                                          *
 * Function: outcurtok                                                      *
 *                                                                          *
 * Purpose : Output the *current* token, to file f or string bp.            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           03-10-12  Added argument bp, for DLL mode.                     *
 *                                                                          *
 ****************************************************************************/

static void outcurtok(FILE *f, char *bp)
{
    switch (tok)
    {
        case ID:
            fprint(f, bp, "'%s'", tokstr);
            break;

        case ICONST:
            fprint(f, bp, "'%s'", value_to_str(toksym->type, toksym->u.c.v));
            break;

        case SCONST:
        {
            int i, n;

            if (ischar(toksym->type->type))
            {
                uchar_t *s = toksym->u.c.v.p;
                n = toksym->type->size;

                fprint(f, bp, "\"");
                for (i = 0; i < 20 && i < n && *s; s++, i++)
                    fprint(f, bp, isprint(*s) ? "%c" : "\\x%x", *s);
                fprint(f, bp, (i < n && *s) ? " ...\"" : "\"");
            }
            else  /* wchar_t string */
            {
                widechar_t *s = toksym->u.c.v.p;
                n = toksym->type->size / widechartype->size;
                assert(toksym->type->type->size == widechartype->size);

                fprint(f, bp, "L\"");
                for (i = 0; i < 20 && i < n && *s; s++, i++)
                    fprint(f, bp, isprint(*s) ? "%c" : "\\x%x", *s);
                fprint(f, bp, (i < n && *s) ? " ...\"" : "\"");
            }
            break;
        }

        case FCONST:
            fprint(f, bp, "'%S'", tokstr, (char *)cp-tokstr);
            break;

        case '`':
        case '\'':
            fprint(f, bp, "\"%k\"", tok);
            break;

        default:
            fprint(f, bp, "'%k'", tok);
            break;
    }
}

