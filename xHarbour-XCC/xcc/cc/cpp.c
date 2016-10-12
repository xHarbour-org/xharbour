/****************************************************************************
 *                                                                          *
 * File    : cpp.c                                                          *
 *                                                                          *
 * Purpose : ISO C Compiler; Preprocessor; Main module.                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           01-09-29  New global variable added: ifline[].                 *
 *           02-01-09  Support for _Pragma() directive added.               *
 *           04-12-23  Support for #pragma lib -> #pragma comment added.    *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#define PREPROCESSOR
#include "lcc.h"

#ifndef __POCC__
#include <share.h>
#endif

#ifdef PRERELEASE
extern size_t tot_lines;
#endif

/* Globals */
char outbuf[16384];
char *outpp = outbuf;
SOURCE *cursource;
char *curtime;
int incdepth;
TOKENROW maintr = {0};

/* Locals */
static int ifdepth = 0;
static int ifsatisfied[NIF];
static int ifline[NIF];
static bool_t skipping = FALSE;

/* Static function prototypes */
static void control(TOKENROW *);
static void Pragma(TOKENROW *);
static void pragma_lib(TOKENROW *);

/****************************************************************************
 *                                                                          *
 * Function: pp_init                                                        *
 *                                                                          *
 * Purpose : Initialize the preprocessor.                                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           03-07-22  Changed __POCC__ version to product version (2.60).  *
 *           03-10-20  Changed __POCC__ version to product version (2.70).  *
 *           03-12-10  Added dynamic reading of version for __POCC__.       *
 *           04-08-18  Bugfix: changed _WIN32 to _WIN32=1.                  *
 *                                                                          *
 ****************************************************************************/

void pp_init(void)
{
    short major, minor;
    char buf[20];

    setup_kwtab(FALSE);
    setup_include();
    setup_lexfsm();

    my_getversion(&major, &minor);
    wsprintf(buf, "__POCC__=%u", major * 100 + minor);  /* 2.72 -> 272 */
    pp_define(buf, 'D');
    pp_define("_WIN32=1", 'D');
#ifdef XHARBOUR
    pp_define("__XCC__=__POCC__", 'D');  // Ron 04-02-09
#endif /* XHARBOUR */

#if _MSC_VER >= 1200
    pp_define("__MSPOCC__", 'D');
#endif
}

/****************************************************************************
 *                                                                          *
 * Function: pp_define                                                      *
 *                                                                          *
 * Purpose : Define a preprocessor symbol (predefined or on command line).  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void pp_define(const char *s, int type /* D or U */)
{
    TOKENROW tr;

    set_source("<cmdarg>", -1, s);
    make_tokenrow(3, &tr);
    get_tokens(&tr, TRUE);
    define_or_undefine(&tr, type);
    unset_source();
}

/****************************************************************************
 *                                                                          *
 * Function: pp_start                                                       *
 *                                                                          *
 * Purpose : Start processing the preprocessor input.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void pp_start(const char *filename)
{
    time_t tm;
    int fd;

    tm = time(NULL);
    curtime = ctime(&tm);

    make_tokenrow(3, &maintr);

    if ((fd = _sopen(filename, _O_RDONLY|_O_SEQUENTIAL|_O_TEXT, _SH_DENYWR)) == -1)
        pp_error(RCFATAL(ERROR_CANT_OPEN_INPUT_FILE), filename);

    setup_hideset();

    set_source(my_strdup(filename), fd, NULL);
    (void)genline();  /* generate #line */
}

/****************************************************************************
 *                                                                          *
 * Function: pp_process                                                     *
 *                                                                          *
 * Purpose : Process the preprocessor input.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           01-09-29  New global variable added: ifline[].                 *
 *           04-06-21  Bugfix: must use trp->tp < trp->lp in _Pragma test!  *
 *                                                                          *
 ****************************************************************************/

bool_t pp_process(TOKENROW *trp)
{
    static int anymacros = 0;
    bool_t readmore = TRUE;
    bool_t eof = FALSE;

    while (readmore)
    {
        if (trp->tp >= trp->lp)
        {
            trp->tp = trp->lp = trp->bp;
            outpp = outbuf;
            anymacros |= get_tokens(trp, TRUE);
            trp->tp = trp->bp;
        }

        if (trp->tp->type == END)
        {
            if (--incdepth >= 0)
            {
                if (cursource->ifdepth != 0)
                {
                    pp_error(RCWARNING2(ERROR_UNTERM_INCLUDE_COND));
                    do
                        if (--ifdepth < skipping) skipping = 0;
                    while (--cursource->ifdepth);
                }
                unset_source();
                cursource->line += cursource->lineinc;
#ifdef PRERELEASE
                tot_lines += cursource->lineinc;
#endif
                trp->tp = trp->lp;
                (void)genline();
                continue;
            }

            if (ifdepth != 0)
                pp_error(RCERROR(ERROR_UNTERM_IF_DIRECT), ifline[ifdepth]);

            eof = TRUE;
            break;
        }

        if (trp->tp->type == SHARP)
        {
            trp->tp += 1;
            control(trp);
        }
        else if (!skipping && anymacros)
            expand_tokenrow(trp, NULL);

        if (skipping)
            empty_tokenrow(trp);

        /* look for a _Pragma directive after expansion */
        if (!skipping && trp->tp < trp->lp && trp->tp->type == NAME &&
            trp->tp->len == 7 && strncmp((char *)trp->tp->t, "_Pragma", 7) == 0)
        {
            trp->tp += 1;
            Pragma(trp);
        }

        readmore = put_tokens(trp);
        anymacros = 0;

        cursource->line += cursource->lineinc;
#ifdef PRERELEASE
        tot_lines += cursource->lineinc;
#endif
        if (cursource->lineinc > 1)
            readmore = genline();
    }

    return eof;
}

/****************************************************************************
 *                                                                          *
 * Function: control                                                        *
 *                                                                          *
 * Purpose : Handle preprocessor directives.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           01-09-29  New global variable added: ifline[].                 *
 *           04-03-13  Changed newstring() to my_strndup().                 *
 *                                                                          *
 ****************************************************************************/

static void control(TOKENROW *trp)
{
    NLIST *np;
    TOKEN *tp;

    tp = trp->tp;
    if (tp->type != NAME)
    {
        if (tp->type == NUMBER)
            goto kline;

        if (tp->type != NL)
            pp_error(RCERROR(ERROR_INVALID_CONTROL_LINE));

        return;  /* else empty line */
    }

    np = pp_lookup(tp, FALSE);
    if (np == NULL || (np->flag & ISKW) == 0 && !skipping)
    {
        pp_error(RCWARNING1(ERROR_UNKNOWN_PP_CONTROL), tp);
        return;
    }

    if (skipping)
    {
        switch (np->val)
        {
            case KENDIF:
                if (--ifdepth < skipping)
                    skipping = 0;
                --cursource->ifdepth;
                empty_tokenrow(trp);
                return;

            case KIFDEF:
            case KIFNDEF:
            case KIF:
                if (++ifdepth >= NIF)
                    pp_error(RCFATAL(ERROR_IF_TOO_DEEPLY_NESTED));
                ifline[ifdepth] = cursource->line;
                ++cursource->ifdepth;
                return;

            case KELIF:
            case KELSE:
                if (ifdepth <= skipping)
                    break;
                return;

            default:
                return;
        }
    }

    switch (np->val)
    {
        case KDEFINE:
            define(trp);
            break;

        case KUNDEF:
            tp += 1;
            if (tp->type != NAME || trp->lp - trp->bp != 4)
            {
                pp_error(RCERROR(ERROR_UNDEF_SYNTAX_ERROR));
                break;
            }
            if ((np = pp_lookup(tp, FALSE)) != NULL)
                np->flag &= ~ISDEFINED;
            break;

        case KPRAGMA:
            tp += 1;
            if (tp->type == NAME && tp->len == 3 && strncmp((char *)tp->t, "lib", 3) == 0)
                pragma_lib(trp);
#ifdef SUPPORTS_PRAGMA_ONCE
            else if (tp->type == NAME && tp->len == 4 && strncmp(tp->t, "once", 4) == 0)
                printf("found #pragma once in %s\n", cursource->filename);
#endif
            return;

        case KIFDEF:
        case KIFNDEF:
        case KIF:
            if (++ifdepth >= NIF)
                pp_error(RCFATAL(ERROR_IF_TOO_DEEPLY_NESTED));

            ifline[ifdepth] = cursource->line;
            ++cursource->ifdepth;
            ifsatisfied[ifdepth] = 0;

            if (eval(trp, np->val))
                ifsatisfied[ifdepth] = 1;
            else
                skipping = ifdepth;
            break;

        case KELIF:
            if (ifdepth == 0)
            {
                pp_error(RCERROR(ERROR_ELIF_WITHOUT_IF));
                return;
            }

            if (ifsatisfied[ifdepth] == 2)
                pp_error(RCERROR(ERROR_ELIF_AFTER_ELSE));

            if (eval(trp, np->val))
            {
                if (ifsatisfied[ifdepth])
                    skipping = ifdepth;
                else
                {
                    skipping = 0;
                    ifsatisfied[ifdepth] = 1;
                }
            }
            else
            {
                skipping = ifdepth;
            }
            break;

        case KELSE:
            if (ifdepth == 0 || cursource->ifdepth == 0)
            {
                pp_error(RCERROR(ERROR_ELSE_WITHOUT_IF));
                return;
            }

            if (ifsatisfied[ifdepth] == 2)
                pp_error(RCERROR(ERROR_ELSE_AFTER_ELSE));

            if (trp->lp - trp->bp != 3)
                pp_error(RCERROR(ERROR_ELSE_SYNTAX_ERROR));

            skipping = ifsatisfied[ifdepth] ? ifdepth : 0;
            ifsatisfied[ifdepth] = 2;
            break;

        case KENDIF:
            if (ifdepth == 0 || cursource->ifdepth == 0)
            {
                pp_error(RCERROR(ERROR_ENDIF_WITHOUT_IF));
                return;
            }

            --ifdepth;
            --cursource->ifdepth;

            if (trp->lp - trp->bp != 3)
                pp_error(RCWARNING1(ERROR_ENDIF_SYNTAX_ERROR));
            break;

        case KERROR:
            trp->tp = tp+1;
            pp_error(RCFATAL(ERROR_ERROR_DIRECT), trp);
            break;

        case KLINE:
            trp->tp = tp+1;
            expand_tokenrow(trp, "<line>");
            tp = trp->bp+2;
kline:
            if (tp+1 >= trp->lp || tp->type != NUMBER || tp+3 < trp->lp ||
               (tp+3 == trp->lp && ((tp+1)->type != STRING) ||
                *(tp+1)->t == 'L'))
            {
                pp_error(RCERROR(ERROR_LINE_SYNTAX_ERROR));
                return;
            }

            cursource->line = atol((char *)tp->t)-1;
            if (cursource->line < 0 || cursource->line > 2147483647)  /* old limit 32767 */
                pp_error(RCWARNING1(ERROR_LINE_NUMBER_OUT_OF_RANGE));

            tp = tp+1;
            if (tp+1 < trp->lp)
                cursource->filename = my_strndup((char *)tp->t+1, tp->len-2);
            return;

        case KDEFINED:
            pp_error(RCERROR(ERROR_CONTROL_SYNTAX_ERROR));
            break;

        case KINCLUDE:
            include(trp);
            trp->lp = trp->bp;
            return;

        case KEVAL:
            eval(trp, np->val);
            break;

        default:
            pp_error(RCFATAL(ERROR_CONTROL_NOT_IMPLEMENTED), tp);
            break;
    }

    empty_tokenrow(trp);
    return;
}

/****************************************************************************
 *                                                                          *
 * Function: Pragma                                                         *
 *                                                                          *
 * Purpose : Handle _Pragma directive.                                      *
 *                                                                          *
 * Comment : Our approach is to convert the _Pragma directive to a          *
 *           normal #pragma and let the compiler handle it as usual.        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           02-01-09  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void Pragma(TOKENROW *trp)
{
    char s[512], *sp = s;

    if (trp->lp - trp->tp < 4 || trp->tp->type != LP || (trp->tp+1)->type != STRING)
        goto syntax;

    sp += sprintf(sp, "#pragma ");

    trp->tp += 1;
    while (trp->tp->type == STRING)  /* concat strings */
    {
        char *cp, *ep;

        cp = (char *)trp->tp->t + 1;
        ep = (char *)trp->tp->t + trp->tp->len - 1;

        while (cp < ep)
        {
            if (*cp == '\\')
            {
                if (*++cp != '"' && *cp != '\\')
                    *sp++ = '\\';
            }

            *sp++ = *cp++;
        }

        trp->tp += 1;
        if (trp->lp - trp->tp < 2)
            goto syntax;
    }

    *sp++ = '\n';
    *sp = 0;

    if (trp->lp - trp->tp < 2 || trp->tp->type != RP || (trp->tp+1)->type != NL)
        goto syntax;

    incdepth++;
    set_source("<_Pragma>", -1, s);

    trp->lp = trp->bp;
    return;

syntax:
    pp_error(RCERROR(ERROR_CONTROL_SYNTAX_ERROR));
}

/****************************************************************************
 *                                                                          *
 * Function: pragma_lib                                                     *
 *                                                                          *
 * Purpose : Handle #pragma lib directive.                                  *
 *                                                                          *
 * Comment : Our approach is to convert the #pragma lib directive to a      *
 *           #pragma comment, and let the compiler handle it as usual.      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-12-23  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void pragma_lib(TOKENROW *trp)
{
    char s[256];

    if (trp->lp - trp->tp != 4 || (trp->tp+2)->type != STRING || (trp->tp+2)->len > 128)
        return;

    sprintf(s, "#pragma comment(lib,%.*s)\n", (trp->tp+2)->len, (trp->tp+2)->t);

    incdepth++;
    set_source("<pragma>", -1, s);

    trp->lp = trp->bp;
}

/****************************************************************************
 *                                                                          *
 * Function: pp_error                                                       *
 *                                                                          *
 * Purpose : Display error message.                                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-07-10  Added support for disabled warnings.                 *
 *                                                                          *
 ****************************************************************************/

void pp_error(ulong_t err, ...)
{
    va_list ap;
    char *cp;
    const char *ep;
    TOKEN *tp;
    TOKENROW *trp;
    SOURCE *s;
    int i;
    char fmt[256];

    /* ignore unwanted warnings */
    if (ISWARNING(err) && (WARNLEVEL(err) > options.warnlevel || disabled_warning(err)))
        return;

    /* find the most recent "real" file name */
    for (s = cursource; s && s->fd == -1; s = s->next)
        ;

    if (s != 0)
        printmsg(MSG_ERRTEXT_FILE_LINE, s->filename, s->line);

    /* C language warnings and errors contains the number */
    if (ERRNUM(err) >= CLANG_ERRNUM_MIN && ERRNUM(err) < CLANG_ERRNUM_MAX)
        sprintf(fmt, " #%u", ERRNUM(err));
    else
        *fmt = '\0';

    if (ISFATAL(err)) printmsg(MSG_ERRTEXT_FATAL, fmt);
    if (ISERROR(err)) printmsg(MSG_ERRTEXT_ERROR, fmt);
    if (ISWARNING(err)) printmsg(MSG_ERRTEXT_WARNING, fmt);

    /* load message from the resources */
    if (!FormatMessage(FORMAT_MESSAGE_FROM_HMODULE|FORMAT_MESSAGE_IGNORE_INSERTS, hmod,
        err & ~(ERROR_SEVERITY_FATAL|ERROR_SEVERITY_ERROR|ERROR_SEVERITY_WARNING|ERROR_SEVERITY_WARNING2),
        0, fmt, NELEMS(fmt), NULL))
    {
        sprintf(fmt, "*** No message for error 0x%X ***", err);
    }

    CharToOemA(fmt, fmt);

    va_start(ap, err);
    for (ep = fmt; *ep; ep++)
    {
        if (*ep == '%')
        {
            if (ep[1] == '%')
                ep++;

            switch (*++ep)
            {
                case 'c':
                    i = va_arg(ap, char);
                    fprintf(stdout, "%c", i);
                    break;

                case 's':
                    cp = va_arg(ap, char *);
                    fprintf(stdout, "%s", cp);
                    break;

                case 'd':
                    i = va_arg(ap, int);
                    fprintf(stdout, "%d", i);
                    break;

                case 't':
                    tp = va_arg(ap, TOKEN *);
                    fprintf(stdout, "%.*s", tp->len, tp->t);
                    break;

                case 'r':
                    trp = va_arg(ap, TOKENROW *);
                    for (tp = trp->tp; tp < trp->lp && tp->type != NL; tp++)
                    {
                        if (tp > trp->tp && tp->wslen)
                            fputc(' ', stdout);
                        fprintf(stdout, "%.*s", tp->len, tp->t);
                    }
                    break;

                default:
                    fputc(*ep, stdout);
                    break;
            }
        }
        else
        {
            fputc(*ep, stdout);
        }
    }
    va_end(ap);

    fflush(stdout);

    if (ISFATAL(err)) errorexit(1);
    if (ISERROR(err)) nerrs++;
}

