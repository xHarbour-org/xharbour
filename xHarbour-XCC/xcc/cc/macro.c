/****************************************************************************
 *                                                                          *
 * File    : macro.c                                                        *
 *                                                                          *
 * Purpose : ISO C Compiler; Preprocessor; Macro management.                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           02-01-08  Added support for variadic arguments.                *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#define PREPROCESSOR
#include "lcc.h"

/* Static function prototypes */
static void expand(TOKENROW *, NLIST *);
static int gather_args(TOKENROW *, TOKENROW **, int *, int);
static void subst_args(NLIST *, TOKENROW *, TOKENROW **, int);
static void doconcat(TOKENROW *);
static int lookup_arg(NLIST *, TOKEN *);
static TOKENROW *stringify(TOKENROW *);
static void builtin(TOKENROW *, int);

/****************************************************************************
 *                                                                          *
 * Function: define                                                         *
 *                                                                          *
 * Purpose : Do a macro definition.                                         *
 *           tp points to the name being defined in the line.               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void define(TOKENROW *trp)
{
    TOKEN *tp;
    NLIST *np;
    TOKENROW *def;
    TOKENROW *args;

    tp = trp->tp+1;
    if (tp >= trp->lp || tp->type != NAME)
    {
        pp_error(RCERROR(ERROR_DEF_TOKEN_IS_NOT_A_NAME));
        return;
    }

    np = pp_lookup(tp, TRUE);
    if (np->flag & ISUNCHANGE)
    {
        pp_error(RCERROR(ERROR_DEF_TOKEN_CANT_BE_REDEF), tp);
        return;
    }

    /* collect arguments */
    args = NULL;
    tp += 1;
    if (tp < trp->lp && tp->type == LP && tp->wslen == 0)
    {
        /* macro with arguments */
        int narg = 0;

        args = my_alloc(sizeof(*args));
        make_tokenrow(2, args);

        tp += 1;
        if (tp->type != RP)
        {
            int err = 0;

            for (;;)
            {
                static TOKEN vatoken = { NAME, 0, 0, 0, 11, (uchar_t *)"__VA_ARGS__" };
                TOKEN *atp;

                if (tp->type == NAME && tp->len == 11 && strncmp((char *)vatoken.t, (char *)tp->t, tp->len) == 0)
                    pp_error(RCERROR(ERROR_RESERVED_MACRO_ARGUMENT));

                if (tp->type == ELLIPS)
                {
                    *tp = vatoken;
                    np->flag |= ISVARIADIC;
                }

                if (tp->type != NAME)
                {
                    err++;
                    break;
                }

                if (narg >= args->max)
                    grow_tokenrow(args);

                for (atp = args->bp; atp < args->lp; atp++)
                {
                    if (atp->len == tp->len && strncmp((char *)atp->t, (char *)tp->t, tp->len) == 0)
                        pp_error(RCERROR(ERROR_DUP_MACRO_ARGUMENT));
                }

                *args->lp++ = *tp;
                narg++;

                tp += 1;

                if (tp->type == RP)
                    break;

                if (tp->type != COMMA || (np->flag & ISVARIADIC) != 0)
                {
                    err++;
                    break;
                }

                tp += 1;
            }

            if (err)
            {
                pp_error(RCERROR(ERROR_MACRO_ARGS_SYNTAX_ERROR));
                return;
            }
        }

        tp += 1;
    }

    trp->tp = tp;
    if (((trp->lp)-1)->type == NL)
        trp->lp -= 1;

    def = norm_tokenrow(trp);

    if (np->flag & ISDEFINED)
    {
        if (compare_tokens(def, np->vp) ||
            (np->ap == NULL) != (args == NULL) ||
            np->ap && compare_tokens(args, np->ap))
        {
            pp_error(RCERROR(ERROR_MACRO_REDEFINITION), trp->bp+2);
        }
    }

    if (args)
    {
        TOKENROW *tap;
        tap = norm_tokenrow(args);
        my_free(args->bp);
        args = tap;
    }

    np->ap = args;
    np->vp = def;
    np->flag |= ISDEFINED;
}

/****************************************************************************
 *                                                                          *
 * Function: define_or_undefine                                             *
 *                                                                          *
 * Purpose : Definition received via -D or -U.                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           03-08-12  Bugfix: -D<symbol> same as #define <symbol>.         *
 *                                                                          *
 ****************************************************************************/

void define_or_undefine(TOKENROW *trp, int type)
{
    static TOKEN niltoken = { 0 };
    static TOKENROW niltr = { &niltoken, &niltoken, &niltoken, 0 };
    NLIST *np;

    trp->tp = trp->bp;
    if (type == 'U')
    {
        if ((trp->lp - trp->tp) != 2 || trp->tp->type != NAME)
            goto syntax;

        if ((np = pp_lookup(trp->tp, FALSE)) == NULL)
            return;

        np->flag &= ~ISDEFINED;
        return;
    }

    if (trp->tp >= trp->lp || trp->tp->type != NAME)
        goto syntax;

    np = pp_lookup(trp->tp, TRUE);
    np->flag |= ISDEFINED;

    trp->tp += 1;
    if (trp->tp >= trp->lp || trp->tp->type == END)
    {
        np->vp = &niltr;
        return;
    }

    if (trp->tp->type != ASGN)
        goto syntax;

    trp->tp += 1;

    if ((trp->lp-1)->type == END)
        trp->lp -= 1;

    np->vp = norm_tokenrow(trp);
    return;

syntax:
    pp_error(RCFATAL(ERROR_INVALID_D_OR_U_ARG), trp);
}

/****************************************************************************
 *                                                                          *
 * Function: expand_tokenrow                                                *
 *                                                                          *
 * Purpose : Do macro expansion in a row of tokens.                         *
 *           Argument specname is NULL if more input can be gathered.       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void expand_tokenrow(TOKENROW *trp, const char *specname)
{
    TOKEN *tp;
    NLIST *np;

    if (specname) set_source(specname, -1, "");

    for (tp = trp->tp; tp < trp->lp; )
    {
        if (tp->type != NAME ||
            quicklook(tp->t[0], (tp->len > 1) ? tp->t[1] : 0) == 0 ||
            (np = pp_lookup(tp, FALSE)) == NULL ||
            (np->flag & (ISDEFINED|ISMAC)) == 0 ||
            (tp->hideset && check_hideset(tp->hideset, np)))
        {
            tp++;
            continue;
        }

        trp->tp = tp;
        if (np->val == KDEFINED)
        {
            tp->type = DEFINED;

            if ((tp+1) < trp->lp && (tp+1)->type == NAME)
                (tp+1)->type = NAME1;
            else if ((tp+3) < trp->lp && (tp+1)->type == LP &&
                (tp+2)->type == NAME && (tp+3)->type == RP)
                (tp+2)->type = NAME1;
            else
                pp_error(RCERROR(ERROR_BAD_DEFINED_SYNTAX));

            tp++;
            continue;
        }

        if (np->flag & ISMAC)
            builtin(trp, np->val);
        else
            expand(trp, np);

        tp = trp->tp;
    }

    if (specname) unset_source();
}

/****************************************************************************
 *                                                                          *
 * Function: expand                                                         *
 *                                                                          *
 * Purpose : Expand the macro whose name is np, at token trp->tp,           *
 *           in the tokenrow. Return trp->tp at the first token next        *
 *           to be expanded (ordinarily the beginning of the expansion).    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-11-24  Try to handle special Microsoft /##/ pasting case.   *
 *                                                                          *
 ****************************************************************************/

static void expand(TOKENROW *trp, NLIST *np)
{
    TOKENROW ntr;
    TOKENROW *atr[NARG+1];
    TOKEN *tp;
    int ntokc;
    int narg;
    int varg;
    int hs;
    int i;

    copy_tokenrow(&ntr, np->vp);  /* copy macro value */
    if (np->ap == NULL)  /* parameterless */
    {
        ntokc = 1;
    }
    else
    {
        /* get index of __VA_ARGS__ argument, if any */
        varg = ((np->flag & ISVARIADIC) != 0) ? rowlen(np->ap)-1 : -1;

        ntokc = gather_args(trp, atr, &narg, varg);
        if (narg < 0)  /* not actually a call (no '(') */
        {
            trp->tp++;
            return;
        }

        if (narg != rowlen(np->ap))
        {
            pp_error(RCERROR(ERROR_MACRO_ARG_DISAGREEMENT));
            trp->tp->hideset = new_hideset(trp->tp->hideset, np);
            trp->tp += ntokc;
            return;
        }

        subst_args(np, &ntr, atr, narg);  /* put args into replacement */
        for (i = 0; i < narg; i++)
        {
            my_free(atr[i]->bp);
            my_free(atr[i]);
        }
    }

    doconcat(&ntr);  /* execute ## operators */

    hs = new_hideset(trp->tp->hideset, np);
    for (tp = ntr.bp; tp < ntr.lp; tp++)  /* distribute hidesets */
    {
        if (tp->type == NAME)
        {
            if (tp->hideset == 0)
                tp->hideset = hs;
            else
                tp->hideset = union_hideset(tp->hideset, hs);
        }
        else if (tp->type == MSCOM)
        {
            /* Microsoft /##/ case - remove rest of line (= comment) */
            ntokc = trp->lp - trp->tp;
            ntr.lp = ntr.bp;
            break;
        }
    }

    ntr.tp = ntr.bp;
    replace_tokens(trp, ntokc, &ntr);
    trp->tp -= rowlen(&ntr);
    my_free(ntr.bp);
}

/****************************************************************************
 *                                                                          *
 * Function: gather_args                                                    *
 *                                                                          *
 * Purpose : Gather an arglist, starting in trp with tp pointing at         *
 *           the macro name. Return total number of tokens passed,          *
 *           stash number of args found. trp->tp is not changed             *
 *           relative to the tokenrow.                                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int gather_args(TOKENROW *trp, TOKENROW **atr, int *narg, int varg)
{
    int parens = 1;
    int ntok = 0;
    int ntokp;
    TOKEN *bp;
    TOKEN *lp;
    TOKENROW ttr;
    bool_t needspace;

    *narg = -1;  /* means that there is no macro call */

    /* look for the ( */
    for (;;)
    {
        trp->tp++;
        ntok++;

        if (trp->tp >= trp->lp)
        {
            get_tokens(trp, FALSE);
            if ((trp->lp-1)->type == END)
            {
                trp->lp -= 1;
                trp->tp -= ntok;
                return ntok;
            }
        }

        if (trp->tp->type == LP)
            break;

        if (trp->tp->type != NL)
            return ntok;
    }

    *narg = 0;

    ntok++;
    ntokp = ntok;
    trp->tp++;

    /* search for the terminating ), possibly extending the row */
    needspace = FALSE;
    while (parens > 0)
    {
        if (trp->tp >= trp->lp)
            get_tokens(trp, FALSE);

        if (needspace)
        {
            needspace = FALSE;
            makespace(trp);
        }

        if (trp->tp->type == END)
        {
            trp->lp -= 1;
            trp->tp -= ntok;
            pp_error(RCERROR(ERROR_EOF_IN_MACRO_ARGLIST));
            return ntok;
        }

        if (trp->tp->type == NL)
        {
            trp->tp += 1;
            adjust_tokenrow(trp, -1);
            trp->tp -= 1;
            makespace(trp);
            needspace = TRUE;
            continue;
        }

        if (trp->tp->type == LP)
            parens++;
        else if (trp->tp->type == RP)
            parens--;

        trp->tp++;
        ntok++;
    }
    trp->tp -= ntok;

    /* now trp->tp won't move underneath us */
    lp = bp = trp->tp + ntokp;
    for (; parens >= 0; lp++)
    {
        if (lp->type == LP)
        {
            parens++;
            continue;
        }

        if (lp->type == RP)
            parens--;

        if (lp->type == DSHARP)
            lp->type = DSHARP1;  /* ## not special in arg */

        /* add normal argument, or keep collecting tokens if special __VA_ARGS__ argument */
        if (lp->type == COMMA && varg != *narg && parens == 0 || parens < 0 && (lp-1)->type != LP)
        {
            if (*narg >= NARG-1)
                pp_error(RCFATAL(ERROR_TOO_MANY_MACRO_ARGS));

            ttr.bp = ttr.tp = bp;
            ttr.lp = lp;
            atr[(*narg)++] = norm_tokenrow(&ttr);
            bp = lp+1;
        }
    }

    /* add special __VA_ARGS__ argument */
    if (varg == *narg && bp != lp)
    {
        if (*narg >= NARG-1)
            pp_error(RCFATAL(ERROR_TOO_MANY_MACRO_ARGS));

        ttr.bp = ttr.tp = bp;
        ttr.lp = lp;
        atr[(*narg)++] = norm_tokenrow(&ttr);
    }

    return ntok;
}

/****************************************************************************
 *                                                                          *
 * Function: subst_args                                                     *
 *                                                                          *
 * Purpose : Substitute the argument list into the replacement string.      *
 *           This would be simple except for ## and #.                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-12-07  Bugfix: handle L#string case.                        *
 *                                                                          *
 ****************************************************************************/

static void subst_args(NLIST *np, TOKENROW *trp, TOKENROW **atr, int narg)
{
    TOKEN *tp;
    int ntok;
    int argno;

    for (trp->tp = trp->bp; trp->tp < trp->lp; )
    {
        if (trp->tp->type == SHARP)  /* string operator */
        {
            tp = trp->tp;
            trp->tp += 1;

            if ((argno = lookup_arg(np, trp->tp)) < 0)
            {
                pp_error(RCERROR(ERROR_SHARP_WITHOUT_MACRO_ARG));
                continue;
            }

            ntok = 1 + (trp->tp - tp);
            trp->tp = tp;
            replace_tokens(trp, ntok, stringify(atr[argno]));
            /* make sure L#string is L"string", not L "string" (04-12-07) */
            if (tp > trp->bp && (tp-1)->type == NAME && strcmp((char *)(tp-1)->t, "L") == 0)
                tp->wslen = 0;
            continue;
        }

        /* skip empty macro arguments, MACRO(1,,3) */
        if (trp->tp->type == NAME && (argno = lookup_arg(np, trp->tp)) >= 0)
        {
            if (trp->tp+1 < trp->lp && (trp->tp+1)->type == DSHARP ||
                trp->tp != trp->bp && (trp->tp-1)->type == DSHARP)
            {
                replace_tokens(trp, 1, atr[argno]);
            }
            else
            {
                TOKENROW tatr;

                copy_tokenrow(&tatr, atr[argno]);
                expand_tokenrow(&tatr, "<macro>");
                replace_tokens(trp, 1, &tatr);
                my_free(tatr.bp);
            }
            continue;
        }

        trp->tp++;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: doconcat                                                       *
 *                                                                          *
 * Purpose : Evaluate the ## operators in a tokenrow.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           01-01-23  Bugfix: changed tt[128] to tt[MAXLINE].              *
 *           04-11-24  Bugfix: handle any number of tokens.                 *
 *                                                                          *
 ****************************************************************************/

static void doconcat(TOKENROW *trp)
{
    TOKEN *ltp;
    TOKEN *ntp;
    TOKENROW ntr;
    int len;

    for (trp->tp = trp->bp; trp->tp < trp->lp; trp->tp++)
    {
        if (trp->tp->type == DSHARP1)
        {
            trp->tp->type = DSHARP;
        }
        else if (trp->tp->type == DSHARP)
        {
            char tt[MAXLINE];

            ltp = trp->tp-1;
            ntp = trp->tp+1;

            if (ltp < trp->bp || ntp >= trp->lp)
            {
                pp_error(RCERROR(ERROR_DSHARP_AT_BORDER));
                continue;
            }

            len = ltp->len + ntp->len;
            strncpy((char *)tt, (char *)ltp->t, ltp->len);
            strncpy((char *)tt + ltp->len, (char *)ntp->t, ntp->len);
            tt[len] = '\0';

            set_source("<##>", -1, tt);
            make_tokenrow(3, &ntr);
            get_tokens(&ntr, TRUE);
            unset_source();

#if 0
            if ((ntr.lp - ntr.bp) != 2 || ntr.bp->type == UNCLASS)
                pp_error(RCWARNING1(ERROR_BAD_TOKEN_FROM_DSHARP), &ntr);

            ntr.lp = ntr.bp+1;
#else
            /* handle any number of tokens - like Do##Str##(): Str( = NAME + LP */
            if ((ntr.lp - ntr.bp) < 2 || ntr.bp->type == UNCLASS)
                pp_error(RCWARNING1(ERROR_BAD_TOKEN_FROM_DSHARP), &ntr);

            ntr.lp--;
#endif

            trp->tp = ltp;
            makespace(&ntr);
            replace_tokens(trp, (ntp - ltp) + 1, &ntr);
            my_free(ntr.bp);
            trp->tp--;
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: lookup_arg                                                     *
 *                                                                          *
 * Purpose : tp is a potential parameter name of macro mac;                 *
 *           look it up in mac's arglist, and if found, return the          *
 *           corresponding index in the argname array.                      *
 *           Return -1 if not found.                                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int lookup_arg(NLIST *np, TOKEN *tp)
{
    TOKEN *ap;

    if (tp->type != NAME || np->ap == NULL)
        return -1;

    for (ap = np->ap->bp; ap < np->ap->lp; ap++)
    {
        if (ap->len == tp->len && strncmp((char *)ap->t, (char *)tp->t, ap->len) == 0)
            return ap - np->ap->bp;
    }

    return -1;
}

/****************************************************************************
 *                                                                          *
 * Function: stringify                                                      *
 *                                                                          *
 * Purpose : Return a quoted version of the tokenrow (from # arg).          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#define STRLEN  512
static TOKENROW *stringify(TOKENROW *vp)
{
    static TOKEN t = { STRING };
    static TOKENROW tr = { &t, &t, &t+1, 1 };
    TOKEN *tp;
    uchar_t s[STRLEN], *sp = s;

    *sp++ = '"';
    for (tp = vp->bp; tp < vp->lp; tp++)
    {
        bool_t instring = tp->type == STRING || tp->type == CCON;
        uint_t i;
        uchar_t *cp;

        if (sp + 2*tp->len >= &s[STRLEN-10])
        {
            pp_error(RCERROR(ERROR_STR_MACRO_ARG_TOO_LONG));
            break;
        }

        if (tp->wslen && (tp->flag & XPWS) == 0)
            *sp++ = ' ';

        for (i = 0, cp = tp->t; i < tp->len; i++)
        {
            if (instring && (*cp == '"' || *cp == '\\'))
                *sp++ = '\\';

            *sp++ = *cp++;
        }
    }
    *sp++ = '"';
    *sp = '\0';

    sp = s;
    t.len = strlen((char *)sp);
    t.t = (uchar_t *)my_strndup((char *)sp, t.len);

    return &tr;
}

/****************************************************************************
 *                                                                          *
 * Function: builtin                                                        *
 *                                                                          *
 * Purpose : Expand a builtin name.                                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-08-19  Bugfix: emit KSTDCVERSION and KSTDCISO10646 as long. *
 *                                                                          *
 ****************************************************************************/

static void builtin(TOKENROW *trp, int biname)
{
    char *op;
    TOKEN *tp;
    SOURCE *s;

    tp = trp->tp;
    trp->tp++;

    /* need to find the real source */
    for (s = cursource; s && s->fd == -1; s = s->next)
        ;

    if (s == NULL)
        s = cursource;

    /* most are strings */
    tp->type = STRING;
    if (tp->wslen)
    {
        *outpp++ = ' ';
        tp->wslen = 1;
    }

    op = outpp;
    *op++ = '"';
    switch (biname)
    {
        case KLINENO:
            tp->type = NUMBER;
            op = outnum(op-1, s->line);
            break;

        case KFILE:
        {
            const char *src = s->filename;
            while ((*op++ = *src++) != 0)
                if (src[-1] == '\\') *op++ = '\\';
            op--;
            break;
        }

        case KDATE:
            strncpy(op, curtime+4, 7);
            strncpy(op+7, curtime+20, 4);
            op += 11;
            break;

        case KTIME:
            strncpy(op, curtime+11, 8);
            op += 8;
            break;

        case KSTDC:
            tp->type = NUMBER;
            op = outnum(op-1, 1);
            break;

        case KSTDCHOSTED:
            tp->type = NUMBER;
            op = outnum(op-1, 0);  /* freestanding implementation since no complex type */
            break;

        case KSTDCIEC559:
            tp->type = NUMBER;
            op = outnum(op-1, 1);  /* IEEE 854 */
            break;

        case KSTDCISO10646:
            tp->type = NUMBER;
            op = outnum(op-1, 199901);
            *op++ = 'L';  /* bugfix 04-08-19 */
            break;

        case KSTDCVERSION:
            tp->type = NUMBER;
            op = outnum(op-1, 199901);
            *op++ = 'L';  /* bugfix 04-08-19 */
            break;

        default:
            pp_error(RCFATAL(ERROR_INTERNAL_MACRO_BOTCH));
            return;
    }
    if (tp->type == STRING)
        *op++ = '"';

    tp->t = (uchar_t *)outpp;
    tp->len = op - outpp;
    outpp = op;
}
