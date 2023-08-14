/****************************************************************************
 *                                                                          *
 * File    : eval.c                                                         *
 *                                                                          *
 * Purpose : ISO C Compiler; Preprocessor; Conditional expressions.         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#define PREPROCESSOR
#include "lcc.h"

#define NSTAK  32
#define SGN    0
#define UNS    1
#define UND    2

#define UNSMARK  0x1000

struct value {
    intmax_t val;
    int type;
};

/* conversion types */
#define RELAT  1
#define ARITH  2
#define LOGIC  3
#define SPCL   4
#define SHIFT  5
#define UNARY  6

/* operator priority, arity, and conversion type, indexed by tokentype */
struct pri {
    char pri;
    char arity;
    char ctype;
}
priority[] = {
    { 0, 0, 0 },            /* END */
    { 0, 0, 0 },            /* UNCLASS */
    { 0, 0, 0 },            /* NAME */
    { 0, 0, 0 },            /* NUMBER */
    { 0, 0, 0 },            /* STRING */
    { 0, 0, 0 },            /* CCON */
    { 0, 0, 0 },            /* NL */
    { 0, 0, 0 },            /* WS */
    { 0, 0, 0 },            /* DSHARP */
    { 11, 2, RELAT },       /* EQ */
    { 11, 2, RELAT },       /* NEQ */
    { 12, 2, RELAT },       /* LEQ */
    { 12, 2, RELAT },       /* GEQ */
    { 13, 2, SHIFT },       /* LSH */
    { 13, 2, SHIFT },       /* RSH */
    { 7, 2, LOGIC },        /* LAND */
    { 6, 2, LOGIC },        /* LOR */
    { 0, 0, 0 },            /* PPLUS */
    { 0, 0, 0 },            /* MMINUS */
    { 0, 0, 0 },            /* ARROW */
    { 0, 0, 0 },            /* SBRA */
    { 0, 0, 0 },            /* SKET */
    { 3, 0, 0 },            /* LP */
    { 3, 0, 0 },            /* RP */
    { 0, 0, 0 },            /* DOT */
    { 10, 2, ARITH },       /* AND */
    { 15, 2, ARITH },       /* STAR */
    { 14, 2, ARITH },       /* PLUS */
    { 14, 2, ARITH },       /* MINUS */
    { 16, 1, UNARY },       /* TILDE */
    { 16, 1, UNARY },       /* NOT */
    { 15, 2, ARITH },       /* SLASH */
    { 15, 2, ARITH },       /* PCT */
    { 12, 2, RELAT },       /* LT */
    { 12, 2, RELAT },       /* GT */
    { 9, 2, ARITH },        /* CIRC */
    { 8, 2, ARITH },        /* OR */
    { 5, 2, SPCL },         /* QUEST */
    { 5, 2, SPCL },         /* COLON */
    { 0, 0, 0 },            /* ASGN */
    { 4, 2, 0 },            /* COMMA */
    { 0, 0, 0 },            /* SHARP */
    { 0, 0, 0 },            /* SEMIC */
    { 0, 0, 0 },            /* CBRA */
    { 0, 0, 0 },            /* CKET */
    { 0, 0, 0 },            /* ASPLUS */
    { 0, 0, 0 },            /* ASMINUS */
    { 0, 0, 0 },            /* ASSTAR */
    { 0, 0, 0 },            /* ASSLASH */
    { 0, 0, 0 },            /* ASPCT */
    { 0, 0, 0 },            /* ASCIRC */
    { 0, 0, 0 },            /* ASLSH */
    { 0, 0, 0 },            /* ASRSH */
    { 0, 0, 0 },            /* ASOR */
    { 0, 0, 0 },            /* ASAND */
    { 0, 0, 0 },            /* ELLIPS */
    { 0, 0, 0 },            /* DSHARP1 */
    { 0, 0, 0 },            /* NAME1 */
    { 16, 1, UNARY },       /* DEFINED */
    { 16, 0, UNARY },       /* UMINUS */
};

/* Locals */
static struct value vals[NSTAK], *vp;
static enum toktype ops[NSTAK], *op;

/* Static function prototypes */
static int evalop(struct pri);
static struct value tokval(TOKEN *);
static int digit(int);

/****************************************************************************
 *                                                                          *
 * Function: eval                                                           *
 *                                                                          *
 * Purpose : Evaluate an #if, #elif, #ifdef or #ifndef line.                *
 *           trp->tp points to the keyword.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

intmax_t eval(TOKENROW *trp, int kw)
{
    TOKEN *tp;
    int ntok;
    int rand;

    trp->tp++;
    if (kw == KIFDEF || kw == KIFNDEF)
    {
        NLIST *np;

        if (trp->lp - trp->bp != 4 || trp->tp->type != NAME)
        {
            pp_error(RCERROR(ERROR_IF_SYNTAX_ERROR));
            return 0;
        }

        np = pp_lookup(trp->tp, FALSE);
        return (kw == KIFDEF) == (np && (np->flag & (ISDEFINED|ISMAC)));
    }

    ntok = trp->tp - trp->bp;
    kwdefined->val = KDEFINED;  /* activate special meaning of defined */
    expand_tokenrow(trp, "<if>");
    kwdefined->val = NAME;
    vp = vals;
    op = ops;
    *op++ = END;

    for (rand = FALSE, tp = trp->bp + ntok; tp < trp->lp; tp++)
    {
        switch (tp->type)
        {
            case WS:
            case NL:
                continue;

            /* nilary */
            case NAME:
            case NAME1:
            case NUMBER:
            case CCON:
            case STRING:
                if (rand) goto syntax;
                *vp++ = tokval(tp);
                rand = TRUE;
                continue;

            /* unary */
            case DEFINED:
            case TILDE:
            case NOT:
                if (rand) goto syntax;
                *op++ = tp->type;
                continue;

            /* unary-binary */
            case PLUS:
            case MINUS:
            case STAR:
            case AND:
                if (!rand)
                {
                    if (tp->type == MINUS)
                        *op++ = UMINUS;

                    if (tp->type == STAR || tp->type == AND)
                    {
                        pp_error(RCERROR(ERROR_INVALID_OPERATOR_IN_IF));
                        return 0;
                    }
                    continue;
                }
                /* fall through */

            /* plain binary */
            case EQ:
            case NEQ:
            case LEQ:
            case GEQ:
            case LSH:
            case RSH:
            case LAND:
            case LOR:
            case SLASH:
            case PCT:
            case LT:
            case GT:
            case CIRC:
            case OR:
            case QUEST:
            case COLON:
            case COMMA:
                if (!rand) goto syntax;
                if (evalop(priority[tp->type]) != 0)
                    return 0;
                *op++ = tp->type;
                rand = FALSE;
                continue;

            case LP:
                if (rand) goto syntax;
                *op++ = LP;
                continue;

            case RP:
                if (!rand) goto syntax;
                if (evalop(priority[RP]) != 0)
                    return 0;
                if (op <= ops || op[-1] != LP) goto syntax;
                op--;
                continue;

            default:
                pp_error(RCERROR(ERROR_BAD_OPERATOR_IN_IF), tp);
                return 0;
        }
    }

    if (!rand) goto syntax;

    if (evalop(priority[END]) != 0)
        return 0;

    if (op != &ops[1] || vp != &vals[1])
    {
        pp_error(RCERROR(ERROR_BOTCH_IN_IF));
        return 0;
    }

    if (vals[0].type == UND)
        pp_error(RCERROR(ERROR_UNDEF_EXPRESSION_VALUE));

    return vals[0].val;

syntax:
    pp_error(RCERROR(ERROR_IF_SYNTAX_ERROR));
    return 0;
}

/****************************************************************************
 *                                                                          *
 * Function: evalop                                                         *
 *                                                                          *
 * Purpose : Apply operator to operand(s) on top of stack.                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-08-19  Bugfix: expressions like "(2 - 1 - 1 != 0)" were     *
 *                     evaluated as "(2 - (1 - 1) != 0)".                   *
 *                                                                          *
 ****************************************************************************/

static int evalop(struct pri pri)
{
    struct value v1;
    struct value v2 = {0};
    intmax_t rv1;
    intmax_t rv2;
    int rtype;
    int oper;

    rv2 = 0; rtype = 0;
    /* old buggy code: while (pri.pri < priority[op[-1]].pri) */
    while (pri.pri < priority[op[-1]].pri || priority[op[-1]].arity == 2 && pri.pri <= priority[op[-1]].pri)  /* bugfix 04-08-19 */
    {
        oper = *--op;
        if (priority[oper].arity == 2)
        {
            v2 = *--vp;
            rv2 = v2.val;
        }
        v1 = *--vp;
        rv1 = v1.val;

/*lint -e574 -e644 */
        switch (priority[oper].ctype)
        {
            case 0:
            default:
                pp_error(RCWARNING1(ERROR_IF_SYNTAX_ERROR));
                return 1;

            case ARITH:
            case RELAT:
                if (v1.type == UNS || v2.type == UNS)
                    rtype = UNS;
                else
                    rtype = SGN;

                if (v1.type == UND || v2.type == UND)
                    rtype = UND;

                if (priority[oper].ctype == RELAT && rtype == UNS)
                {
                    oper |= UNSMARK;
                    rtype = SGN;
                }
                break;

            case SHIFT:
                if (v1.type == UND || v2.type == UND)
                    rtype = UND;
                else
                    rtype = v1.type;

                if (rtype == UNS)
                    oper |= UNSMARK;
                break;

            case UNARY:
                rtype = v1.type;
                break;

            case LOGIC:
            case SPCL:
                break;
        }

        switch (oper)
        {
            case EQ: case EQ|UNSMARK:
                rv1 = (rv1 == rv2); break;
            case NEQ: case NEQ|UNSMARK:
                rv1 = (rv1 != rv2); break;
            case LEQ:
                rv1 = (rv1 <= rv2); break;
            case GEQ:
                rv1 = (rv1 >= rv2); break;
            case LT:
                rv1 = (rv1 < rv2); break;
            case GT:
                rv1 = (rv1 > rv2); break;
            case LEQ|UNSMARK:
                rv1 = ((uintmax_t)rv1 <= (uintmax_t)rv2); break;
            case GEQ|UNSMARK:
                rv1 = ((uintmax_t)rv1 >= (uintmax_t)rv2); break;
            case LT|UNSMARK:
                rv1 = ((uintmax_t)rv1 < (uintmax_t)rv2); break;
            case GT|UNSMARK:
                rv1 = ((uintmax_t)rv1 > (uintmax_t)rv2); break;
            case LSH:
                rv1 <<= rv2; break;
            case LSH|UNSMARK:
                rv1 = ((uintmax_t)rv1 << rv2); break;
            case RSH:
                rv1 >>= rv2; break;
            case RSH|UNSMARK:
                rv1 = ((uintmax_t)rv1 >> rv2); break;

            case LAND:
                rtype = UND;
                if (v1.type == UND)
                    break;
                if (rv1 != 0)
                {
                    if (v2.type == UND)
                        break;
                    rv1 = (rv2 != 0);
                }
                else rv1 = 0;
                rtype = SGN;
                break;

            case LOR:
                rtype = UND;
                if (v1.type == UND)
                    break;
                if (rv1 == 0)
                {
                    if (v2.type == UND)
                        break;
                    rv1 = (rv2 != 0);
                }
                else rv1 = 1;
                rtype = SGN;
                break;

            case AND:
                rv1 &= rv2; break;
            case STAR:
                rv1 *= rv2; break;
            case PLUS:
                rv1 += rv2; break;
            case MINUS:
                rv1 -= rv2; break;
            case UMINUS:
                if (v1.type == UND)
                    rtype = UND;
                rv1 = -rv1; break;
            case OR:
                rv1 |= rv2; break;
            case CIRC:
                rv1 ^= rv2; break;
            case TILDE:
                rv1 = ~rv1; break;
            case NOT:
                rv1 = !rv1; if (rtype != UND) rtype = SGN; break;

            case SLASH:
                if (rv2 == 0)
                {
                    rtype = UND;
                    break;
                }
                if (rtype == UNS)
                    rv1 /= (uintmax_t)rv2;
                else
                    rv1 /= rv2;
                break;

            case PCT:
                if (rv2 == 0)
                {
                    rtype = UND;
                    break;
                }
                if (rtype == UNS)
                    rv1 %= (uintmax_t)rv2;
                else
                    rv1 %= rv2;
                break;

            case COLON:
                if (op[-1] != QUEST)
                    pp_error(RCERROR(ERROR_BAD_COND_OPERATOR_IN_IF));
                else
                {
                    op--;
                    if ((--vp)->val == 0)
                        v1 = v2;
                    rtype = v1.type;
                    rv1 = v1.val;
                }
                break;

            case DEFINED:
                break;

            default:
                pp_error(RCERROR(ERROR_EVAL_BOTCH));
                return 1;
        }

/*lint +e574 +e644 */
        v1.val = rv1;
        v1.type = rtype;
        *vp++ = v1;
    }

    return 0;
}

/****************************************************************************
 *                                                                          *
 * Function: tokval                                                         *
 *                                                                          *
 * Purpose : Convert token into appropriate value.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static struct value tokval(TOKEN *tp)
{
    struct value v;
    NLIST *np;
    int i;
    int base;
    int c;
    uintmax_t n;
    uchar_t *p;

    v.type = SGN;
    v.val = 0;

    switch (tp->type)
    {
        case NAME:
            v.val = 0;
            break;

        case NAME1:
            np = pp_lookup(tp, FALSE);
            if (np != NULL && (np->flag & (ISDEFINED|ISMAC)))
                v.val = 1;
            break;

        case NUMBER:
            n = 0;
            base = 10;

            p = tp->t;
            c = p[tp->len];
            p[tp->len] = '\0';

            if (*p == '0')
            {
                base = 8;
                if (p[1] == 'x' || p[1] == 'X')
                {
                    base = 16;
                    p++;
                }
                p++;
            }

            for (;; p++)
            {
                if ((i = digit(*p)) < 0)
                    break;

                if (i >= base)
                    pp_error(RCWARNING1(ERROR_BAD_DIGIT_IN_NUMBER), tp);

                n *= base;
                n += i;
            }

            if (n > INTMAX_MAX && base != 10)  /* if (n >= 0x80000000 && base != 10) */
                v.type = UNS;

            for (; *p; p++)
            {
                if (*p == 'u' || *p == 'U')
                    v.type = UNS;
                else if (*p == 'l' || *p == 'L')
                    ;
                else
                {
                    pp_error(RCERROR(ERROR_BAD_NUMBER_IN_IF), tp);
                    break;
                }
            }

            v.val = n;
            tp->t[tp->len] = c;
            break;

        case CCON:
            n = 0;
            p = tp->t;
            if (*p == 'L')
            {
                p += 1;
                pp_error(RCWARNING1(ERROR_WIDE_CHAR_VALUE_UNDEF));
            }

            p += 1;
            if (*p == '\\')
            {
                p += 1;
                if ((i = digit(*p)) >= 0 && i <= 7)
                {
                    n = i;
                    p += 1;
                    if ((i = digit(*p)) >= 0 && i <= 7)
                    {
                        p += 1;
                        n <<= 3;
                        n += i;
                        if ((i = digit(*p)) >= 0 && i <= 7)
                        {
                            p += 1;
                            n <<= 3;
                            n += i;
                        }
                    }
                }
                else if (*p == 'x')
                {
                    p += 1;
                    while ((i = digit(*p)) >= 0 && i <= 15)
                    {
                        p += 1;
                        n <<= 4;
                        n += i;
                    }
                }
                else
                {
                    static char cvcon[] = "b\bf\fn\nr\rt\tv\v''\"\"??\\\\";
                    for (i = 0; i < sizeof(cvcon); i += 2)
                    {
                        if (*p == cvcon[i])
                        {
                            n = cvcon[i+1];
                            break;
                        }
                    }
                    p += 1;

                    if (i >= sizeof(cvcon))
                        pp_error(RCWARNING1(ERROR_UNDEF_ESC_IN_CHAR_CONST));
                }
            }
            else if (*p == '\'')
                pp_error(RCERROR(ERROR_EMPTY_CHAR_CONST));
            else
                n = *p++;

            if (*p != '\'')
                pp_error(RCWARNING1(ERROR_MULTIBYTE_VALUE_UNDEF));
            else if (n > 127)
                pp_error(RCWARNING2(ERROR_SIGNED_CHAR_CONST));

            v.val = n;
            break;

        case STRING:
            pp_error(RCERROR(ERROR_STRING_IN_IF));
            break;
    }

    return v;
}

/****************************************************************************
 *                                                                          *
 * Function: digit                                                          *
 *                                                                          *
 * Purpose : Return numeric value of a digit character.                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int digit(int i)
{
    if ('0' <= i && i <= '9')
        i -= '0';
    else if ('a' <= i && i <= 'f')
        i -= 'a'-10;
    else if ('A' <= i && i <= 'F')
        i -= 'A'-10;
    else
        i = -1;

    return i;
}
