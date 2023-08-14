/****************************************************************************
 *                                                                          *
 * File    : evaluate.c                                                     *
 *                                                                          *
 * Purpose : ISO C Compiler; Generic Assembler; Expression evaluator.       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "lcc.h"

#define scan  as.scanner.scan

#define STACK_INCREMENT  8
#define EXPRS_INCREMENT  128

static EXPR *stack_tempexpr;
static size_t stack_count;
static size_t stack_maxcount;

static EXPR **exprs;
static size_t exprs_count;
static size_t exprs_maxcount;

static TOKENVAL *tv;        /* the current token */
static int token;           /* the current token type */

static int *opflags;        /* ptr to operand flags */

static EVAL_HINTS *hint;
static bool_t inlined;      /* true if inline assembler mode */

/* Static function prototypes */
static EXPR *aexp0(int);
static EXPR *aexp1(int);
static EXPR *aexp2(int);
static EXPR *aexp3(int);
static EXPR *aexp4(int);
static EXPR *aexp5(int);
static EXPR *aexp6(int);
static EXPR *mul_vector_by_scalar(EXPR *, intmax_t, bool_t);
static EXPR *add_vectors(EXPR *, EXPR *);
static EXPR *unknown_expr(void);
static EXPR *scalar_expr(intmax_t);
static void reset_stack(void);
static void push_expr(long, intmax_t, void *);
static EXPR *finalize_stack(void);
static void asmexp_reset(void);

/****************************************************************************
 *                                                                          *
 * Function: asmexp                                                         *
 *                                                                          *
 * Purpose : Expression evaluator.                                          *
 *                                                                          *
 * Comment : The actual expression evaluator function. When called, it      *
 *           expects the first token of its expression to already be in     *
 *           '*atv'; if it is not, set atv->type to TOK_INVALID and it      *
 *           will start by calling the scanner.                             *
 *                                                                          *
 *           If a forward reference happens during evaluation, the          *
 *           evaluator must set '*aopflags' to OPFLAG_FORWARD if            *
 *           'aopflags' is non-NULL.                                        *
 *                                                                          *
 *           'critical' is non-zero if the expression may not contain       *
 *           forward references. The evaluator will report its own error    *
 *           if this occurs; if 'critical' is 1, the error will be          *
 *           "symbol not defined before use", whereas if 'critical' is 2,   *
 *           the error will be "symbol undefined".                          *
 *                                                                          *
 *           If 'ahint' is non-NULL, it gets filled in with some hints      *
 *           as to the base register in complex effective addresses.        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

EXPR *asmexp(TOKENVAL *atv, int *aopflags, int critical, EVAL_HINTS *ahint, bool_t ainlined)
{
    EXPR *e;

    /* copy to module-global storage */
    tv = atv;
    opflags = aopflags;
    hint = ahint;
    inlined = ainlined;

    if (hint) hint->type = EAH_NOHINT;

    if (tv->type == TOK_INVALID)
        token = scan(tv);
    else
        token = tv->type;

    asmexp_reset();

    e = aexp0(critical);
    if (!e) return NULL;

    /* strip far-absolute segment part */
    e = mul_vector_by_scalar(e, 1, FALSE);

    return e;
}

/****************************************************************************
 *                                                                          *
 * Function: aexp0                                                          *
 *                                                                          *
 * Purpose : Recursive-descent parser.                                      *
 *                                                                          *
 * Comment : Called with a single integer operand, which is 1 or 2 if the   *
 *           evaluation is critical (i.e. unresolved symbols are an error   *
 *           condition). Must update the global 'token' to reflect the      *
 *           token after the parsed string. May return NULL.                *
 *                                                                          *
 *           asmexp() should report its own errors: on return it is         *
 *           assumed that if NULL has been returned, the error has already  *
 *           been reported.                                                 *
 *                                                                          *
 *           Grammar parsed is:                                             *
 *                                                                          *
 *           expr  : aexp0                                                  *
 *           aexp0 : aexp1 [ {|} aexp1...]                                  *
 *           aexp1 : aexp2 [ {^} aexp2...]                                  *
 *           aexp2 : aexp3 [ {&} aexp3...]                                  *
 *           aexp3 : aexp4 [ {<<,>>} aexp4...]                              *
 *           aexp4 : aexp5 [ {+,-} aexp5...]                                *
 *           aexp5 : aexp6 [ {*,/,%,//,%%} aexp6...]                        *
 *           aexp6 : { ~,+,- } aexp6                                        *
 *                 | (aexp0)                                                *
 *                 | symbol                                                 *
 *                 | $                                                      *
 *                 | number                                                 *
 *                 | sizeof symbol                                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-05-01  Added support for SIZEOF <symbol> to inline asm.     *
 *           04-09-14  Bugfix: also accept struct . member in inline mode.  *
 *                                                                          *
 ****************************************************************************/

static EXPR *aexp0(int critical)
{
    EXPR *e;
    EXPR *f;

    e = aexp1(critical);
    if (!e) return NULL;

    while (token == '|')
    {
        token = scan(tv);
        f = aexp1(critical);
        if (!f) return NULL;

        if (!(is_simple(e) || is_just_unknown(e)) ||
            !(is_simple(f) || is_just_unknown(f)))
        {
            apperror(RCERROR(ERROR_OPERATOR_NEEDS_SCALAR), "|");
        }

        if (is_just_unknown(e) || is_just_unknown(f))
            e = unknown_expr();
        else
            e = scalar_expr(reloc_value(e) | reloc_value(f));
    }

    return e;
}

/****************************************************************************
 *                                                                          *
 * Subfunction: aexp1                                                       *
 *                                                                          *
 ****************************************************************************/

static EXPR *aexp1(int critical)
{
    EXPR *e;
    EXPR *f;

    e = aexp2(critical);
    if (!e) return NULL;

    while (token == '^')
    {
        token = scan(tv);
        f = aexp2(critical);
        if (!f) return NULL;

        if (!(is_simple(e) || is_just_unknown(e)) ||
            !(is_simple(f) || is_just_unknown(f)))
        {
            apperror(RCERROR(ERROR_OPERATOR_NEEDS_SCALAR), "^");
        }

        if (is_just_unknown(e) || is_just_unknown(f))
            e = unknown_expr();
        else
            e = scalar_expr(reloc_value(e) ^ reloc_value(f));
    }

    return e;
}

/****************************************************************************
 *                                                                          *
 * Subfunction: aexp2                                                       *
 *                                                                          *
 ****************************************************************************/

static EXPR *aexp2(int critical)
{
    EXPR *e;
    EXPR *f;

    e = aexp3(critical);
    if (!e) return NULL;

    while (token == '&')
    {
        token = scan(tv);
        f = aexp3(critical);
        if (!f) return NULL;

        if (!(is_simple(e) || is_just_unknown(e)) ||
            !(is_simple(f) || is_just_unknown(f)))
        {
            apperror(RCERROR(ERROR_OPERATOR_NEEDS_SCALAR), "&");
        }

        if (is_just_unknown(e) || is_just_unknown(f))
            e = unknown_expr();
        else
            e = scalar_expr(reloc_value(e) & reloc_value(f));
    }

    return e;
}

/****************************************************************************
 *                                                                          *
 * Subfunction: aexp3                                                       *
 *                                                                          *
 ****************************************************************************/

static EXPR *aexp3(int critical)
{
    EXPR *e;
    EXPR *f;

    e = aexp4(critical);
    if (!e) return NULL;

    while (token == TOK_SHL || token == TOK_SHR)
    {
        int token2 = token;

        token = scan(tv);
        f = aexp4(critical);
        if (!f) return NULL;

        if (!(is_simple(e) || is_just_unknown(e)) ||
            !(is_simple(f) || is_just_unknown(f)))
        {
            apperror(RCERROR(ERROR_OPERATOR_NEEDS_SCALAR), "shift");
        }
        else if (is_just_unknown(e) || is_just_unknown(f))
        {
            e = unknown_expr();
        }
        else switch (token2)
        {
            case TOK_SHL:
                e = scalar_expr(reloc_value(e) << reloc_value(f));
                break;

            case TOK_SHR:
                e = scalar_expr(((uintmax_t)reloc_value(e)) >> reloc_value(f));
                break;
        }
    }

    return e;
}

/****************************************************************************
 *                                                                          *
 * Subfunction: aexp4                                                       *
 *                                                                          *
 ****************************************************************************/

static EXPR *aexp4(int critical)
{
    EXPR *e;
    EXPR *f;

    e = aexp5(critical);
    if (!e) return NULL;

    while (token == '+' || token == '-')
    {
        int token2 = token;

        token = scan(tv);
        f = aexp5(critical);
        if (!f) return NULL;

        switch (token2)
        {
            case '+':
                e = add_vectors(e, f);
                break;

            case '-':
                e = add_vectors(e, mul_vector_by_scalar(f, -1, FALSE));
                break;
        }
    }

    return e;
}

/****************************************************************************
 *                                                                          *
 * Subfunction: aexp5                                                       *
 *                                                                          *
 ****************************************************************************/

static EXPR *aexp5(int critical)
{
    EXPR *e;
    EXPR *f;

    e = aexp6(critical);
    if (!e) return NULL;

    while (token == '*' || token == '/' || token == '%' || token == TOK_SDIV || token == TOK_SMOD)
    {
        int token2 = token;

        token = scan(tv);
        f = aexp6(critical);
        if (!f) return NULL;

        if (token2 != '*' && (!(is_simple(e) || is_just_unknown(e)) || !(is_simple(f) || is_just_unknown(f))))
        {
            apperror(RCERROR(ERROR_OPERATOR_NEEDS_SCALAR), "div");
            return NULL;
        }
        if (token2 != '*' && !is_unknown(f) && reloc_value(f) == 0)
        {
            apperror(RCERROR(ERROR_DIVISION_BY_ZERO));
            return NULL;
        }

        switch (token2)
        {
            case '*':
                if (is_simple(e))
                    e = mul_vector_by_scalar(f, reloc_value(e), TRUE);
                else if (is_simple(f))
                    e = mul_vector_by_scalar(e, reloc_value(f), TRUE);
                else if (is_just_unknown(e) && is_just_unknown(f))
                    e = unknown_expr();
                else
                {
                    apperror(RCERROR(ERROR_OPERATOR_NEEDS_SCALAR), "*");
                    return NULL;
                }
                break;

            case '/':  /* unsigned division (/) */
                if (is_just_unknown(e) || is_just_unknown(f))
                    e = unknown_expr();
                else
                    e = scalar_expr(((uintmax_t)reloc_value(e)) / ((uintmax_t)reloc_value(f)));
                break;

            case '%':  /* unsigned modulo (%) */
                if (is_just_unknown(e) || is_just_unknown(f))
                    e = unknown_expr();
                else
                    e = scalar_expr(((uintmax_t)reloc_value(e)) % ((uintmax_t)reloc_value(f)));
                break;

            case TOK_SDIV:  /* signed division (//) */
                if (is_just_unknown(e) || is_just_unknown(f))
                    e = unknown_expr();
                else
                    e = scalar_expr(((intmax_t)reloc_value(e)) / ((intmax_t)reloc_value(f)));
                break;

            case TOK_SMOD:  /* signed modulo (%%) */
                if (is_just_unknown(e) || is_just_unknown(f))
                    e = unknown_expr();
                else
                    e = scalar_expr(((intmax_t)reloc_value(e)) % ((intmax_t)reloc_value(f)));
                break;
        }
    }

    return e;
}

/****************************************************************************
 *                                                                          *
 * Subfunction: aexp6                                                       *
 *                                                                          *
 ****************************************************************************/

static EXPR *aexp6(int critical)
{
    bool_t sizeop = FALSE;
    long type;
    EXPR *e;

    if (token == '-')
    {
        token = scan(tv);
        e = aexp6(critical);
        if (!e) return NULL;

        return mul_vector_by_scalar(e, -1, FALSE);
    }
    else if (token == '+')
    {
        token = scan(tv);
        return aexp6(critical);
    }
    else if (token == '~')
    {
        token = scan(tv);
        e = aexp6(critical);
        if (!e) return NULL;

        if (is_just_unknown(e))
        {
            return unknown_expr();
        }
        else if (!is_simple(e))
        {
            apperror(RCERROR(ERROR_OPERATOR_NEEDS_SCALAR), "~");
            return NULL;
        }
        return scalar_expr(~reloc_value(e));
    }
    else if (token == '(')
    {
        token = scan(tv);
        e = aexp0(critical);
        if (!e) return NULL;

        if (token != ')')
        {
            apperror(RCERROR(ERROR_EXPECTING_RPAREN));
            return NULL;
        }
        token = scan(tv);
        return e;
    }
    else if (token == TOK_NUM || token == TOK_REG || token == TOK_ID || token == TOK_HERE || token == TOK_BASE || token == TOK_SIZEOF)
    {
        reset_stack();
        switch (token)
        {
            case TOK_NUM:
                push_expr(EXPR_SIMPLE, tv->integer, NULL);
                break;

            case TOK_REG:
                push_expr((long)tv->integer, 1, NULL);
                if (hint && hint->type == EAH_NOHINT)
                    hint->base = (long)tv->integer, hint->type = EAH_MAKEBASE;
                break;

            case TOK_SIZEOF:
                token = scan(tv);
                if (!inlined || token != TOK_ID)
                {
                    apperror(RCERROR(ERROR_EXPR_SYNTAX_ERROR));
                    return NULL;
                }
                sizeop = TRUE;
                /* fall through */
            case TOK_ID:
            case TOK_BASE:
            case TOK_HERE:
                if (inlined)  /* C inline assembler */
                {
                    char *p, *name;
                    SYMBOL *sym;

                    if (token == TOK_BASE || token == TOK_HERE)
                    {
                        apperror(RCERROR(ERROR_UNDEFINED_SYMBOL), tv->charptr);
                        return NULL;
                    }

                    /* Look for a structure reference */
                    p = strchr(tv->charptr, '.');
                    if (p)
                    {
                        /* struct.member */
                        name = stringn(tv->charptr, p - tv->charptr);
                        p++;
                    }
                    else
                    {
                        /* maybe struct . member? */
                        const char *save = as.scanner.getptr();
                        static TOKENVAL tokval;
                        if (scan(&tokval) == TOK_ID && strcmp(tokval.charptr, ".") == 0)
                        {
                            name = string(tv->charptr);
                            p = (token = scan(tv)) == TOK_ID ? p = tv->charptr : "";
                        }
                        else
                        {
                            as.scanner.setptr(save);
                            name = NULL;
                        }
                    }
                    if (name)
                    {
                        FIELD *field;

                        sym = lookup_symbol(name, identifiers);
                        if (!sym || !sym->type || !isstruct(sym->type))
                        {
                            apperror(RCERROR(ERROR_UNDEFINED_STRUCTURE), name);
                            return NULL;
                        }

                        name = string(p);
                        if ((field = struct_field_reference(name, sym->type)) == NULL)
                        {
                            apperror(RCERROR(ERROR_UNKNOWN_FIELD), name, sym->type);
                            return NULL;
                        }

                        if (sizeop)  /* sizeof symbol */
                            push_expr(EXPR_SIMPLE, field->type ? field->type->size : 0, NULL);
                        else
                            push_expr(EXPR_SIMPLE, field->xoffset, sym);
                    }
                    else  /* normal symbol reference */
                    {
                        name = string(tv->charptr);
                        if (sizeop)  /* sizeof symbol */
                        {
                            sym = lookup_symbol(name, types);
                            if (sym == NULL)
                                sym = lookup_symbol(name, identifiers);
                            if (sym == NULL)
                            {
                                apperror(RCERROR(ERROR_EXPR_SYNTAX_ERROR));
                                return NULL;
                            }
                            push_expr(EXPR_SIMPLE, sym->type ? sym->type->size : 0, NULL);
                            break;
                        }
                        else
                        {
                            sym = lookup_symbol(name, identifiers);
                            if (sym == NULL)
                            {
                                sym = lookup_symbol(name, stmtlabs);
                                if (sym == NULL)
                                {
                                    sym = install_symbol(name, &stmtlabs, 0, funca);
                                    sym->scope = LABELS;
                                    sym->u.lab.label = make_label(1);
                                    sym->src = src;
                                }
                                sym = find_label(sym->u.lab.label);
                            }
                            push_expr(EXPR_SIMPLE, 0, sym);
                        }
                    }
                }
                else  /* NOT C inline assembler */
                {
                    LABELDEF *labdef = NULL;
                    long segment, offset;

                    if (token == TOK_BASE)
                    {
                        type = EXPR_SIMPLE;
                        segment = location.segment;
                        offset = 0;
                    }
                    else if (token == TOK_HERE)
                    {
                        type = EXPR_SIMPLE;
                        segment = location.segment;
                        offset = location.offset;
                    }
                    else
                    {
                        type = EXPR_SIMPLE;
                        if (!asmlab_lookup(tv->charptr, &segment, &offset, &labdef))
                        {
                            if (critical == 2)
                            {
                                apperror(RCERROR(ERROR_UNDEFINED_SYMBOL), tv->charptr);
                                return NULL;
                            }
                            else if (critical == 1)
                            {
                                apperror(RCERROR(ERROR_UNDEFINED_BEFORE_USE), tv->charptr);
                                return NULL;
                            }
                            else
                            {
                                if (opflags) *opflags |= OPFLAG_FORWARD;
                                type = EXPR_UNKNOWN;
                                segment = NO_SEG;
                                offset = 1;
                            }
                        }
                        else
                        {
                            labdef->referenced = TRUE;
                        }
                    }

                    if (opflags && token == TOK_ID && asmlab_is_extern(tv->charptr))
                        *opflags |= OPFLAG_EXTERN;

                    push_expr(type, offset, labdef);
                    if (segment != NO_SEG)
                        push_expr(EXPR_SEGBASE + segment, 1, NULL);
                }
                break;
        }

        token = scan(tv);
        return finalize_stack();
    }
    else
    {
        apperror(RCERROR(ERROR_EXPR_SYNTAX_ERROR));
        return NULL;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: mul_vector_by_scalar                                           *
 *                                                                          *
 * Purpose : Multiply a vector by a scalar. Strip far-absolute segment      *
 *           part if present.                                               *
 *                                                                          *
 * Comment : Explicit treatment of UNKNOWN is not required in this routine, *
 *           since it will silently do the Right Thing anyway.              *
 *                                                                          *
 *           If 'affect_hints' is set, we also change the hint type to      *
 *           NOTBASE if a MAKEBASE hint points at a register being          *
 *           multiplied. This allows [eax*1+ebx] to hint EBX rather than    *
 *           EAX as the base register.                                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static EXPR *mul_vector_by_scalar(EXPR *vect, intmax_t scalar, bool_t affect_hints)
{
    EXPR *e = vect;

    while (e->type && e->type < EXPR_SEGBASE+SEG_ABS)
    {
        e->value *= scalar;

        if (hint && hint->type == EAH_MAKEBASE &&
            e->type == hint->base && affect_hints)
            hint->type = EAH_NOTBASE;

        e++;
    }

    e->type = 0;

    return vect;
}

/****************************************************************************
 *                                                                          *
 * Function: add_vectors                                                    *
 *                                                                          *
 * Purpose : Add two vector datatypes. We have some bizarre behaviour on    *
 *           far-absolute segment types: we preserve them during addition   *
 *           *only* if one of the segments is a truly pure scalar.          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static EXPR *add_vectors(EXPR *e, EXPR *f)
{
    bool_t preserve;

    preserve = is_really_simple(e) || is_really_simple(f);

    reset_stack();

    while (e->type && f->type &&
           e->type < EXPR_SEGBASE+SEG_ABS &&
           f->type < EXPR_SEGBASE+SEG_ABS)
    {
        int lasttype;

        if (e->type > f->type)
        {
            push_expr(f->type, f->value, f->vp);
            lasttype = f->type;
            f++;
        }
        else if (e->type < f->type)
        {
            push_expr(e->type, e->value, e->vp);
            lasttype = e->type;
            e++;
        }
        else  /* same type */
        {
            /*
             * Try to keep these cases straight:
             *
             * 1. label1 + label2 = sum
             * 2. label1(sym) + label2 = sym + offset
             * 3. label1 + label2(sym) = sym + offset
             * 4. label1(sym1) + label2(sym2) = sum (i.e. nuke sym1 & sym2)
             */
            intmax_t sum = e->value + f->value;
            void *vp = (e->vp) ? (f->vp) ? NULL : e->vp : f->vp;
            if (sum) push_expr(e->type, sum, vp);
            lasttype = e->type;
            e++, f++;
        }

        if (lasttype == EXPR_UNKNOWN)
            return finalize_stack();
    }

    while (e->type && (preserve || e->type < EXPR_SEGBASE+SEG_ABS))
    {
        push_expr(e->type, e->value, e->vp);
        e++;
    }

    while (f->type && (preserve || f->type < EXPR_SEGBASE+SEG_ABS))
    {
        push_expr(f->type, f->value, f->vp);
        f++;
    }

    return finalize_stack();
}

/****************************************************************************
 *                                                                          *
 * Function: unknown_expr                                                   *
 *                                                                          *
 * Purpose : Generate a unknown expression (forward reference).             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static EXPR *unknown_expr(void)
{
    reset_stack();
    push_expr(EXPR_UNKNOWN, 1, NULL);
    return finalize_stack();
}

/****************************************************************************
 *                                                                          *
 * Function: scalar_expr                                                    *
 *                                                                          *
 * Purpose : Generate a simple scalar expression.                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static EXPR *scalar_expr(intmax_t scalar)
{
    reset_stack();
    push_expr(EXPR_SIMPLE, scalar, NULL);
    return finalize_stack();
}

/****************************************************************************
 *                                                                          *
 * Function: reset_stack                                                    *
 *                                                                          *
 * Purpose : Clear the temporary expression stack.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void reset_stack(void)
{
    stack_tempexpr = NULL;
    stack_maxcount = stack_count = 0;
}

/****************************************************************************
 *                                                                          *
 * Function: push_expr                                                      *
 *                                                                          *
 * Purpose : Push another entry onto the temporary expression stack.        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void push_expr(long type, intmax_t value, void *vp)
{
    if (stack_count == stack_maxcount)
    {
        stack_maxcount += STACK_INCREMENT;
        stack_tempexpr = my_realloc(stack_tempexpr, stack_maxcount * sizeof(*stack_tempexpr));
    }

    stack_tempexpr[stack_count].type = type;
    stack_tempexpr[stack_count].value = value;
    stack_tempexpr[stack_count++].vp = vp;
}

/****************************************************************************
 *                                                                          *
 * Function: finalize_stack                                                 *
 *                                                                          *
 * Purpose : Move expression from temporary stack to another stack(!)       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static EXPR *finalize_stack(void)
{
    push_expr(0, 0, NULL);  /* terminate */

    if (exprs_count == exprs_maxcount)
    {
        exprs_maxcount += EXPRS_INCREMENT;
        exprs = my_realloc(exprs, exprs_maxcount * sizeof(*exprs));
    }

    return exprs[exprs_count++] = stack_tempexpr;
}

/****************************************************************************
 *                                                                          *
 * Function: asmexp_reset                                                   *
 *                                                                          *
 * Purpose : Reset the temporary stack by popping all entries.              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void asmexp_reset(void)
{
    while (exprs_count != 0)
        my_free(exprs[--exprs_count]);
}

/****************************************************************************
 *                                                                          *
 * Function: asmexp_cleanup                                                 *
 *                                                                          *
 * Purpose : Unimportant cleanup is done to avoid confusing people who      *
 *           are trying to debug real memory leaks.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void asmexp_cleanup(void)
{
    asmexp_reset();
    my_free(exprs);
    exprs = NULL;
    exprs_maxcount = 0;
}

