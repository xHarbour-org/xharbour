/****************************************************************************
 *                                                                          *
 * File    : expr.c                                                         *
 *                                                                          *
 * Purpose : ISO C Compiler; Expression parsing.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           03-09-08  Added compound_literal() and memset_tree().          *
 *           04-12-05  New initializer handling: above changes removed.     *
 *           05-01-02  Added optimized_field_tree().                        *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "lcc.h"

static char prec[] = {
#define xx(a,b,c,d,e,f,g) c,
#define yy(a,b,c,d,e,f,g) c,
#include "token.h"
};

static int oper[] = {
#define xx(a,b,c,d,e,f,g) d,
#define yy(a,b,c,d,e,f,g) d,
#include "token.h"
};

float refinc = 1.0;

/* Static function prototypes */
static TREE *expr2(void);
static TREE *expr3(int);
static TREE *unary(void);
static TREE *postfix(TREE *);
static TREE *primary(void);
static TREE *optimized_field_tree(TREE *, const char *);
static TYPE *supertype(TYPE *);
static TREE *rightkid(TREE *);

/****************************************************************************
 *                                                                          *
 * Function: intexpr                                                        *
 *                                                                          *
 * Purpose : Parse a constant expression and return int value, default n.   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

int intexpr(int token, int n)
{
    TREE *e = constexpr(token);

    if (e->op == CNST+I || e->op == CNST+U)
    {
        need_const++;
        n = (int)cast(e, inttype)->u.v.i;
        need_const--;
    }
    else
    {
        apperror(RCERROR(ERROR_INT_EXPR_MUST_BE_CONST));
    }

    return n;
}

/****************************************************************************
 *                                                                          *
 * Function: constexpr                                                      *
 *                                                                          *
 * Purpose : Parse a constant expression.                                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

TREE *constexpr(int token)
{
    TREE *e;

    need_const++;
    e = expr1(token);
    need_const--;

    return e;
}

/****************************************************************************
 *                                                                          *
 * Function: expr0                                                          *
 *                                                                          *
 * Purpose : Parse expressions for side effects.                            *
 *                                                                          *
 * Comment : Used for statement-level expressions, such as assignments      *
 *           and function calls.                                            *
 *                                                                          *
 *           For example, the statement a + f() includes a useless          *
 *           addition, which the compiler is free to eliminate (even        *
 *           if the addition would overflow). Given the tree for this       *
 *           expression, root returns the tree for f().                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

TREE *expr0(int token)
{
    return root(expr(token));
}

/****************************************************************************
 *                                                                          *
 * Function: expr                                                           *
 *                                                                          *
 * Purpose : Parse expressions.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

TREE *expr(int token)
{
    TREE *e = expr1(0);

    /* { , assignment-expression } */
    while (tok == ',')
    {
        TREE *p;

        tok = gettok();

        p = pointer(expr1(0));
        e = new_tree(RIGHT, p->type, root(value(e)), p);
    }

    if (token)
    {
        static char stop[] = { IF, ID, '}', 0 };
        follow(token, stop);
    }

    return e;
}

/****************************************************************************
 *                                                                          *
 * Function: expr1                                                          *
 *                                                                          *
 * Purpose : Parse assignment expressions.                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

TREE *expr1(int token)
{
    TREE *e = expr2();

    if (tok == '=' ||
        (prec[tok] >=  6 && prec[tok] <=  8) ||
        (prec[tok] >= 11 && prec[tok] <= 13))
    {
        int op = tok;

        tok = gettok();

        if (oper[op] == ASGN)
        {
            e = assignment_tree(ASGN, e, value(expr1(0)));
        }
        else
        {
            expect('=');
            e = increment_tree(op, e, expr1(0));
        }
    }

    if (token)
    {
        static char stop[] = { IF, ID, 0 };
        follow(token, stop);
    }

    return e;
}

/****************************************************************************
 *                                                                          *
 * Function: expr2                                                          *
 *                                                                          *
 * Purpose : Parse conditional expressions (e1 ? e2 : e3).                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static TREE *expr2(void)
{
    TREE *e = expr3(4);

    if (tok == '?')
    {
        COORDINATE pts[2];
        TREE *l;
        TREE *r;

        if (isfunc(e->type))
            apperror(RCWARNING2(ERROR_USED_IN_COND_EXPRESSION), funcname(e));

        e = pointer(e);

        tok = gettok();

        pts[0] = src;
        l = pointer(expr(':'));

        pts[1] = src;
        r = pointer(expr2());

#ifdef PROF
        if (generic(p->op) != CNST && events.points)
        {
            apply(events.points, &pts[0], &l);
            apply(events.points, &pts[1], &r);
        }
#endif

        e = condexpr_tree(e, l, r);
    }

    return e;
}

/****************************************************************************
 *                                                                          *
 * Function: expr3                                                          *
 *                                                                          *
 * Purpose : Parse expressions at precedence level k.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static TREE *expr3(int k)
{
    TREE *e = unary();
    int k1;

    for (k1 = prec[tok]; k1 >= k; k1--)
    {
        while (prec[tok] == k1 && *cp != '=')
        {
            COORDINATE pt;
            TREE *r;
            int op = tok;

            tok = gettok();
            pt = src;

            e = pointer(e);

            if (op == ANDAND || op == OROR)
            {
                r = pointer(expr3(k1));

#ifdef PROF
                if (events.points)
                    apply(events.points, &pt, &r);
#endif
            }
            else
            {
                r = pointer(expr3(k1+1));
            }

            e = (*optree[op])(oper[op], e, r);
        }
    }

    return e;
}

/****************************************************************************
 *                                                                          *
 * Function: unary                                                          *
 *                                                                          *
 * Purpose : Parse an unary expression.                                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           03-08-14  Support for variable-length arrays added.            *
 *           03-09-08  Support for compound literals added.                 *
 *           04-12-04  compound_literal() changed to local_initializer().   *
 *                                                                          *
 ****************************************************************************/

static TREE *unary(void)
{
    TREE *e;

    switch (tok)
    {
        case '*':
            tok = gettok();
            e = unary();
            e = pointer(e);
            if (isptr(e->type) && (isfunc(e->type->type) || isarray(e->type->type)))
                e = retype(e, e->type->type);
            else
                e = rvalue(e);
            break;

        case '&':
            tok = gettok();
            e = unary();

            if (is_compound_literal(e))
            {
                TREE *q = lvalue(rightkid(e));
                e = new_tree(RIGHT, q->type, e->kids[0], q);
            }
            else if (isarray(e->type) || isfunc(e->type))
                e = retype(e, ptr(e->type));
            else
                e = lvalue(e);

            if (isaddrop(e->op) && e->u.sym->sclass == REGISTER)
                apperror(RCERROR(ERROR_INVALID_UNARY_OPERAND), e->u.sym->name);
            else if (isaddrop(e->op))
                e->u.sym->addressed = TRUE;
            break;

        case '+':
            tok = gettok();
            e = unary();
            e = pointer(e);
            if (isarith(e->type))
                e = cast(e, promote_type(e->type));
            else
                typeerror(ADD, e, NULL);
            break;

        case '-':
            tok = gettok();
            e = unary();
            e = pointer(e);
            if (isarith(e->type))
            {
                TYPE *ty = promote_type(e->type);
                e = cast(e, ty);
                if (isunsigned(ty))
                {
                    apperror(RCWARNING2(ERROR_UNSIGNED_NEGATION));
                    e = simplify(ADD, ty, simplify(BCOM, ty, e, NULL), cnst_tree(ty, (uintmax_t)1));
                }
                else
                {
                    e = simplify(NEG, ty, e, NULL);
                }
            }
            else
            {
                typeerror(SUB, e, NULL);
            }
            break;

        case '~':
            tok = gettok();
            e = unary();
            e = pointer(e);
            if (isint(e->type))
            {
                TYPE *ty = promote_type(e->type);
                e = simplify(BCOM, ty, cast(e, ty), NULL);
            }
            else
            {
                typeerror(BCOM, e, NULL);
            }
            break;

        case '!':
            tok = gettok();
            e = unary();
            e = pointer(e);
            /*
             * From C99: "The result of the logical negation operator ! is 0 if the value
             * of its operand compares unequal to 0, 1 if the value of its operand compares
             * equal to 0. THE RESULT HAS TYPE INT (not _Bool, my remark)."
             */
            if (isscalar(e->type))
                e = simplify(NOT, inttype, cond(e), NULL);
            else
                typeerror(NOT, e, NULL);
            break;

        case INCR:
            tok = gettok();
            e = unary();
            e = increment_tree(INCR, pointer(e), const_tree(1, inttype));
            break;

        case DECR:
            tok = gettok();
            e = unary();
            e = increment_tree(DECR, pointer(e), const_tree(1, inttype));
            break;

        case SIZEOF:
        {
            TYPE *ty;

            e = NULL;

            tok = gettok();
            if (tok == '(')
            {
                tok = gettok();
                if (istypename(tok, toksym))
                {
                    ty = parse_typename();
                    expect(')');
                }
                else
                {
                    e = postfix(expr(')'));
                    ty = e->type;
                }
            }
            else
            {
                e = unary();
                ty = e->type;
            }

            assert(ty);

            if (isvla(ty))
            {
                /* calculate sizeof variable-length array at runtime */
                e = cast(vlasize_tree(ty, 0), unsignedlongtype);
            }
            else
            {
                if (isfunc(ty) || ty->size == 0)
                    apperror(RCERROR(ERROR_INVALID_SIZEOF_TYPE), ty);
                else if (e && rightkid(e)->op == AFIELD)
                    apperror(RCERROR(ERROR_SIZEOF_ON_BIT_FIELD));

                e = cnst_tree(unsignedlongtype, (uintmax_t)ty->size);
            }
            break;
        }

        case '(':
            tok = gettok();
            if (istypename(tok, toksym))
            {
                TYPE *ty1 = parse_typename();

                expect(')');

                if (tok == '{')  /* compound literal: (cast){init} */
                {
                    SYMBOL *sym = make_temp_ident(scope == GLOBAL ? STATIC : AUTO, ty1);
                    if (isvla(ty1)) apperror(RCERROR(ERROR_ILLEGAL_VLA_USAGE));
                    e = new_tree(RIGHT, ty1, local_initializer(sym, ty1), id_tree(sym));
                }
                else  /* cast */
                {
                    TYPE *ty;
                    TYPE *pty;

                    ty = unqual(ty1);
                    if (isenum(ty))
                    {
                        TYPE *ty2 = ty->type;

                        if (isconst(ty1))
                            ty2 = qual(CONST_, ty2);
                        if (isvolatile(ty1))
                            ty2 = qual(VOLATILE_, ty2);

                        ty1 = ty2;
                        ty = ty->type;
                    }

                    e = pointer(unary());
                    pty = e->type;

                    if (isenum(pty))
                        pty = pty->type;

                    if (isarith(pty) && isarith(ty) || isptr(pty) && isptr(ty))
                    {
                        explicit_cast++;
                        e = cast(e, ty);
                        explicit_cast--;
                    }
                    else if (isptr(pty) && isint(ty) || isint(pty) && isptr(ty))
                    {
                        if (ty->size < pty->size)
                            apperror(RCWARNING1(ERROR_NON_PORTABLE_CONVERSION), e->type, ty);
                        e = cast(e, ty);
                    }
                    else if (ty != voidtype)
                    {
                        apperror(RCERROR(ERROR_ILLEGAL_CAST), e->type, ty1);
                        ty1 = inttype;
                    }

                    if (generic(e->op) == INDIR || ty->size == 0)
                        e = new_tree(RIGHT, ty1, NULL, e);
                    else
                        e = retype(e, ty1);
                }
            }
            else
            {
                e = postfix(expr(')'));
            }
            break;

        default:
            e = postfix(primary());
            break;
    }

    return e;
}

/****************************************************************************
 *                                                                          *
 * Function: postfix                                                        *
 *                                                                          *
 * Purpose : Parse a postfix expression; e is the primary dag.              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           05-01-02  Added support for small structure optimization.      *
 *                                                                          *
 ****************************************************************************/

static TREE *postfix(TREE *e)
{
    for (;;)
    {
        switch (tok)
        {
            case INCR:
                e = new_tree(RIGHT, e->type, new_tree(RIGHT, e->type, e,
                    increment_tree(tok, e, const_tree(1, inttype))), e);
                tok = gettok();
                break;

            case DECR:
                e = new_tree(RIGHT, e->type, new_tree(RIGHT, e->type, e,
                    increment_tree(tok, e, const_tree(1, inttype))), e);
                tok = gettok();
                break;

            case '[':
            {
                TREE *q;

                tok = gettok();
                q = expr(']');
                e = (*optree['+'])(ADD, pointer(e), pointer(q));
                if (isptr(e->type) && isarray(e->type->type))
                    e = retype(e, e->type->type);
                else
                    e = rvalue(e);
                break;
            }

            case '(':
            {
                COORDINATE pt;
                TYPE *ty;

                e = pointer(e);
                if (isptr(e->type) && isfunc(e->type->type))
                    ty = e->type->type;
                else
                {
                    apperror(RCERROR(ERROR_EXPECTING_FUNCTION), e->type);
                    ty = func(voidtype, NULL, NULL, TRUE, CDECL_);
                    e = retype(e, ptr(ty));
                }

                pt = src;
                tok = gettok();

                e = call(e, ty, pt);
                break;
            }

            case '.':
                tok = gettok();
                if (tok == ID)
                {
                    if (isstruct(e->type))
                    {
                        TREE *q;

                        if ((q = optimized_field_tree(e, tokstr)) != NULL)
                        {
                            /* optimized access */
                            e = q;
                        }
                        else
                        {
                            if (e->op == CALL+B)
                            {
                                /* need adressable object, or addrof() will barf */
                                SYMBOL *symT = make_temp_ident(AUTO, unqual(e->type));
                                e = assignment(symT, e);
                                e = new_tree(RIGHT, symT->type, root(e), id_tree(symT));
                            }
                            else if (e->op == ASGN+B)
                            {
                                /* need adressable object, or addrof() will barf */
                                if (isaddrop(e->kids[0]->op))
                                    e = new_tree(RIGHT, e->type, e, id_tree(e->kids[0]->u.sym));
                            }

                            q = addrof(e);
                            e = field_tree(q, tokstr);
                            q = rightkid(q);
                            if (isaddrop(q->op) && q->u.sym->temporary)
                                e = new_tree(RIGHT, e->type, e, NULL);
                        }
                    }
                    else
                    {
                        apperror(RCERROR(ERROR_BAD_LEFT_DOT_TYPE), e->type);
                    }
                    tok = gettok();
                }
                else
                {
                    apperror(RCERROR(ERROR_EXPECTING_FIELD_NAME));
                }
                break;

            case DEREF:
                tok = gettok();
                e = pointer(e);
                if (tok == ID)
                {
                    if (isptr(e->type) && isstruct(e->type->type))
                        e = field_tree(e, tokstr);
                    else
                        apperror(RCERROR(ERROR_BAD_LEFT_DEREF_TYPE), e->type);
                    tok = gettok();
                }
                else
                {
                    apperror(RCERROR(ERROR_EXPECTING_FIELD_NAME));
                }
                break;

            default:
                return e;
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: primary                                                        *
 *                                                                          *
 * Purpose : Parse a primary expression.                                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           03-10-28  Support for optional arguments added.                *
 *           04-12-09  Cache wide strings too - to avoid duplicates.        *
 *           04-12-29  Replace const variables with literal value.          *
 *                                                                          *
 ****************************************************************************/

static TREE *primary(void)
{
    TREE *e;

    assert(tok != '(');

    switch (tok)
    {
        case ICONST:
        case FCONST:
            e = new_tree(mkop(CNST,toksym->type), toksym->type, NULL, NULL);
            e->u.v = toksym->u.c.v;
            break;

        case SCONST:
            if (ischar(toksym->type->type))
                toksym->u.c.v.p = stringn(toksym->u.c.v.p, toksym->type->size);
            else
                toksym->u.c.v.p = wstringn(toksym->u.c.v.p, toksym->type->size / widechartype->size);
                /*
                toksym->u.c.v.p = memcpy(memalloc((toksym->type->size / widechartype->size) * sizeof(int), PERM),
                    toksym->u.c.v.p, (toksym->type->size / widechartype->size) * sizeof(int));
                */

            toksym = constant(toksym->type, toksym->u.c.v);

            if (toksym->u.c.loc == NULL)
                toksym->u.c.loc = make_ident(STATIC, toksym->type, GLOBAL);

            e = id_tree(toksym->u.c.loc);
            toksym->ref++;
            break;

        case ID:
            if (toksym == NULL)
            {
                SYMBOL *sym = install_symbol(tokstr, &identifiers, scope, PERM);

                sym->src = src;
                if (getchr() == '(')
                {
                    SYMBOL *sym2 = lookup_symbol(tokstr, externals);

                    sym->type = func(inttype, NULL, NULL, TRUE, CDECL_);
                    sym->sclass = EXTERN;

                    /*
                     * C99 no longer has a rule for implicit declaration of functions.
                     * We issue a strong warning, but continue in order to support old code.
                     */
                    apperror(optparam ? RCERROR(ERROR_MISSING_PROTOTYPE_FOR) : RCWARNING1(ERROR_MISSING_PROTOTYPE_FOR), tokstr);

                    if (sym2 && !is_same_type(sym2->type, sym->type, TRUE))
                        apperror(RCWARNING1(ERROR_IMPLICIT_DECL_MISMATCH), sym2->name, &sym2->src);

                    if (sym2 == NULL)
                    {
                        sym2 = install_symbol(sym->name, &externals, GLOBAL, PERM);
                        sym2->type = sym->type;
                        sym2->sclass = EXTERN;
                        sym2->src = src;

                        (*IR->defsymbol)(sym2);
                    }

                    sym->u.alias = sym2;
                }
                else
                {
                    apperror(RCERROR(ERROR_UNDECLARED_IDENT), sym->name);

                    sym->sclass = AUTO;
                    sym->type = inttype;

                    if (sym->scope == GLOBAL)
                        (*IR->defsymbol)(sym);
                    else
                        new_local_var(sym);
                }

                tok = gettok();
#ifdef XREF
                if (options.xreflevel > 1)
                    use_symbol(sym, src);
#endif
                return id_tree(sym);
            }
#ifdef XREF
            if (options.xreflevel > 1)
                use_symbol(toksym, src);
#endif
            if (toksym->sclass == ENUM)
            {
                e = const_tree(toksym->u.value, inttype);
            }
            else
            {
                if (toksym->sclass == TYPEDEF)
                    apperror(RCERROR(ERROR_ILLEGAL_USE_OF_TYPENAME), toksym->name);

                if (toksym->u.seg == LIT && isscalar(toksym->type) && toksym->e)
                    e = toksym->e, toksym->ref += refinc;  /* replace with literal, avoid unused warning */
                else
                    e = id_tree(toksym);
            }
            break;

        default:
            apperror(RCERROR(ERROR_ILLEGAL_EXPRESSION));
            e = cnst_tree(inttype, (intmax_t)0);
            break;
    }

    tok = gettok();
    return e;
}

/****************************************************************************
 *                                                                          *
 * Function: is_compound_literal                                            *
 *                                                                          *
 * Purpose : Check if expression contains a compound literal.               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           03-09-08  Created                                              *
 *                                                                          *
 ****************************************************************************/

bool_t is_compound_literal(TREE *e)
{
    TREE *q;
    return e->op == RIGHT && (q = rightkid(e)) != 0 &&
        (generic(q->op) == INDIR || generic(q->op) == ADDRG) &&
        e->type == pointer(q)->type;
}

/****************************************************************************
 *                                                                          *
 * Function: field_tree                                                     *
 *                                                                          *
 * Purpose : Construct tree for reference to field name via e.              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

TREE *field_tree(TREE *e, const char *name)
{
    FIELD *field;
    TYPE *ty = e->type;
    TYPE *ty1;

    if (isptr(ty))
        ty = dereference_type(ty);

    ty1 = ty;
    ty = unqual(ty);

    if ((field = struct_field_reference(name, ty)) != NULL)
    {
        if (isarray(field->type))
        {
            ty = field->type->type;

            if (isconst(ty1) && !isconst(ty))
                ty = qual(CONST_, ty);
            if (isvolatile(ty1) && !isvolatile(ty))
                ty = qual(VOLATILE_, ty);
            if (isrestrict(ty1) && !isrestrict(ty))
                ty = qual(RESTRICT_, ty);

            ty = new_array(ty, field->type->size/ty->size, field->type->align);
        }
        else
        {
            ty = field->type;

            if (isconst(ty1) && !isconst(ty))
                ty = qual(CONST_, ty);
            if (isvolatile(ty1) && !isvolatile(ty))
                ty = qual(VOLATILE_, ty);
            if (isrestrict(ty1) && !isrestrict(ty))
                ty = qual(RESTRICT_, ty);

            ty = ptr(ty);
        }

        e = simplify(ADD+P, ty, e, const_tree(field->xoffset, signedptrtype /*inttype*/));

        if (field->lsb)
        {
            e = new_tree(AFIELD, ty->type, rvalue(e), NULL);
            e->u.field = field;
        }
        else if (!isarray(field->type))
        {
            e = rvalue(e);
        }
    }
    else
    {
        apperror(RCERROR(ERROR_UNKNOWN_FIELD), name, ty);
        e = rvalue(retype(e, ptr(inttype)));
    }

    return e;
}

/****************************************************************************
 *                                                                          *
 * Function: optimized_field_tree                                           *
 *                                                                          *
 * Purpose : Construct optimized tree for reference to field name via e.    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           05-01-02  Created                                              *
 *                                                                          *
 ****************************************************************************/

static TREE *optimized_field_tree(TREE *e, const char *name)
{
    TYPE *sty;

    /* only works for small structures */
    if ((sty = optimized_struct_type(e->type)) != NULL)
    {
        FIELD *field;

        /* if found, not a bit-field, scalar, and small shift amount */
        if ((field = struct_field_reference(name, e->type)) != NULL && !field->lsb &&
            isscalar(field->type) && field->xoffset <= inttype->size)
        {
            if (e->op == INDIR+B)
            {
                /* use regular case - it's optimal */
                return NULL;
            }
            else if (e->op == CALL+B)
            {
                e = optimized_struct_tree(e);
            }
            else if (e->op == ASGN+B)
            {
                e = optimized_struct_tree(e);
                e = new_tree(RIGHT, sty, e, rvalue(e->kids[0]));
            }
            else if (e->op == RIGHT)
            {
                e = optimized_struct_tree(root(e));
                e = retype(e, sty);
            }
            else assert(0);

            /* shift field into position, cast to proper type */
            assert(field->xoffset < sty->size);
            e = simplify(RSH, sty, e, const_tree(field->xoffset*8, inttype));
            e = cast(e, field->type);
            return e;
        }
    }

    /* use regular case */
    return NULL;
}

/****************************************************************************
 *                                                                          *
 * Function: increment_tree                                                 *
 *                                                                          *
 * Purpose : Construct tree for e1 op= e2.                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

TREE *increment_tree(int op, TREE *v, TREE *e)
{
    return assignment_tree(ASGN, v, (*optree[op])(oper[op], v, e));
}

/****************************************************************************
 *                                                                          *
 * Function: id_tree                                                        *
 *                                                                          *
 * Purpose : Construct tree for reference to r-value of identifier sym.     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           03-08-14  Support for variable-length arrays added.            *
 *           03-08-27  Stupid non-ref-counting removed for inline funcs.    *
 *           04-11-25  Call to inline_symbol_ref() added.                   *
 *                                                                          *
 ****************************************************************************/

TREE *id_tree(SYMBOL *sym)
{
    TYPE *ty;
    TREE *e;
    int op;

    ty = (sym->type) ? unqual(sym->type) : voidptype;

    if (sym->scope == GLOBAL || sym->sclass == STATIC)
    {
        op = ADDRG;
    }
    else if (sym->scope == PARAM)
    {
        op = ADDRF;

        if (isstruct(sym->type) && !IR->wants_argb)
        {
            e = new_tree(mkop(op,voidptype), ptr(ptr(sym->type)), NULL, NULL);
            e->u.sym = sym;
            return rvalue(rvalue(e));
        }
    }
    else if (sym->sclass == EXTERN)
    {
        assert(sym->u.alias);
        sym = sym->u.alias;

        op = ADDRG;
    }
    else
    {
        op = ADDRL;
    }

    sym->ref += refinc;

    /* remember increments for inline symbols */
    if (funcsym && funcsym->attr.inlined && (sym->scope == GLOBAL || sym->sclass == EXTERN))
        inline_symbol_ref(sym);

    if (op == ADDRL && isvla(sym->type))
    {
        e = new_tree(mkop(op,voidptype), ptr(sym->type), NULL, NULL);
        e->u.sym = sym;
        return rvalue(e);
    }
    else if (op == ADDRG && sym->attr.dllimport)
    {
        if (isarray(ty))
        {
            e = new_tree(mkop(op,voidptype), ptr(sym->type), NULL, NULL);
            e->u.sym = sym;
            return rvalue(e);
        }
        else if (isfunc(ty))
        {
            e = new_tree(mkop(op,funcptype), ptr(sym->type), NULL, NULL);
            e->u.sym = sym;
            return rvalue(e);
        }
        else
        {
            e = new_tree(mkop(op,voidptype), ptr(ptr(sym->type)), NULL, NULL);
            e->u.sym = sym;
            return rvalue(rvalue(e));
        }
    }

    if (isarray(ty))
        e = new_tree(mkop(op,voidptype), sym->type, NULL, NULL);
    else if (isfunc(ty))
        e = new_tree(mkop(op,funcptype), sym->type, NULL, NULL);
    else
        e = new_tree(mkop(op,voidptype), ptr(sym->type), NULL, NULL);

    e->u.sym = sym;

    if (isptr(e->type))
        e = rvalue(e);

    return e;
}

/****************************************************************************
 *                                                                          *
 * Function: rvalue                                                         *
 *                                                                          *
 * Purpose : Convert e to an rvalue.                                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

TREE *rvalue(TREE *e)
{
    TYPE *ty = dereference_type(e->type);

    ty = unqual(ty);

    return new_tree(mkop(INDIR,ty), ty, e, NULL);
}

/****************************************************************************
 *                                                                          *
 * Function: lvalue                                                         *
 *                                                                          *
 * Purpose : Check for lvalue, return pointer to lvalue tree.               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

TREE *lvalue(TREE *e)
{
    if (generic(e->op) != INDIR)
    {
        apperror(RCERROR(ERROR_LVALUE_REQUIRED));
        return value(e);
    }
    else if (unqual(e->type) == voidtype)
    {
        apperror(RCWARNING1(ERROR_USED_AS_LVALUE), e->type);
    }

    return e->kids[0];
}

/****************************************************************************
 *                                                                          *
 * Function: condexpr                                                       *
 *                                                                          *
 * Purpose : Parse expression and cast to conditional.                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

TREE *condexpr(int token)
{
    TREE *e = expr(token);

    if (isfunc(e->type))
        apperror(RCWARNING2(ERROR_USED_IN_COND_EXPRESSION), funcname(e));

    return cond(e);
}

/****************************************************************************
 *                                                                          *
 * Function: cond                                                           *
 *                                                                          *
 * Purpose : Check for conditional operator, add comparison if necessary.   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

TREE *cond(TREE *e)
{
    int op = generic(rightkid(e)->op);

    if (op == AND || op == OR || op == NOT ||
        op == EQ  || op == NE || op == LE  || op == LT || op == GE || op == GT)
        return e;

    e = pointer(e);

    return (*optree[NEQ])(NE, e, const_tree(0, inttype));
}

/****************************************************************************
 *                                                                          *
 * Function: pointer                                                        *
 *                                                                          *
 * Purpose : Re-type 'array of T' and 'T function' to                       *
 *           'ptr to T' and 'ptr to T function', respectively.              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

TREE *pointer(TREE *e)
{
    if (isarray(e->type))
        e = retype(e, array_to_ptr(e->type));
    else if (isfunc(e->type))
        e = retype(e, ptr(e->type));

    return e;
}

/****************************************************************************
 *                                                                          *
 * Function: cast                                                           *
 *                                                                          *
 * Purpose : Cast expression e to type.                                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           03-09-23  Added Microsoft flag for float -> unsigned.          *
 *           04-12-27  Conversion to _Bool will now handle long long's.     *
 *                                                                          *
 ****************************************************************************/

TREE *cast(TREE *e, TYPE *type)
{
    TYPE *src;
    TYPE *dst;

    e = value(e);

    if (e->type == type)
        return e;

    dst = unqual(type);
    src = unqual(e->type);

    /* cast to _Bool have a specified behaviour */
    if (dst == booltype)
    {
        e = simplify(CBOOL, dst, cast(e, isfloat(e->type) ? inttype : promote_type(e->type)), NULL);
        return retype(e, type);
    }

    /* signed/unsigned or size difference */
    if (src->op != dst->op || src->size != dst->size)
    {
        switch (src->op)
        {
            case INT_:
                if (src->size < inttype->size)
                    e = simplify(CVI, inttype, e, NULL);
                break;

            case UNSIGNED:
                if (src->size < inttype->size)
                    e = simplify(CVU, inttype, e, NULL);
                else if (src->size < unsignedtype->size)
                    e = simplify(CVU, unsignedtype, e, NULL);
                break;

            case ENUM:
                e = retype(e, inttype);
                break;

            case POINTER:
                if (isint(dst) && src->size > dst->size)
                    apperror(RCWARNING1(ERROR_UNDEFINED_CONVERSION), e->type, type);
                e = simplify(CVP, supertype(src), e, NULL);
                break;

            case FLOAT_:
                break;

            default:
                assert(0);
        }

        src = unqual(e->type);
        dst = supertype(dst);

        /* signed/unsigned difference */
        if (src->op != dst->op)
        {
            switch (src->op)
            {
                case INT_:
                    e = simplify(CVI, dst, e, NULL);
                    break;

                case UNSIGNED:
                    if (isfloat(dst))
                    {
                        TYPE *ssrc = signedint_type(src);
                        TREE *two = cnst_tree(longdoubletype, (long double)2.0);

                        /* use the formula: 2.*(int)(u>>1) + (int)(u&1) */
                        e = (*optree['+'])(ADD,
                            (*optree['*'])(MUL,
                            two,
                            simplify(CVU, ssrc,
                            simplify(RSH, src,
                            e, const_tree(1, inttype)), NULL)),
                            simplify(CVU, ssrc,
                            simplify(BAND, src,
                            e, const_tree(1, unsignedtype)), NULL));
                    }
                    else
                    {
                        e = simplify(CVU, dst, e, NULL);
                    }
                    break;

                case FLOAT_:
                    if (isunsigned(dst))
                    {
                        if (options.microsoft)
                        {
                            e = simplify(CVF, signedint_type(dst), e, NULL);
                        }
                        else
                        {
                            TYPE *sdst = signedint_type(dst);
                            TREE *c = cast(cnst_tree(longdoubletype, (long double)sdst->u.sym->u.limits.max.i + 1), src);

                            /* check for value within bounds */
                            e = condexpr_tree(
                                simplify(GE, src, e, c),
                                (*optree['+'])(ADD,
                                cast(cast(simplify(SUB, src, e, c), sdst), dst),
                                cast(cnst_tree(unsignedlongtype, (uintmax_t)sdst->u.sym->u.limits.max.i + 1), dst)),
                                simplify(CVF, sdst, e, NULL));
                        }
                    }
                    else
                    {
                        e = simplify(CVF, dst, e, NULL);
                    }
                    break;

                default:
                    assert(0);
            }
        }

        dst = unqual(type);
    }

    src = unqual(e->type);
    switch (src->op)
    {
        case INT_:
            if (src->op != dst->op || src->size != dst->size)
                e = simplify(CVI, dst, e, NULL);
            break;

        case UNSIGNED:
            if (src->op != dst->op || src->size != dst->size)
                e = simplify(CVU, dst, e, NULL);
            break;

        case FLOAT_:
            if (src->op != dst->op || src->size != dst->size)
                e = simplify(CVF, dst, e, NULL);
            break;

        case POINTER:
            if (src->op != dst->op)
            {
                e = simplify(CVP, dst, e, NULL);
            }
            else
            {
                if (isfunc(src->type) && !isfunc(dst->type) ||
                    isfunc(dst->type) && !isfunc(src->type))
                    apperror(RCWARNING2(ERROR_NON_PORTABLE_CONVERSION), e->type, type);

                if (src->size != dst->size)
                    e = simplify(CVP, dst, e, NULL);
            }
            break;

        case STRUCT:  /* added 04-08-25; avoid assert() */
            break;

        default:
            assert(0);
    }

    return retype(e, type);
}

/****************************************************************************
 *                                                                          *
 * Function: retype                                                         *
 *                                                                          *
 * Purpose : Force expression to given type.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

TREE *retype(TREE *e, TYPE *ty)
{
    TREE *p;

    if (e->type == ty)
        return e;

    p = new_tree(e->op, ty, e->kids[0], e->kids[1]);
    p->node = e->node;
    p->u = e->u;

    return p;
}

/****************************************************************************
 *                                                                          *
 * Function: supertype                                                      *
 *                                                                          *
 * Purpose : Return supertype of the given type.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static TYPE *supertype(TYPE *ty)
{
    switch (ty->op)
    {
        case INT_:
            if (ty->size < inttype->size)
                return inttype;
            break;

        case UNSIGNED:
            if (ty->size < unsignedtype->size)
                return unsignedtype;
            break;

        case POINTER:
            return unsignedptrtype;
    }

    return ty;
}

/****************************************************************************
 *                                                                          *
 * Function: value                                                          *
 *                                                                          *
 * Purpose : Convert e from a conditional to a value, if necessary.         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

TREE *value(TREE *e)
{
    int op = generic(rightkid(e)->op);

    if (e->type != voidtype &&
        (op == AND || op == OR || op == NOT ||
         op == EQ || op == NE || op == LE ||
         op == LT || op == GE || op == GT))
    {
        e = condexpr_tree(e, const_tree(1, inttype), const_tree(0, inttype));
    }

    return e;
}

/****************************************************************************
 *                                                                          *
 * Function: rightkid                                                       *
 *                                                                          *
 * Purpose : Return the rightmost kid in e.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static TREE *rightkid(TREE *e)
{
    while (e && e->op == RIGHT)
    {
        if (e->kids[1])
            e = e->kids[1];
        else if (e->kids[0])
            e = e->kids[0];
        else
            assert(0);
    }

    assert(e);
    return e;
}

/****************************************************************************
 *                                                                          *
 * Function: funcname                                                       *
 *                                                                          *
 * Purpose : Return name of function e or "a function".                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

char *funcname(TREE *e)
{
    if (isaddrop(e->op))
        return stringf("'%s'", e->u.sym->name);
    else
        return "a function";
}
