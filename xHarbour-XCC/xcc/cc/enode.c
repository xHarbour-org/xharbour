/****************************************************************************
 *                                                                          *
 * File    : enode.c                                                        *
 *                                                                          *
 * Purpose : ISO C Compiler; Expression type-checking and tree construct.   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           05-01-02  Added optimized_struct_tree().                       *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "lcc.h"

#define isvoidptr(ty) \
    (isptr(ty) && unqual(ty->type) == voidtype)

/* Static function prototypes */
static TREE *intrinsic_tree(TREE *, TYPE *, TYPE *, TREE *);
static TREE *call_tree(TREE *, TYPE *, TREE *, SYMBOL *);
static bool_t hascall(TREE *);
static TREE *cmp_tree(int, TREE *, TREE *);
static TREE *add_tree(int, TREE *, TREE *);
static TREE *sub_tree(int, TREE *, TREE *);
static TREE *mul_tree(int, TREE *, TREE *);
static TREE *and_tree(int, TREE *, TREE *);
static bool_t isnullptr(TREE *);
static bool_t is_compatible(TYPE *, TYPE *);
static TYPE *binary_type(TYPE *, TYPE *);

/* After prototypes */
TREE *(*optree[])(int, TREE *, TREE *) = {
#define xx(a,b,c,d,e,f,g) e,
#define yy(a,b,c,d,e,f,g) e,
#include "token.h"
};

/* Globals */
bool_t no_const_check;

/****************************************************************************
 *                                                                          *
 * Function: call                                                           *
 *                                                                          *
 * Purpose : Parse a function call to f, type fty.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           03-10-28  Support for optional arguments added.                *
 *           04-02-10  Bugfix: calling func(arg,) with optparam crashed.    *
 *           05-01-02  Added support for small structure optimization.      *
 *                                                                          *
 ****************************************************************************/

TREE *call(TREE *f, TYPE *fty, COORDINATE src)
{
    int n = 0;
    TREE *args = NULL, *r = NULL, *e;
    TYPE *rty = unqual(func_return(fty));
    TYPE **prototype;
    TREE **optparam;
    SYMBOL *sym = NULL;

    if (fty->u.fcn.oldstyle)
    {
        prototype = NULL;
        optparam = NULL;
    }
    else
    {
        prototype = fty->u.fcn.prototype;
        optparam = fty->u.fcn.optparam;
    }

    if (hascall(f))
        r = f;

    if (isstruct(rty) && !optimized_struct_type(rty))
    {
        sym = make_temp_ident(AUTO, unqual(rty));
        if (rty->size == 0)
            apperror(RCERROR(ERROR_ILLEGAL_USE_OF_TYPE), rty);
    }

    if (tok != ')' || (optparam && *optparam)) for (;;)
    {
        /* must make a copy of the optional argument, and to *permanent* storage */
        TREE *q = tok == ')' && (optparam && *optparam) ? pointer(tree_to_arena(*optparam, PERM)) : pointer(expr1(0));

        if (prototype && *prototype && *prototype != voidtype)
        {
            TYPE *aty;

            q = value(q);

            if ((aty = check_assignment(*prototype, q)) != NULL)
                q = cast(q, aty);
            else
                apperror(RCERROR(ERROR_TYPE_ERROR_IN_ARG_FOUND), n+1, funcname(f), q->type, *prototype);

            if ((isint(q->type) || isenum(q->type)) && q->type->size != inttype->size)
                q = cast(q, promote_type(q->type));

            ++prototype;
            if (optparam) ++optparam;
        }
        else
        {
            if (!fty->u.fcn.oldstyle && *prototype == NULL)
                apperror(RCERROR(ERROR_TOO_MANY_FUNC_PARAMS), funcname(f));

            q = value(q);

            if (isarray(q->type) || q->type->size == 0)
                apperror(RCERROR(ERROR_TYPE_ERROR_IN_ARG), n+1, funcname(f), q->type);
            else
                q = cast(q, promote_type(q->type));
        }

        if (!IR->wants_argb && isstruct(q->type))
        {
            if (is_callb(q))
            {
                q = addrof(q);
            }
            else
            {
                SYMBOL *symT = make_temp_ident(AUTO, unqual(q->type));
                q = assignment(symT, q);
                q = new_tree(RIGHT, ptr(symT->type), root(q), lvalue(id_tree(symT)));
            }
        }

        if (q->type->size == 0)
            q->type = inttype;

        if (hascall(q))
            r = (r) ? new_tree(RIGHT, voidtype, r, q) : q;

        args = new_tree(mkop(ARG, q->type), q->type, q, args);
        n++;

        if (n == 128)  /* old limit 32 */
            apperror(RCWARNING2(ERROR_MORE_THAN_X_CALL_PARAMS), 127, funcname(f));

        if (tok == ')' && optparam && *optparam)
            continue;

        if (tok != ',')
            break;

        tok = gettok();
    }
    expect(')');

    if (prototype && *prototype && *prototype != voidtype)
        apperror(RCERROR(ERROR_INSUFFICIENT_FUNC_ARGS), funcname(f));

    if (r)
        args = new_tree(RIGHT, voidtype, r, args);

    /* Check for special intrinsic function calls */
    if (f->u.sym && (e = intrinsic_tree(f, fty, rty, args)) != NULL)
        ;
    else
        e = call_tree(f, rty, args, sym);

#ifdef PROF
    if (events.calls)
        apply(events.calls, &src, &e);
#endif

    return e;
}

/****************************************************************************
 *                                                                          *
 * Function: intrinsic_tree                                                 *
 *                                                                          *
 * Purpose : Map special intrinsic function calls.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           02-01-09  Added optimization for strlen(constant).             *
 *           02-02-27  Bugfix for above optimization (is_strconst).         *
 *           02-08-25  Support for inline function added.                   *
 *           04-03-17  Added more checks for strlen() function.             *
 *           04-07-15  Completely rewritten support for intrinsics.         *
 *                                                                          *
 ****************************************************************************/

static TREE *intrinsic_tree(TREE *f, TYPE *fty, TYPE *rty, TREE *args)
{
    /* map call to _exception_info() into temporary load */
    if (f->u.sym->name == sehinfo.infoname && rty == voidptype && args == NULL)
    {
        if (!sehinfo.acceptinfo)
        {
            apperror(RCERROR(ERROR_BAD_INTRINSIC_CONTEXT), f->u.sym->name);
            return NULL;
        }

        f->u.sym->intrinsic = TRUE;

        if (!sehinfo.infosym)
        {
            sehinfo.infosym = make_temp_ident(AUTO, voidptype);
            sehinfo.infosym->intrinsic = TRUE;
            sehinfo.infosym->ref++;
        }

        return id_tree(sehinfo.infosym);
    }

    /* map call to _exception_code() into temporary load */
    if (f->u.sym->name == sehinfo.codename && rty == unsignedlongtype && args == NULL)
    {
        if (!sehinfo.acceptcode)
        {
            apperror(RCERROR(ERROR_BAD_INTRINSIC_CONTEXT), f->u.sym->name);
            return NULL;
        }

        f->u.sym->intrinsic = TRUE;

        if (!sehinfo.codesym)
        {
            sehinfo.codesym = make_temp_ident(AUTO, unsignedlongtype);
            sehinfo.codesym->ref++;
        }

        return id_tree(sehinfo.codesym);
    }

    if (options.pragmaopt)
    {
        TREE *e;

        if (options.pragmaopt == MAXSPEED)
        {
            /* see if the function can be called inline */
            if ((e = inline_function(f->u.sym, rty, args)) != NULL)
            {
                f->u.sym->ref -= refinc;
                return e;
            }
        }

        /* see if the function has an intrinsic version */
        if (args && generic(args->op) == RIGHT)
        {
            if (args->kids[1] && (e = intrinsic_call(f, fty, rty, args->kids[1])) != NULL)
            {
                args->kids[1] = e;
                args->type = rty;
                return args;
            }
            return intrinsic_call(f, fty, rty, args->kids[0]);
        }
        else
        {
            return intrinsic_call(f, fty, rty, args);
        }
    }

    return NULL;
}

/****************************************************************************
 *                                                                          *
 * Function: vcall                                                          *
 *                                                                          *
 * Purpose : Build a call to an internal function.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-12-30  Created                                              *
 *                                                                          *
 ****************************************************************************/

TREE *vcall(SYMBOL *func, TYPE *ty, ...)
{
    TREE *f = pointer(id_tree(func));
    TREE *args = NULL, *r = NULL, *e;
    va_list ap;

    assert(isfunc(func->type));

    if (ty == NULL)
        ty = func_return(func->type);

    va_start(ap, ty);
    while ((e = va_arg(ap, TREE *)) != NULL)
    {
        if (hascall(e))
            r = (r == NULL) ? e : new_tree(RIGHT, voidtype, r, e);
        args = new_tree(mkop(ARG, e->type), e->type, e, args);
    }
    va_end(ap);

    if (r != NULL)
        args = new_tree(RIGHT, voidtype, r, args);

    return call_tree(f, ty, args, NULL);
}

/****************************************************************************
 *                                                                          *
 * Function: call_tree                                                      *
 *                                                                          *
 * Purpose : Construct tree for a function call.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           05-01-02  Added support for small structure optimization.      *
 *                                                                          *
 ****************************************************************************/

static TREE *call_tree(TREE *f, TYPE *ty, TREE *args, SYMBOL *sym)
{
    TREE *e;

    if (args != NULL)
        f = new_tree(RIGHT, f->type, args, f);

    if (isstruct(ty))
    {
        if (!optimized_struct_type(ty))
        {
            /* structure call with hidden argument */
            assert(sym);
            e = new_tree(RIGHT, ty,
                new_tree(CALL+B, ty, f, addrof(id_tree(sym))), id_tree(sym));
        }
        else
        {
            /* optimized structure call */
            e = new_tree(CALL+B, ty, f, NULL);
        }
    }
    else
    {
        TYPE *rty = ty;

        if (isenum(ty))
            rty = unqual(ty)->type;

        if (!isfloat(rty))
            rty = promote_type(rty);

        e = new_tree(mkop(CALL, rty), rty, f, NULL);

        if (isptr(ty) || e->type->size > ty->size)
            e = cast(e, ty);
    }

    return e;
}

/****************************************************************************
 *                                                                          *
 * Function: hascall                                                        *
 *                                                                          *
 * Purpose : Return true if the expression contains a call.                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-11-24  Bugfix: use dagop in IR->opcall (not e->op).         *
 *                                                                          *
 ****************************************************************************/

#define dagop(e)  (isarray(e->type) ? e->op + sizeop(voidptype->size) : e->op + sizeop(e->type->size))
static bool_t hascall(TREE *e)
{
    if (e == NULL)
        return FALSE;

    if (generic(e->op) == CALL || (*IR->opcall)(dagop(e)))
        return TRUE;

    return hascall(e->kids[0]) || hascall(e->kids[1]);
}
#undef dagop

/****************************************************************************
 *                                                                          *
 * Function: vlasize_tree                                                   *
 *                                                                          *
 * Purpose : Return a tree for a variable-length array size.                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           03-08-14  Created                                              *
 *                                                                          *
 ****************************************************************************/

TREE *vlasize_tree(TYPE *ty, int asgn)
{
    TREE *e;

    assert(isvla(ty));

    if (ty->u.arr.sym != 0 && asgn >= 0)
        return id_tree(ty->u.arr.sym);

    if (isvla(ty->type))  /* multiple VLA dimensions */
        e = mul_tree(MUL, vlasize_tree(ty->type, asgn), tree_to_arena(ty->u.arr.e, STMT));
    else
        e = mul_tree(MUL, cnst_tree(inttype, (intmax_t)ty->type->size), tree_to_arena(ty->u.arr.e, STMT));

    if (asgn)
    {
        if (asgn > 0)
            ty->u.arr.sym = make_temp_ident(AUTO, inttype);
        return assignment(ty->u.arr.sym, e);
    }

    return e;
}

/****************************************************************************
 *                                                                          *
 * Function: const_tree                                                     *
 *                                                                          *
 * Purpose : Return a tree for a constant n of type ty (int or unsigned).   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

TREE *const_tree(uintmax_t n, TYPE *ty)
{
    if (isarray(ty))
        ty = array_to_ptr(ty);
    else
        assert(isint(ty));

    return cnst_tree(ty, n);
}

/****************************************************************************
 *                                                                          *
 * Function: cnst_tree                                                      *
 *                                                                          *
 * Purpose : Construct tree for a constant.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

TREE *cnst_tree(TYPE *ty, ...)  /* dangerous - always use (u)intmax_t */
{
    TREE *e = new_tree(mkop(CNST,ty), ty, NULL, NULL);
    va_list ap;

    va_start(ap, ty);
    switch (ty->op)
    {
        case INT_:     e->u.v.i = va_arg(ap, intmax_t); break;
        case UNSIGNED: e->u.v.u = va_arg(ap, uintmax_t)&ones(8*ty->size); break;
        case FLOAT_:   e->u.v.d = va_arg(ap, long double); break;
        case POINTER:  e->u.v.p = va_arg(ap, void *); break;
        default: assert(0);
    }
    va_end(ap);
    return e;
}

/****************************************************************************
 *                                                                          *
 * Function: assignment_tree                                                *
 *                                                                          *
 * Purpose : Construct tree for l = r.                                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-07-22  Better handling of Microsoft 'cast on lvalue'.       *
 *           04-07-22  Moved 'cast on lvalue' warning to level 2.           *
 *                                                                          *
 ****************************************************************************/

TREE *assignment_tree(int op, TREE *l, TREE *r)
{
    TREE *q = NULL;  /* Microsoft lvalue */
    TYPE *aty, *ty;

    r = pointer(r);

    if ((ty = check_assignment(l->type, r)) != NULL)
    {
        r = cast(r, ty);
    }
    else
    {
        typeerror(ASGN, l, r);

        if (r->type == voidtype)
            r = retype(r, inttype);

        ty = r->type;
    }

    /* accept Microsoft "cast on lvalue" */
    if (options.microsoft && l->op == RIGHT && !l->kids[0] && generic(l->kids[1]->op) == INDIR)
    {
        apperror(RCWARNING2(ERROR_CAST_ON_LVALUE), l->type);
        q = l = l->kids[1];
    }

    if (l->op != AFIELD)
        l = lvalue(l);

    aty = l->type;
    if (isptr(aty))
        aty = unqual(aty)->type;

    if (!no_const_check && (isconst(aty) || (isstruct(aty) && unqual(aty)->u.sym->u.s.cfields)))
    {
        if (isaddrop(l->op) && !l->u.sym->computed && !l->u.sym->generated)
            apperror(RCERROR(ERROR_ASSIGN_TO_CONST_IDENT), l->u.sym->name);
        else
            apperror(RCERROR(ERROR_ASSIGN_TO_CONST_LOC));
    }

    if (l->op == AFIELD)
    {
        intmax_t n = 8*l->u.field->type->size - fieldsize(l->u.field);

        if (n > 0 && isunsigned(l->u.field->type))
        {
            r = bit_tree(BAND, r, cnst_tree(r->type, (uintmax_t)fieldmask(l->u.field)));
        }
        else if (n > 0)
        {
            if (r->op == CNST+I)
            {
                n = r->u.v.i;

                if (n & (1 << (fieldsize(l->u.field)-1)))
                    n |= ~0UL << fieldsize(l->u.field);
                r = cnst_tree(r->type, n);
            }
            else
            {
                r = shift_tree(RSH, shift_tree(LSH, r, cnst_tree(inttype, n)), cnst_tree(inttype, n));
            }
        }
    }

    if (isstruct(ty) && isaddrop(l->op) && is_callb(r))
        return new_tree(RIGHT, ty, new_tree(CALL+B, ty, r->kids[0]->kids[0], l), id_tree(l->u.sym));

    if (q)  /* could always use RIGHT tree, but some optimizations will fail */
        return new_tree(RIGHT, ty, q, new_tree(mkop(op,ty), ty, l, r));
    else
        return new_tree(mkop(op,ty), ty, l, r);
}

/****************************************************************************
 *                                                                          *
 * Function: condexpr_tree                                                  *
 *                                                                          *
 * Purpose : Construct tree for e ? l : r.                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           03-09-03  Added undefined conversions.                         *
 *                                                                          *
 ****************************************************************************/

TREE *condexpr_tree(TREE *e, TREE *l, TREE *r)
{
    TYPE *xty = l->type;
    TYPE *yty = r->type;
    TYPE *ty;
    TREE *p;
    SYMBOL *sym;

    if (isarith(xty) && isarith(yty))
        ty = binary_type(xty, yty);
    else if (is_same_type(xty, yty, TRUE))
        ty = unqual(xty);
    else if (isptr(xty) && isnullptr(r))
        ty = xty;
    else if (isnullptr(l) && isptr(yty))
        ty = yty;
    else if (isptr(xty) && !isfunc(xty->type) && isvoidptr(yty) ||
             isptr(yty) && !isfunc(yty->type) && isvoidptr(xty))
        ty = voidptype;
    else if ((isptr(xty) && isptr(yty) &&
            is_same_type(unqual(xty->type), unqual(yty->type), TRUE)))
        ty = xty;
#ifdef XHARBOUR
    else if (isptr(xty) && isint(yty))  /* 03-09-03 */
    {
        apperror(RCWARNING1(ERROR_UNDEFINED_CONVERSION), yty, xty);
        ty = xty;
    }
    else if (isptr(yty) && isint(xty))  /* 03-09-03 */
    {
        apperror(RCWARNING1(ERROR_UNDEFINED_CONVERSION), xty, yty);
        ty = yty;
    }
#endif /* XHARBOUR */
    else
    {
        typeerror(COND, l, r);
        return const_tree(0, inttype);
    }

    if (isptr(ty))
    {
        ty = unqual(unqual(ty)->type);

        if (isptr(xty) && isconst(unqual(xty)->type) ||
            isptr(yty) && isconst(unqual(yty)->type))
            ty = qual(CONST_, ty);

        if (isptr(xty) && isvolatile(unqual(xty)->type) ||
            isptr(yty) && isvolatile(unqual(yty)->type))
            ty = qual(VOLATILE_, ty);

        if (isptr(xty) && isrestrict(unqual(xty)->type) ||
            isptr(yty) && isrestrict(unqual(yty)->type))
            ty = qual(RESTRICT_, ty);

        ty = ptr(ty);
    }

    switch (e->op)
    {
        case CNST+I: return cast(e->u.v.i != 0 ? l : r, ty);
        case CNST+U: return cast(e->u.v.u != 0 ? l : r, ty);
        case CNST+P: return cast(e->u.v.p != 0 ? l : r, ty);
        case CNST+F: return cast(e->u.v.d != 0.0 ? l : r, ty);
    }

    if (ty != voidtype && ty->size > 0)
    {
        sym = make_ident(REGISTER, unqual(ty), scope);
        /*  sym = make_temp_ident(REGISTER, unqual(ty));  */
        l = assignment(sym, l);
        r = assignment(sym, r);
    }
    else
    {
        sym = NULL;
    }

    p = new_tree(COND, ty, cond(e),
        new_tree(RIGHT, ty, root(l), root(r)));

    p->u.sym = sym;
    return p;
}

/****************************************************************************
 *                                                                          *
 * Function: equality_tree                                                  *
 *                                                                          *
 * Purpose : Construct tree for l [== !=] r.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-12-23  Fix RIGHT trees with single kid - for better code.   *
 *                                                                          *
 ****************************************************************************/

TREE *equality_tree(int op, TREE *l, TREE *r)
{
    TYPE *xty = unqual(l->type);
    TYPE *yty = unqual(r->type);

    if (isptr(xty) && isnullptr(r) ||
        isptr(xty) && !isfunc(xty->type) && isvoidptr(yty) ||
       (isptr(xty) && isptr(yty) && is_same_type(unqual(xty->type), unqual(yty->type), TRUE)))
    {
        TYPE *ty = unsignedptrtype;
        l = cast(l, ty);
        r = cast(r, ty);
        return simplify(mkop(op,ty), inttype, l, r);
    }

    if (isptr(yty) && isnullptr(l) ||
        isptr(yty) && !isfunc(yty->type) && isvoidptr(xty))
        return equality_tree(op, r, l);

    while (l->op == RIGHT && !l->kids[0] && l->type == l->kids[1]->type)
        l = l->kids[1];
    while (r->op == RIGHT && !r->kids[0] && r->type == r->kids[1]->type)
        r = r->kids[1];

    return cmp_tree(op, l, r);
}

/****************************************************************************
 *                                                                          *
 * Function: cmp_tree                                                       *
 *                                                                          *
 * Purpose : Construct tree for l [< <= >= >] r.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           03-09-03  Added undefined conversions.                         *
 *                                                                          *
 ****************************************************************************/

static TREE *cmp_tree(int op, TREE *l, TREE *r)
{
    TYPE *ty;

    if (isarith(l->type) && isarith(r->type))
    {
        ty = binary_type(l->type, r->type);
        l = cast(l, ty);
        r = cast(r, ty);
    }
    else if (is_compatible(l->type, r->type))
    {
        ty = unsignedptrtype;
        l = cast(l, ty);
        r = cast(r, ty);
    }
#ifdef XHARBOUR
    else if (isptr(l->type) && isint(r->type))  /* 03-09-03 */
    {
        ty = unsignedptrtype;
        apperror(RCWARNING1(ERROR_UNDEFINED_CONVERSION), l->type, ty);
        l = cast(l, ty);
        r = cast(r, ty);
    }
    else if (isptr(r->type) && isint(l->type))  /* 03-09-03 */
    {
        ty = unsignedptrtype;
        apperror(RCWARNING1(ERROR_UNDEFINED_CONVERSION), r->type, ty);
        l = cast(l, ty);
        r = cast(r, ty);
    }
#endif /* XHARBOUR */
    else
    {
        ty = unsignedtype;
        typeerror(op, l, r);
    }

    return simplify(mkop(op,ty), inttype, l, r);
}

/****************************************************************************
 *                                                                          *
 * Function: add_tree                                                       *
 *                                                                          *
 * Purpose : Construct tree for l + r.                                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           03-08-14  Support for variable-length arrays added.            *
 *                                                                          *
 ****************************************************************************/

static TREE *add_tree(int op, TREE *l, TREE *r)
{
    TYPE *ty = inttype;

    if (isarith(l->type) && isarith(r->type))
    {
        ty = binary_type(l->type, r->type);
        l = cast(l, ty);
        r = cast(r, ty);
    }
    else if (isptr(l->type) && isint(r->type))
    {
        return add_tree(ADD, r, l);
    }
    else if (isptr(r->type) && isint(l->type) && !isfunc(r->type->type))
    {
        ty = unqual(r->type);

        if (isvla(ty->type))
        {
            /* handle VLA addressing */
            l = cast(l, promote_type(l->type));
            l = mul_tree(MUL, vlasize_tree(ty->type, 0), l);
        }
        else
        {
            intmax_t sz = unqual(ty->type)->size;

            if (sz == 0)
                apperror(RCERROR(ERROR_UNKNOWN_SIZE_FOR_TYPE), ty->type);

            l = cast(l, promote_type(l->type));

            if (sz > 1)
                l = mul_tree(MUL, cnst_tree(signedptrtype, sz), l);
        }

        if (isunsigned(l->type))
            l = cast(l, unsignedptrtype);
        else
            l = cast(l, signedptrtype);

        return simplify(ADD, ty, l, r);
    }
    else
    {
        typeerror(op, l, r);
    }

    return simplify(op, ty, l, r);
}

/****************************************************************************
 *                                                                          *
 * Function: sub_tree                                                       *
 *                                                                          *
 * Purpose : Construct tree for l - r.                                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static TREE *sub_tree(int op, TREE *l, TREE *r)
{
    TYPE *ty = inttype;
    intmax_t sz;

    if (isarith(l->type) && isarith(r->type))
    {
        ty = binary_type(l->type, r->type);
        l = cast(l, ty);
        r = cast(r, ty);
    }
    else if (isptr(l->type) && !isfunc(l->type->type) && isint(r->type))
    {
        ty = unqual(l->type);
        sz = unqual(ty->type)->size;
        if (sz == 0)
            apperror(RCERROR(ERROR_UNKNOWN_SIZE_FOR_TYPE), ty->type);

        r = cast(r, promote_type(r->type));
        if (sz > 1)
            r = mul_tree(MUL, cnst_tree(signedptrtype, sz), r);

        if (isunsigned(r->type))
            r = cast(r, unsignedptrtype);
        else
            r = cast(r, signedptrtype);

        return simplify(SUB+P, ty, l, r);
    }
    else if (is_compatible(l->type, r->type))
    {
        ty = unqual(l->type);
        sz = unqual(ty->type)->size;
        if (sz == 0)
            apperror(RCERROR(ERROR_UNKNOWN_SIZE_FOR_TYPE), ty->type);

        l = simplify(SUB+U, unsignedptrtype,
            cast(l, unsignedptrtype), cast(r, unsignedptrtype));

        return simplify(DIV+I, longtype,
            cast(l, longtype), cnst_tree(longtype, sz));
    }
    else
    {
        typeerror(op, l, r);
    }

    return simplify(op, ty, l, r);
}

/****************************************************************************
 *                                                                          *
 * Function: mul_tree                                                       *
 *                                                                          *
 * Purpose : Construct tree for l [* /] r.                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static TREE *mul_tree(int op, TREE *l, TREE *r)
{
    TYPE *ty = inttype;

    if (isarith(l->type) && isarith(r->type))
    {
        ty = binary_type(l->type, r->type);
        l = cast(l, ty);
        r = cast(r, ty);
    }
    else
    {
        typeerror(op, l, r);
    }

    return simplify(op, ty, l, r);
}

/****************************************************************************
 *                                                                          *
 * Function: and_tree                                                       *
 *                                                                          *
 * Purpose : Construct tree for l [&& ||] r.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static TREE *and_tree(int op, TREE *l, TREE *r)
{
    if (!isscalar(l->type) || !isscalar(r->type))
        typeerror(op, l, r);

    return simplify(op, inttype, cond(l), cond(r));
}

/****************************************************************************
 *                                                                          *
 * Function: bit_tree                                                       *
 *                                                                          *
 * Purpose : Construct tree for l [& | ^ %] r.                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

TREE *bit_tree(int op, TREE *l, TREE *r)
{
    TYPE *ty = inttype;

    if (isint(l->type) && isint(r->type))
    {
        ty = binary_type(l->type, r->type);
        l = cast(l, ty);
        r = cast(r, ty);
    }
    else
    {
        typeerror(op, l, r);
    }

    return simplify(op, ty, l, r);
}

/****************************************************************************
 *                                                                          *
 * Function: shift_tree                                                     *
 *                                                                          *
 * Purpose : Construct tree for l [>> <<] r.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

TREE *shift_tree(int op, TREE *l, TREE *r)
{
    TYPE *ty = inttype;

    if (isint(l->type) && isint(r->type))
    {
        ty = promote_type(l->type);
        l = cast(l, ty);
        r = cast(r, inttype);
    }
    else
    {
        typeerror(op, l, r);
    }

    return simplify(op, ty, l, r);
}

/****************************************************************************
 *                                                                          *
 * Function: optimized_struct_tree                                          *
 *                                                                          *
 * Purpose : Rebuild tree for small structure optimization.                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           05-01-02  Created                                              *
 *                                                                          *
 ****************************************************************************/

TREE *optimized_struct_tree(TREE *e)
{
    if (e)
    {
        /* must process kids first */
        e->kids[0] = optimized_struct_tree(e->kids[0]);
        e->kids[1] = optimized_struct_tree(e->kids[1]);

        switch (e->op)
        {
            TYPE *sty;

            case CALL+B:
                if ((sty = optimized_struct_type(e->type)) != NULL)
                {
                    sty = promote_type(sty);
                    e = new_tree(mkop(CALL,sty), sty, e->kids[0], e->kids[1]);
                }
                break;

            case RET+B:
                if ((sty = optimized_struct_type(e->type)) != NULL)
                {
                    sty = promote_type(sty);
                    e = new_tree(mkop(RET,sty), sty, cast(e->kids[0], sty), NULL);
                }
                break;

            case ARG+B:
                if ((sty = optimized_struct_type(e->type)) != NULL)
                {
                    sty = promote_type(sty);
                    e = new_tree(mkop(ARG,sty), sty, cast(e->kids[0], sty), e->kids[1]);
                }
                break;

            case ASGN+B:
                if ((sty = optimized_struct_type(e->type)) != NULL)
                    e = new_tree(mkop(ASGN,sty), sty, e->kids[0], cast(e->kids[1], sty));
                break;

            case INDIR+B:
                if ((sty = optimized_struct_type(e->type)) != NULL)
                    e = new_tree(mkop(INDIR,sty), sty, e->kids[0], NULL);
                break;
        }
    }

    return e;
}

/****************************************************************************
 *                                                                          *
 * Function: is_callb                                                       *
 *                                                                          *
 * Purpose : Return true if this is a "function returning struct".          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

bool_t is_callb(TREE *e)
{
    return e->op == RIGHT && e->kids[0] && e->kids[1] &&
        e->kids[0]->op == CALL+B &&
        e->kids[1]->op == INDIR+B &&
        isaddrop(e->kids[1]->kids[0]->op) &&
        e->kids[1]->kids[0]->u.sym->temporary;
}

/****************************************************************************
 *                                                                          *
 * Function: isnullptr                                                      *
 *                                                                          *
 * Purpose : Return true if this is a "null pointer value".                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static bool_t isnullptr(TREE *e)
{
    TYPE *ty = unqual(e->type);

    return generic(e->op) == CNST &&
        (ty->op == INT_ && e->u.v.i == 0 ||
         ty->op == UNSIGNED && e->u.v.u == 0 ||
         isvoidptr(ty) && e->u.v.p == NULL);
}

/****************************************************************************
 *                                                                          *
 * Function: check_assignment                                               *
 *                                                                          *
 * Purpose : Perform type-checking of assignment of e to variable           *
 *           of type xty.                                                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           03-09-03  Added undefined conversions.                         *
 *           03-09-08  Accept signed/unsigned mismatch with a warning.      *
 *           04-10-05  Handle multi-level pointers and incomplete structs.  *
 *                                                                          *
 ****************************************************************************/

TYPE *check_assignment(TYPE *xty, TREE *e)
{
    TYPE *yty = unqual(e->type);

    xty = unqual(xty);

    if (isenum(xty))
        xty = xty->type;

    if (xty->size == 0 || yty->size == 0)
        return NULL;

    if (xty == booltype)
        return xty;

    if (isarith(xty) && isarith(yty) || isstruct(xty) && xty == yty)
        return xty;

    if (isptr(xty) && isnullptr(e))
        return xty;

#ifdef XHARBOUR
    if (isptr(xty) && isint(yty))  /* 03-09-03 */
    {
        apperror(RCWARNING1(ERROR_UNDEFINED_CONVERSION), xty, yty);  /* warning added 03-09-14 */
        return xty;
    }
    if (isptr(yty) && isint(xty))  /* 03-09-03 */
    {
        apperror(RCWARNING1(ERROR_UNDEFINED_CONVERSION), yty, xty);  /* warning added 03-09-14 */
        return yty;
    }
#endif /* XHARBOUR */

    if ((isvoidptr(xty) && isptr(yty) || isptr(xty) && isvoidptr(yty)) &&
        ((isconst(xty->type) || !isconst(yty->type)) &&
        (isvolatile(xty->type) || !isvolatile(yty->type)) &&
        (isrestrict(xty->type) || !isrestrict(yty->type))))
        return xty;

    if (isptr(xty) && isptr(yty) &&
        ((isconst(xty->type) || !isconst(yty->type)) &&
        (isvolatile(xty->type) || !isvolatile(yty->type)) &&
        (isrestrict(xty->type) || !isrestrict(yty->type))))
    {
        TYPE *lty = unqual(xty->type);
        TYPE *rty = unqual(yty->type);

        if (is_same_type(lty, rty, TRUE))
            return xty;

        while (isptr(lty) && isptr(rty) &&
            ((isconst(lty->type) || !isconst(rty->type)) &&
            (isvolatile(lty->type) || !isvolatile(rty->type)) &&
            (isrestrict(lty->type) || !isrestrict(rty->type))))  /* 04-10-05 */
        {
            lty = unqual(lty->type);
            rty = unqual(rty->type);
        }

        if (isenum(lty) && rty == inttype || isenum(rty) && lty == inttype)  /* lcc org */
        {
            apperror(RCWARNING1(ERROR_NON_PORTABLE_ASSIGNMENT), xty, yty);
            return xty;
        }

        if (isint(lty) && isint(rty) && lty->size == rty->size)
        {
            apperror(RCWARNING1(ERROR_ASSIGNMENT_OF_TYPES), yty, xty);
            return xty;
        }

        if (isfunc(lty) && isfunc(rty))
        {
            apperror(RCWARNING1(ERROR_INVALID_BINARY_TYPES), "=", lty, rty);
            return xty;
        }

        if (isstruct(lty) && isstruct(rty) && lty->u.sym->name == rty->u.sym->name)  /* 04-10-05 */
        {
            apperror(options.microsoft ? 
                RCWARNING2(ERROR_ASSIGNMENT_OF_TYPES) :
                RCWARNING1(ERROR_ASSIGNMENT_OF_TYPES), yty, xty);
            return xty;
        }
    }

    return NULL;
}

/****************************************************************************
 *                                                                          *
 * Function: assignment                                                     *
 *                                                                          *
 * Purpose : Generate tree for assignment of expr e to symbol sym           *
 *           sans qualifiers.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           03-08-14  Support for variable-length arrays added.            *
 *                                                                          *
 ****************************************************************************/

TREE *assignment(SYMBOL *sym, TREE *e)
{
    if (isvla(sym->type))
    {
        e = assignment_tree(ASGN, retype(id_tree(sym), voidptype), e);
    }
    else if (isarray(sym->type))
    {
        e = new_tree(ASGN+B, sym->type, id_tree(sym),
            new_tree(INDIR+B, e->type, e, NULL));
    }
    else
    {
        TYPE *ty = sym->type;

        sym->type = unqual(sym->type);
        if (isstruct(sym->type) && sym->type->u.sym->u.s.cfields)
        {
            sym->type->u.sym->u.s.cfields = FALSE;
            e = assignment_tree(ASGN, id_tree(sym), e);
            sym->type->u.sym->u.s.cfields = TRUE;
        }
        else
        {
            e = assignment_tree(ASGN, id_tree(sym), e);
        }

        sym->type = ty;
    }

    return e;
}

/****************************************************************************
 *                                                                          *
 * Function: addrof                                                         *
 *                                                                          *
 * Purpose : Address of p.                                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

TREE *addrof(TREE *p)
{
    TREE *q = p;

    for (;;)
    {
        switch (generic(q->op))
        {
            case RIGHT:
                assert(q->kids[0] || q->kids[1]);
                q = q->kids[1] ? q->kids[1] : q->kids[0];
                continue;

            case ASGN:
                q = q->kids[1];
                continue;

            case COND:
            {
                SYMBOL *sym = q->u.sym;
                q->u.sym = 0;
                q = id_tree(sym);
            }
            /* fall through */
            case INDIR:
                if (p == q)
                    return q->kids[0];
                q = q->kids[0];
                return new_tree(RIGHT, q->type, root(p), q);

            default:
                apperror(RCERROR(ERROR_ADDRESSABLE_OBJ_REQUIRED));
                return value(p);
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: is_compatible                                                  *
 *                                                                          *
 * Purpose : Are ty1 & ty2 sans qualifiers pointers to compatible           *
 *           object or incomplete types?                                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static bool_t is_compatible(TYPE *ty1, TYPE *ty2)
{
    ty1 = unqual(ty1);
    ty2 = unqual(ty2);

    return isptr(ty1) && !isfunc(ty1->type) &&
        isptr(ty2) && !isfunc(ty2->type) &&
        is_same_type(unqual(ty1->type), unqual(ty2->type), FALSE);
}

/****************************************************************************
 *                                                                          *
 * Function: binary_type                                                    *
 *                                                                          *
 * Purpose : Usual arithmetic conversions, return target type.              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static TYPE *binary_type(TYPE *xty, TYPE *yty)
{
    if (xty == longdoubletype || yty == longdoubletype) return longdoubletype;
    if (xty == doubletype || yty == doubletype) return doubletype;
    if (xty == floattype || yty == floattype) return floattype;
    if (xty == unsignedlonglongtype || yty == unsignedlonglongtype) return unsignedlonglongtype;
    if (xty == longlongtype || yty == longlongtype) return longlongtype;
    if (xty == unsignedlongtype || yty == unsignedlongtype) return unsignedlongtype;
    if (xty == longtype && yty == unsignedtype || xty == unsignedtype && yty == longtype)
        return (longtype->size > unsignedtype->size) ? longtype : unsignedlongtype;
    if (xty == longtype || yty == longtype) return longtype;
    if (xty == unsignedtype || yty == unsignedtype) return unsignedtype;

    return inttype;
}

/****************************************************************************
 *                                                                          *
 * Function: typeerror                                                      *
 *                                                                          *
 * Purpose : Issue "operands of op have illegal types 'l' and 'r'".         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void typeerror(int op, TREE *l, TREE *r)
{
    int i;

    static struct { int op; char *name; } ops[] = {
        ASGN, "=",      INDIR, "*",     NEG,  "-",
        ADD,  "+",      SUB,   "-",     LSH,  "<<",
        MOD,  "%",      RSH,   ">>",    BAND, "&",
        BCOM, "~",      BOR,   "|",     BXOR, "^",
        DIV,  "/",      MUL,   "*",     EQ,   "==",
        GE,   ">=",     GT,    ">",     LE,   "<=",
        LT,   "<",      NE,    "!=",    AND,  "&&",
        NOT,  "!",      OR,    "||",    COND, "?:",
        0, 0
    };

    op = generic(op);

    for (i = 0; ops[i].op; i++)
        if (op == ops[i].op) break;

    assert(ops[i].name);

    if (r)
        apperror(RCERROR(ERROR_INVALID_BINARY_TYPES), ops[i].name, l->type, r->type);
    else
        apperror(RCERROR(ERROR_INVALID_UNARY_TYPE), ops[i].name, l->type);
}
