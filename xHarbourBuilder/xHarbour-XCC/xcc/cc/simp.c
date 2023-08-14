/****************************************************************************
 *                                                                          *
 * File    : simp.c                                                         *
 *                                                                          *
 * Purpose : ISO C Compiler; Constant folding & expression simplification.  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-10-15  Bugfix: no warning in geu() macro if two CNST+U.     *
 *           04-10-15  Bugfix: added functions lshi() and msb().            *
 *           04-12-18  Reorganize expressions for better int/float code.    *
 *           05-01-03  Force folding if explicit_cast is true.              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include <float.h>
#include "lcc.h"

/* force constants in right child */
#define commute(L,R) \
    if (generic(L->op) == CNST && generic(R->op) != CNST) \
    { TREE *t = L; L = R; R = t; }

/* force constants in right child (conditionals) */
#define commutecond(L,R,OP) \
    if (generic(L->op) == CNST && generic(R->op) != CNST) \
        return simplify(OP, ty, R, L);

/* evaluate constant expression */
#define foldcnst(TYPE,VAR,OP) \
    if (l->op == CNST+TYPE && r->op == CNST+TYPE) \
        return cnst_tree(ty, l->u.v.VAR OP r->u.v.VAR)

/* evaluate constant conditional expression */
#define foldcnstcond(TYPE,VAR,OP) \
    if (l->op == CNST+TYPE && r->op == CNST+TYPE) \
        return cnst_tree(inttype, (intmax_t)(l->u.v.VAR OP r->u.v.VAR))

/* evaluate constant expression with overflow check */
#define foldcnstlim(TYPE,VAR,OP,FUNC) \
    if (l->op == CNST+TYPE && r->op == CNST+TYPE && \
        FUNC(l->u.v.VAR,r->u.v.VAR,\
            ty->u.sym->u.limits.min.VAR,\
            ty->u.sym->u.limits.max.VAR, need_const)) \
        return cnst_tree(ty, l->u.v.VAR OP r->u.v.VAR)

/* evaluate constant unary expression */
#define ufoldcnst(TYPE,EXP) \
    if (l->op == CNST+TYPE) return EXP

/* evaluate constant shift expression */
#define sfoldcnst(OP) \
    if (l->op == CNST+U && r->op == CNST+I && \
        r->u.v.i >= 0 && r->u.v.i < 8*l->type->size) \
        return cnst_tree(ty, (uintmax_t)(l->u.v.u OP r->u.v.i))

/* evaluate constant ADD+P */
#define foldaddp(L,R,RTYPE,VAR) \
    if (L->op == CNST+P && R->op == CNST+RTYPE) \
    { \
        TREE *e = new_tree(CNST+P, ty, NULL, NULL); \
        e->u.v.p = (char *)L->u.v.p + R->u.v.VAR; \
        return e; \
    }

/* evaluate constant ADD+I or ADD+U */
#define foldadd(TYPE) \
    /* (x + c1) + c2 => x + (c1+c2) */ \
    if (r->op == CNST+TYPE && l->op == ADD+TYPE && l->kids[1]->op == CNST+TYPE) \
        return simplify(ADD, ty, l->kids[0], simplify(ADD, ty, l->kids[1], r)); \
    /* (x - c1) + c2 => x + (c2-c1) */ \
    if (r->op == CNST+TYPE && l->op == SUB+TYPE && l->kids[1]->op == CNST+TYPE) \
        return simplify(ADD, ty, l->kids[0], simplify(SUB, ty, r, l->kids[1])); \
    /* (c1 - x) + c2 => (c1+c2) - x */ \
    if (r->op == CNST+TYPE && l->op == SUB+TYPE && l->kids[0]->op == CNST+TYPE) \
        return simplify(SUB, ty, simplify(ADD, ty, l->kids[0], r), l->kids[1]); \
    /* (x + c1) + (y + c2) => (x + y) + (c1 + c2) */ \
    if (l->op == ADD+TYPE && l->kids[1]->op == CNST+TYPE && r->op == ADD+TYPE && r->kids[1]->op == CNST+TYPE) \
        return simplify(ADD, ty, simplify(ADD, ty, l->kids[0], r->kids[0]), simplify(ADD, ty, l->kids[1], r->kids[1])); \
    /* (x + c1) + (y - c2) => (x + y) + (c1 - c2) */ \
    if (l->op == ADD+TYPE && l->kids[1]->op == CNST+TYPE && r->op == SUB+TYPE && r->kids[1]->op == CNST+TYPE) \
        return simplify(ADD, ty, simplify(ADD, ty, l->kids[0], r->kids[0]), simplify(SUB, ty, l->kids[1], r->kids[1])); \
    /* (x - c1) + (y + c2) => (x + y) + (c2 - c1) */ \
    if (l->op == SUB+TYPE && l->kids[1]->op == CNST+TYPE && r->op == ADD+TYPE && r->kids[1]->op == CNST+TYPE) \
        return simplify(ADD, ty, simplify(ADD, ty, l->kids[0], r->kids[0]), simplify(SUB, ty, r->kids[1], l->kids[1])); \
    /* (x - c1) + (y - c2) => (x + y) - (c1 + c2) */ \
    if (l->op == SUB+TYPE && l->kids[1]->op == CNST+TYPE && r->op == SUB+TYPE && r->kids[1]->op == CNST+TYPE) \
        return simplify(SUB, ty, simplify(ADD, ty, l->kids[0], r->kids[0]), simplify(ADD, ty, l->kids[1], r->kids[1]));

/* simplifications for ADD+F, ADD+I or ADD+U */
#define simpadd(TYPE) \
    /* x + (-y) => x - y */ \
    if (r->op == NEG+TYPE) \
        return simplify(SUB, ty, l, r->kids[0]); \
    /* (-x) + y => y - x */ \
    if (l->op == NEG+TYPE) \
        return simplify(SUB, ty, r, l->kids[0]); \
    /* x + (expr) => (expr) + x */ \
    if (l->op == INDIR+TYPE && r->op != INDIR+TYPE && r->op != CNST+TYPE) \
        return new_tree(ADD+TYPE, ty, r, l); \
    /* (x + c) + y => (x + y) + c */ \
    if (l->op == ADD+TYPE && l->kids[1]->op == CNST+TYPE && r->op == INDIR+TYPE) \
        return simplify(ADD, ty, simplify(ADD, ty, l->kids[0], r), l->kids[1]);

/* evaluate constant SUB+I or SUB+U */
#define foldsub(TYPE) \
    /* (x + c1) - c2 => x + (c1-c2) */ \
    if (r->op == CNST+TYPE && l->op == ADD+TYPE && l->kids[1]->op == CNST+TYPE) \
        return simplify(ADD, ty, l->kids[0], simplify(SUB, ty, l->kids[1], r)); \
    /* (x - c1) - c2 => x - (c1+c2) */ \
    if (r->op == CNST+TYPE && l->op == SUB+TYPE && l->kids[1]->op == CNST+TYPE) \
        return simplify(SUB, ty, l->kids[0], simplify(ADD, ty, r, l->kids[1])); \
    /* (c1 - x) - c2 => (c1-c2) - x */ \
    if (r->op == CNST+TYPE && l->op == SUB+TYPE && l->kids[0]->op == CNST+TYPE) \
        return simplify(SUB, ty, simplify(SUB, ty, l->kids[0], r), l->kids[1]); \
    /* (x + c1) - (y + c2) => (x - y) + (c1 - c2) */ \
    if (l->op == ADD+TYPE && l->kids[1]->op == CNST+TYPE && r->op == ADD+TYPE && r->kids[1]->op == CNST+TYPE) \
        return simplify(ADD, ty, simplify(SUB, ty, l->kids[0], r->kids[0]), simplify(SUB, ty, l->kids[1], r->kids[1])); \
    /* (x + c1) - (y - c2) => (x - y) + (c1 + c2) */ \
    if (l->op == ADD+TYPE && l->kids[1]->op == CNST+TYPE && r->op == SUB+TYPE && r->kids[1]->op == CNST+TYPE) \
        return simplify(ADD, ty, simplify(SUB, ty, l->kids[0], r->kids[0]), simplify(ADD, ty, l->kids[1], r->kids[1])); \
    /* (x - c1) - (y + c2) => (x - y) - (c1 + c2) */ \
    if (l->op == SUB+TYPE && l->kids[1]->op == CNST+TYPE && r->op == ADD+TYPE && r->kids[1]->op == CNST+TYPE) \
        return simplify(SUB, ty, simplify(SUB, ty, l->kids[0], r->kids[0]), simplify(ADD, ty, r->kids[1], l->kids[1])); \
    /* (x - c1) - (y - c2) => (x - y) - (c1 - c2) */ \
    if (l->op == SUB+TYPE && l->kids[1]->op == CNST+TYPE && r->op == SUB+TYPE && r->kids[1]->op == CNST+TYPE) \
        return simplify(SUB, ty, simplify(SUB, ty, l->kids[0], r->kids[0]), simplify(SUB, ty, l->kids[1], r->kids[1]));

/* simplifications for SUB+F, SUB+I or SUB+U */
#define simpsub(TYPE) \
    /* x - x => 0 */ \
    if (l->op == INDIR+TYPE && isaddrop(l->kids[0]->op) && \
        r->op == INDIR+TYPE && isaddrop(r->kids[0]->op) && l->kids[0]->u.sym == r->kids[0]->u.sym) \
    { \
        l->kids[0]->u.sym->ref -= (refinc + refinc); \
        return cnst_tree(ty, (uintmax_t)0); \
    } \
    /* x - (-y) => x + y */ \
    if (r->op == NEG+TYPE) \
        return simplify(ADD, ty, l, r->kids[0]); \
    /* (-x) - y => -(x + y) */ \
    if (l->op == NEG+TYPE) \
        return simplify(NEG, ty, simplify(ADD, ty, l->kids[0], r), NULL); \
    /* x - (y - z) => (x - y) + z */ \
    if (l->op == INDIR+TYPE && r->op == SUB+TYPE) \
        return simplify(ADD, ty, simplify(SUB, ty, l, r->kids[0]), r->kids[1]);

/* evaluate constant MUL+I or MUL+U */
#define foldmul(TYPE) \
    /* (x * c1) * c2 => x * (c1*c2) */ \
    if (r->op == CNST+TYPE && l->op == MUL+TYPE && l->kids[1]->op == CNST+TYPE) \
        return simplify(MUL, ty, l->kids[0], simplify(MUL, ty, l->kids[1], r)); \
    /* (x + c1) * c2 => (x*c2) + (c1*c2) */ \
    if (r->op == CNST+TYPE && l->op == ADD+TYPE && l->kids[1]->op == CNST+TYPE) \
        return simplify(ADD, ty, simplify(MUL, ty, l->kids[0], r), simplify(MUL, ty, l->kids[1], r)); \
    /* (x - c1) * c2 => (x*c2) - (c1*c2) */ \
    if (r->op == CNST+TYPE && l->op == SUB+TYPE && l->kids[1]->op == CNST+TYPE) \
        return simplify(SUB, ty, simplify(MUL, ty, l->kids[0], r), simplify(MUL, ty, l->kids[1], r));

/* simplifications for MUL+F, MUL+I or MUL+U */
#define simpmul(TYPE,VAR) \
    /* x * 0 => (x,0) */ \
    if (r->op == CNST+TYPE && r->u.v.VAR == 0) \
        return new_tree(RIGHT, ty, root(l), cnst_tree(ty, (intmax_t)0)); \
    /* x * (expr) => (expr) * x */ \
    if (l->op == INDIR+TYPE && r->op != INDIR+TYPE && r->op != CNST+TYPE) \
        return new_tree(MUL+TYPE, ty, r, l); \
    /* (x * c) * y => (x * y) * c */ \
    if (l->op == MUL+TYPE && l->kids[1]->op == CNST+TYPE && r->op == INDIR+TYPE) \
        return simplify(MUL, ty, simplify(MUL, ty, l->kids[0], r), l->kids[1]);

/* common simplifications for BAND+I, BAND+U */
#define foldband(TYPE,VAR) \
    /* X & 0 => (x,0) */ \
    if (r->op == CNST+TYPE && r->u.v.VAR == 0) \
        return new_tree(RIGHT, ty, root(l), cnst_tree(ty, (intmax_t)0)); \
    /* (x & c1) & c2 => x & (c1&c2) */ \
    if (r->op == CNST+TYPE && l->op == BAND+TYPE && l->kids[1]->op == CNST+TYPE) \
        return simplify(BAND, ty, l->kids[0], simplify(BAND, ty, l->kids[1], r));

/* common simplifications for BOR+I, BOR+U */
#define foldbor(TYPE) \
    /* (x | c1) | c2 => x | (c1|c2) */ \
    if (r->op == CNST+TYPE && l->op == BOR+TYPE && l->kids[1]->op == CNST+TYPE) \
        return simplify(BOR, ty, l->kids[0], simplify(BOR, ty, l->kids[1], r));

/* constant type conversion */
#define cvtcnst(TYPE,SRC,DST,VAR,EXPR) \
    if (l->op == CNST+TYPE) \
    { \
        if (!explicit_cast && \
            ((SRC) < DST->u.sym->u.limits.min.VAR || (SRC) > DST->u.sym->u.limits.max.VAR)) \
            apperror(RCWARNING1(ERROR_OVERFLOW_IN_CONVERSION), l->type, DST);\
        if (need_const || explicit_cast || \
            !((SRC) < DST->u.sym->u.limits.min.VAR || (SRC) > DST->u.sym->u.limits.max.VAR)) \
            return cnst_tree(ty, (EXPR)); \
    }

/* remove operation without effect (x*1, x+0, ...) */
#define identity(X,Y,TYPE,VAR,VAL) \
    if (X->op == CNST+TYPE && X->u.v.VAR == VAL) \
        return Y

/* bitfield compare against zero */
#define zerofield(OP,TYPE,VAR) \
    if (l->op == AFIELD && \
        r->op == CNST+TYPE && r->u.v.VAR == 0) \
        return equality_tree(OP, bit_tree(BAND, l->kids[0],\
            cnst_tree(unsignedtype, (uintmax_t)fieldmask(l->u.field)<<fieldright(l->u.field))), r)

#define geu(L,R,V) \
    if (R->op == CNST+U && R->u.v.u == 0 && L->op != CNST+U) \
    { \
        apperror(RCWARNING1(ERROR_UNSIGNED_COMP_IS_CONST)); \
        return new_tree(RIGHT, inttype, root(L), cnst_tree(inttype, (intmax_t)(V))); \
    }

#define idempotent(OP) \
    if (l->op == OP) return l->kids[0]

#define condtest(X,TYPE,VAR) \
    if (X->op == COND && \
        X->kids[1]->op == RIGHT && \
        X->kids[1]->kids[0]->op == ASGN+TYPE && \
        X->kids[1]->kids[0]->kids[1]->op == CNST+TYPE && \
        X->kids[1]->kids[0]->kids[1]->u.v.VAR == 1 && \
        X->kids[1]->kids[1]->op == ASGN+TYPE && \
        X->kids[1]->kids[1]->kids[1]->op == CNST+TYPE && \
        X->kids[1]->kids[1]->kids[1]->u.v.VAR == 0) \
        return simplify(X->kids[0]->op, ty, X->kids[0]->kids[0], X->kids[0]->kids[1])

/* Globals */
bool_t need_const;
bool_t explicit_cast;

/* Static function prototypes */
static TREE *addr_tree(TREE *, long, TYPE *);
static bool_t addi(intmax_t, intmax_t, intmax_t, intmax_t, bool_t);
static bool_t addd(double, double, double, double, bool_t);
static bool_t subi(intmax_t, intmax_t, intmax_t, intmax_t, bool_t);
static bool_t subd(double, double, double, double, bool_t);
static bool_t muli(intmax_t, intmax_t, intmax_t, intmax_t, bool_t);
static bool_t muld(double, double, double, double, bool_t);
static bool_t divi(intmax_t, intmax_t, intmax_t, intmax_t, bool_t);
static bool_t divd(double, double, double, double, bool_t);
static bool_t lshi(intmax_t, uintmax_t, int, bool_t);
static int ispow2(uintmax_t);
static int msb(uintmax_t);

/****************************************************************************
 *                                                                          *
 * Function: simplify                                                       *
 *                                                                          *
 * Purpose : Simplify and fold constants, if possible.                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-06-22  Bugfix: must sign-extend value in LSH+I.             *
 *           04-10-15  Bugfix: didn't warn about 1<<31, but 128<<24.        *
 *           04-12-07  Added serveral formal identity cases.                *
 *           04-12-18  Reorganize expressions for better int/float code.    *
 *                                                                          *
 ****************************************************************************/

TREE *simplify(int op, TYPE *ty, TREE *l, TREE *r)
{
    int n;

    if (optype(op) == 0)
        op = mkop(op, ty);

    switch (op)
    {
        case CBOOL+U:
            ufoldcnst(I, cnst_tree(ty, (uintmax_t)(l->u.v.i != 0)));
            ufoldcnst(U, cnst_tree(ty, (uintmax_t)(l->u.v.u != 0)));
            ufoldcnst(F, cnst_tree(ty, (uintmax_t)(l->u.v.d != 0.0)));  /* 04-12-27 */
            break;

        case CVI+I:
            cvtcnst(I, l->u.v.i, ty, i, (intmax_t)extend(l->u.v.i,ty));
            break;

        case CVU+I:
            if (l->op == CNST+U)
            {
                if (!explicit_cast && l->u.v.u > (uintmax_t)ty->u.sym->u.limits.max.i && !options.microsoft)  /* 04-07-23 */
                    apperror(RCWARNING2(ERROR_OVERFLOW_IN_CONVERSION), l->type, ty);
                if (need_const || explicit_cast || !(l->u.v.u > (uintmax_t)ty->u.sym->u.limits.max.i))
                    return cnst_tree(ty, (intmax_t)extend(l->u.v.u,ty));
                else if (options.microsoft)  /* 04-07-23 */
                    apperror(RCWARNING2(ERROR_OVERFLOW_IN_CONVERSION), l->type, ty);
            }
            break;

        case CVP+U:
            cvtcnst(P, ((uintmax_t)l->u.v.p)&ones(8*ty->size), ty, u, (uintmax_t)l->u.v.p);
            break;

        case CVU+P:
            cvtcnst(U, (void*)(ulong_t)l->u.v.u, ty, p, (void*)(ulong_t)l->u.v.u);
            break;

        case CVP+P:
            cvtcnst(P, l->u.v.p, ty, p, l->u.v.p);
            break;

        case CVI+U:
            cvtcnst(I, ((uintmax_t)l->u.v.i)&ones(8*ty->size), ty, u, ((uintmax_t)l->u.v.i)&ones(8*ty->size));
            break;

        case CVU+U:
            cvtcnst(U, l->u.v.u, ty, u, l->u.v.u&ones(8*ty->size));
            break;

        case CVI+F:
            cvtcnst(I, l->u.v.i, ty, d, (long double)l->u.v.i);
            break;

#if 0
        /* Microsoft VC++ can't handle this! - but no one seems to call us anyway?! */
        case CVU+F:
            cvtcnst(U, l->u.v.u, ty, d, (long double)l->u.v.u);
            break;
#endif

        case CVF+I:
            cvtcnst(F, l->u.v.d, ty, i, (intmax_t)l->u.v.d);
            break;

        case CVF+F:
        {
            float d = 0;
            if (l->op == CNST+F)
            {
                if (l->u.v.d < ty->u.sym->u.limits.min.d)
                    d = (float)ty->u.sym->u.limits.min.d;
                else if (l->u.v.d > ty->u.sym->u.limits.max.d)
                    d = (float)ty->u.sym->u.limits.max.d;
                else
                    d = (float)l->u.v.d;
            }
            cvtcnst(F, l->u.v.d, ty, d, (long double)d);
            break;
        }

        case ADD+F:
            foldcnstlim(F,d,+,addd);
            commute(l,r);
            /* dangerous evaluating float constants? */
            simpadd(F);
            break;

        case ADD+I:
            foldcnstlim(I,i,+,addi);
            commute(l,r);
            identity(r,l,I,i,0);
            foldadd(I);
            simpadd(I);
            break;

        case ADD+U:
            foldcnst(U,u,+);
            commute(l,r);
            identity(r,l,U,u,0);
            foldadd(U);
            simpadd(U);
            break;

        case ADD+P:
            foldaddp(l,r,I,i);
            foldaddp(l,r,U,u);
            foldaddp(r,l,I,i);
            foldaddp(r,l,U,u);
            commute(l,r);
            identity(r,retype(l,ty),I,i,0);
            identity(r,retype(l,ty),U,u,0);
            if (isaddrop(l->op) &&
                (r->op == CNST+I && r->u.v.i <= longtype->u.sym->u.limits.max.i && r->u.v.i >= longtype->u.sym->u.limits.min.i ||
                 r->op == CNST+U && r->u.v.u <= (uintmax_t)longtype->u.sym->u.limits.max.i))
                return addr_tree(l, (long)cast(r, longtype)->u.v.i, ty);

            if (l->op == ADD+P && isaddrop(l->kids[1]->op) &&
                (r->op == CNST+I && r->u.v.i <= longtype->u.sym->u.limits.max.i && r->u.v.i >= longtype->u.sym->u.limits.min.i ||
                 r->op == CNST+U && r->u.v.u <= (uintmax_t)longtype->u.sym->u.limits.max.i))
                return simplify(ADD+P, ty, l->kids[0], addr_tree(l->kids[1], (long)cast(r, longtype)->u.v.i, ty));

            if ((l->op == ADD+I || l->op == SUB+I) && l->kids[1]->op == CNST+I && isaddrop(r->op))
                return simplify(ADD+P, ty, l->kids[0], simplify(generic(l->op)+P, ty, r, l->kids[1]));

            if (l->op == ADD+P && generic(l->kids[1]->op) == CNST && generic(r->op) == CNST)
                return simplify(ADD+P, ty, l->kids[0], simplify(ADD, l->kids[1]->type, l->kids[1], r));

            if (l->op == ADD+I && generic(l->kids[1]->op) == CNST &&
                r->op == ADD+P && generic(r->kids[1]->op) == CNST)
                return simplify(ADD+P, ty, l->kids[0], simplify(ADD+P, ty, r->kids[0], simplify(ADD, r->kids[1]->type, l->kids[1], r->kids[1])));

            if (l->op == RIGHT && l->kids[1])
                return new_tree(RIGHT, ty, l->kids[0], simplify(ADD+P, ty, l->kids[1], r));
            else if (l->op == RIGHT && l->kids[0])
                return new_tree(RIGHT, ty, simplify(ADD+P, ty, l->kids[0], r), NULL);
            break;

        case AND+I:
            op = AND;
            /* 0&&r => 0, 1&&r => r */
            ufoldcnst(I, l->u.v.i ? cond(r) : l);
            break;

        case OR+I:
            op = OR;
            /* 0||r => r, 1||r => 1 */
            ufoldcnst(I, l->u.v.i ? cnst_tree(ty, (intmax_t)1) : cond(r));
            break;

        case BCOM+I:
            ufoldcnst(I, cnst_tree(ty, (intmax_t)extend((~l->u.v.i)&ones(8*ty->size), ty)));
            idempotent(BCOM+U);
            break;

        case BCOM+U:
            ufoldcnst(U, cnst_tree(ty, (uintmax_t)((~l->u.v.u)&ones(8*ty->size))));
            idempotent(BCOM+U);
            break;

        case BAND+I:
            foldcnst(I,i,&);
            commute(l,r);
            identity(r,l,I,i,(int)ones(8*ty->size));
            foldband(I,i);
            break;

        case BAND+U:
            foldcnst(U,u,&);
            commute(l,r);
            identity(r,l,U,u,ones(8*ty->size));
            foldband(U,u);
            break;

        case BOR+I:
            foldcnst(I,i,|);
            commute(l,r);
            identity(r,l,I,i,0);
            foldbor(I);
            break;

        case BOR+U:
            foldcnst(U,u,|);
            commute(l,r);
            identity(r,l,U,u,0);
            foldbor(U);
            break;

        case BXOR+I:
            foldcnst(I,i,^);
            commute(l,r);
            identity(r,l,I,i,0);
            break;

        case BXOR+U:
            foldcnst(U,u,^);
            commute(l,r);
            identity(r,l,U,u,0);
            break;

        case DIV+F:
            foldcnstlim(F,d,/,divd);
            break;

        case DIV+I:
            identity(r,l,I,i,1);
            identity(r,simplify(NEG,ty,l,NULL),I,i,-1);
            if (r->op == CNST+I && r->u.v.i == 0 ||
                l->op == CNST+I && l->u.v.i == ty->u.sym->u.limits.min.i &&
                r->op == CNST+I && r->u.v.i == -1)
                break;
            foldcnstlim(I,i,/,divi);
            /* x / (-y) => -(x / y) */
            if (r->op == NEG+I)
                return simplify(NEG, ty, simplify(DIV, ty, l, r->kids[0]), NULL);  /* added 04-12-07 */
            /* (-x) / y => -(x / y) */
            if (l->op == NEG+I)
                return simplify(NEG, ty, simplify(DIV, ty, l->kids[0], r), NULL);  /* added 04-12-07 */
            break;

        case DIV+U:
            identity(r,l,U,u,1);
            if (r->op == CNST+U && r->u.v.u == 0)
                break;
            if (r->op == CNST+U && (n = ispow2(r->u.v.u)) != 0)
                return simplify(RSH, ty, l, cnst_tree(inttype, (intmax_t)n));
            foldcnst(U,u,/);
            break;

        case EQ+F:
            foldcnstcond(F,d,==);
            commute(l,r);
            break;

        case EQ+I:
            foldcnstcond(I,i,==);
            commute(l,r);
            zerofield(EQ,I,i);
            break;

        case EQ+U:
            foldcnstcond(U,u,==);
            commute(l,r);
            zerofield(EQ,U,u);
            break;

        case GE+F:
            foldcnstcond(F,d,>=);
            break;

        case GE+I:
            foldcnstcond(I,i,>=);
            commutecond(l,r,LE+I);
            break;

        case GE+U:
            geu(l,r,1);  /* l >= 0 => (l,1) */
            foldcnstcond(U,u,>=);
            commutecond(l,r,LE+U);
            if (l->op == CNST+U && l->u.v.u == 0)  /* 0 >= r => r == 0 */
                return equality_tree(EQ, r, l);
            break;

        case GT+F:
            foldcnstcond(F,d,>);
            break;

        case GT+I:
            foldcnstcond(I,i,>);
            commutecond(l,r,LT+I);
            break;

        case GT+U:
            geu(r,l,0);  /* 0 > r => (r,0) */
            foldcnstcond(U,u, >);
            commutecond(l,r,LT+U);
            if (r->op == CNST+U && r->u.v.u == 0)  /* l > 0 => l != 0 */
                return equality_tree(NE, l, r);
            break;

        case LE+F:
            foldcnstcond(F,d,<=);
            break;

        case LE+I:
            foldcnstcond(I,i,<=);
            commutecond(l,r,GE+I);
            break;

        case LE+U:
            geu(r,l,1);  /* 0 <= r => (r,1) */
            foldcnstcond(U,u,<=);
            commutecond(l,r,GE+U);
            if (r->op == CNST+U && r->u.v.u == 0)  /* l <= 0 => l == 0 */
                return equality_tree(EQ, l, r);
            break;

        case LSH+I:
            identity(r,l,I,i,0);
            if (l->op == CNST+I && r->op == CNST+I && r->u.v.i >= 0 && r->u.v.i < 8*l->type->size &&
                lshi(l->u.v.i, 1U<<r->u.v.i, 8*l->type->size, need_const))  /* bugfix 04-10-15 */
                return cnst_tree(ty, (intmax_t)extend(l->u.v.i<<r->u.v.i,ty));  /* bugfix 04-06-22 */
            if (r->op == CNST+I && (r->u.v.i >= 8*ty->size || r->u.v.i < 0))
                apperror(RCWARNING1(ERROR_SHIFTING_X_BITS_IS_UNDEF), ty, r->u.v.i);
            break;

        case LSH+U:
            identity(r,l,I,i,0);
            sfoldcnst(<<);
            if (r->op == CNST+I && (r->u.v.i >= 8*ty->size || r->u.v.i < 0))
                apperror(RCWARNING1(ERROR_SHIFTING_X_BITS_IS_UNDEF), ty, r->u.v.i);
            break;

        case LT+F:
            foldcnstcond(F,d,<);
            break;

        case LT+I:
            foldcnstcond(I,i,<);
            commutecond(l,r,GT+I);
            break;

        case LT+U:
            geu(l,r,0);  /* l < 0 => (l,0) */
            foldcnstcond(U,u, <);
            commutecond(l,r,GT+U);
            if (l->op == CNST+U && l->u.v.u == 0)  /* 0 < r => r != 0 */
                return equality_tree(NE, r, l);
            break;

        case MOD+I:
            if (r->op == CNST+I && r->u.v.i == 0 ||
                l->op == CNST+I && l->u.v.i == ty->u.sym->u.limits.min.i &&
                r->op == CNST+I && r->u.v.i == -1)
                break;
            foldcnstlim(I,i,%,divi);
            if (r->op == CNST+I && r->u.v.i == 1)  /* l%1 => (l,0) */
                return new_tree(RIGHT, ty, root(l), cnst_tree(ty, (intmax_t)0));
            break;

        case MOD+U:
            if (r->op == CNST+U && ispow2(r->u.v.u))  /* l%2^n => l&(2^n-1) */
                return bit_tree(BAND, l, cnst_tree(ty, r->u.v.u - 1));
            if (r->op == CNST+U && r->u.v.u == 0)
                break;
            foldcnst(U,u,%);
            break;

        case MUL+F:
            foldcnstlim(F,d,*,muld);
            commute(l,r);
            simpmul(F,d);
            break;

        case MUL+I:
            commute(l,r);
            foldcnstlim(I,i,*,muli);
            foldmul(I);
            simpmul(I,i);
            /* x * 2^n => x<<n */
            if (r->op == CNST+I && r->u.v.i > 0 && (n = ispow2(r->u.v.i)) != 0)
                return simplify(LSH, ty, l, cnst_tree(inttype, (intmax_t)n));
            identity(r,l,I,i,1);
            identity(r,simplify(NEG,ty,l,NULL),I,i,-1);
            /* x * (-y) => -(x * y) */
            if (r->op == NEG+I)
                return simplify(NEG, ty, simplify(MUL, ty, l, r->kids[0]), NULL);  /* added 04-12-07 */
            /* (-x) * y => -(x * y) */
            if (l->op == NEG+I)
                return simplify(NEG, ty, simplify(MUL, ty, l->kids[0], r), NULL);  /* added 04-12-07 */
            break;

        case MUL+U:
            commute(l,r);
            foldmul(U);
            simpmul(U,u);
            /* x * 2^n => x<<n */
            if (r->op == CNST+U && (n = ispow2(r->u.v.u)) != 0)
                return simplify(LSH, ty, l, cnst_tree(inttype, (intmax_t)n));
            foldcnst(U,u,*);
            identity(r,l,U,u,1);
            break;

        case NE+F:
            foldcnstcond(F,d,!=);
            commute(l,r);
            break;

        case NE+I:
            foldcnstcond(I,i,!=);
            commute(l,r);
            zerofield(NE,I,i);
            /* ((e ? 1 : 0) != 0) => e */
            if (r->op == CNST+I && r->u.v.i == 0)  /* added 04-08-25 */
            {
                if (l->op == COND)
                {
                    condtest(l,I,i);
                }
                else if (l->op == RIGHT && !l->kids[0] && l->kids[1] && l->kids[1]->op == CVI+I)
                {
                    condtest(l->kids[1]->kids[0],I,i);
                }
            }
            break;

        case NE+U:
            foldcnstcond(U,u,!=);
            commute(l,r);
            zerofield(NE,U,u);
            break;

        case NEG+F:
            ufoldcnst(F, cnst_tree(ty, -l->u.v.d));
            idempotent(NEG+F);
            break;

        case NEG+I:
            if (l->op == CNST+I)
            {
                if (need_const && l->u.v.i == ty->u.sym->u.limits.min.i)
                    apperror(RCWARNING1(ERROR_OVERFLOW_IN_CONST_EXPR));
                if (need_const || l->u.v.i != ty->u.sym->u.limits.min.i)
                    return cnst_tree(ty, -l->u.v.i);
            }
            idempotent(NEG+I);
            break;

        case NOT+I:
            op = NOT;
            ufoldcnst(I, cnst_tree(ty, (intmax_t)!l->u.v.i));
            break;

        case RSH+I:
            identity(r,l,I,i,0);
            if (l->op == CNST+I && r->op == CNST+I &&
                r->u.v.i >= 0 && r->u.v.i < 8*l->type->size)
            {
                intmax_t n = l->u.v.i>>r->u.v.i;
                if (l->u.v.i < 0)
                    n |= ~0UL<<(8*l->type->size - r->u.v.i);
                return cnst_tree(ty, n);
            }
            if (r->op == CNST+I && (r->u.v.i >= 8*ty->size || r->u.v.i < 0))
                apperror(RCWARNING1(ERROR_SHIFTING_X_BITS_IS_UNDEF), ty, r->u.v.i);
            break;

        case RSH+U:
            identity(r,l,I,i,0);
            sfoldcnst(>>);
            if (r->op == CNST+I && (r->u.v.i >= 8*ty->size || r->u.v.i < 0))
                apperror(RCWARNING1(ERROR_SHIFTING_X_BITS_IS_UNDEF), ty, r->u.v.i);
            break;

        case SUB+F:
            foldcnstlim(F,d,-,subd);
            simpsub(F);
            break;

        case SUB+I:
            foldcnstlim(I,i,-,subi);
            identity(r,l,I,i,0);
            foldsub(I);
            simpsub(I);
            /* 0 - x => -x */
            if (l->op == CNST+I && l->u.v.i == 0)
                return simplify(NEG, ty, r, NULL);  /* added 04-12-07 */
            break;

        case SUB+U:
            foldcnst(U,u,-);
            identity(r,l,U,u,0);
            foldsub(U);
            simpsub(U);
            break;

        case SUB+P:
            if (l->op == CNST+P && r->op == CNST+P)
                return cnst_tree(ty, (intmax_t)((char *)l->u.v.p - (char *)r->u.v.p));
            if (r->op == CNST+I || r->op == CNST+U)
                return simplify(ADD, ty, l, cnst_tree(inttype, (r->op == CNST+I ? -r->u.v.i : -(intmax_t)r->u.v.u)));
            /* l - (x + c) => l - c - x */
            if (isaddrop(l->op) && r->op == ADD+I && r->kids[1]->op == CNST+I)
                return simplify(SUB, ty, simplify(SUB, ty, l, r->kids[1]), r->kids[0]);
            break;

        default:
            assert(0);
    }

    return new_tree(op, ty, l, r);
}

/****************************************************************************
 *                                                                          *
 * Function: addr_tree                                                      *
 *                                                                          *
 * Purpose : Construct tree for the address of e plus offset n.             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static TREE *addr_tree(TREE *e, long n, TYPE *ty)
{
    SYMBOL *sym = e->u.sym;
    SYMBOL *p;

    if (sym->scope  == GLOBAL || sym->sclass == STATIC || sym->sclass == EXTERN)
    {
        p = memalloc(sizeof(*p), PERM);
        memset(p, 0, sizeof(*p));
    }
    else
    {
        p = memalloc(sizeof(*p), funca);
        memset(p, 0, sizeof(*p));
    }

    p->name = stringd(make_label(1));
    p->sclass = sym->sclass;
    p->scope = sym->scope;
    assert(isptr(ty) || isarray(ty));
    p->type = isptr(ty) ? ty->type : ty;
    p->temporary = sym->temporary;
    p->generated = sym->generated;
    p->addressed = sym->addressed;
    p->computed = TRUE;
    p->defined = TRUE;
    p->ref = 1;

    if (sym->scope  == GLOBAL ||  sym->sclass == STATIC || sym->sclass == EXTERN)
    {
        if (sym->sclass == AUTO)
            p->sclass = STATIC;

        (*IR->address)(p, sym, n);
    }
    else
    {
        CODE *cp;

        new_local_var(sym);

        cp = new_code(CODE_ADDRESS);
        cp->u.addr.sym = p;
        cp->u.addr.base = sym;
        cp->u.addr.offset = n;
    }

    e = new_tree(e->op, ty, NULL, NULL);
    e->u.sym = p;

    return e;
}

/****************************************************************************
 *                                                                          *
 * Function: addi                                                           *
 *                                                                          *
 * Purpose : Return true if min <= x+y <= max, false otherwise.             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static bool_t addi(intmax_t x, intmax_t y, intmax_t min, intmax_t max, bool_t need_const)
{
    bool_t cond = (x == 0 || y == 0 ||
        x < 0 && y < 0 && x >= min - y ||
        x < 0 && y > 0 ||
        x > 0 && y < 0 ||
        x > 0 && y > 0 && x <= max - y);

    if (!cond && need_const)
    {
        apperror(RCWARNING1(ERROR_OVERFLOW_IN_CONST_EXPR));
        cond = TRUE;
    }

    return cond;
}

/****************************************************************************
 *                                                                          *
 * Function: addd                                                           *
 *                                                                          *
 * Purpose : Return true if min <= x+y <= max, false otherwise.             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static bool_t addd(double x, double y, double min, double max, bool_t need_const)
{
    bool_t cond = (x == 0 || y == 0 ||
        x < 0 && y < 0 && x >= min - y ||
        x < 0 && y > 0 ||
        x > 0 && y < 0 ||
        x > 0 && y > 0 && x <= max - y);

    if (!cond && need_const)
    {
        apperror(RCWARNING1(ERROR_OVERFLOW_IN_CONST_EXPR));
        cond = TRUE;
    }

    return cond;
}

/****************************************************************************
 *                                                                          *
 * Function: subi                                                           *
 *                                                                          *
 * Purpose : Return true if min <= x-y <= max, false otherwise.             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static bool_t subi(intmax_t x, intmax_t y, intmax_t min, intmax_t max, bool_t need_const)
{
    return addi(x, -y, min, max, need_const);
}

/****************************************************************************
 *                                                                          *
 * Function: subd                                                           *
 *                                                                          *
 * Purpose : Return true if min <= x-y <= max, false otherwise.             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static bool_t subd(double x, double y, double min, double max, bool_t need_const)
{
    return addd(x, -y, min, max, need_const);
}

/****************************************************************************
 *                                                                          *
 * Function: muli                                                           *
 *                                                                          *
 * Purpose : Return true if min <= x*y <= max, false otherwise.             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static bool_t muli(intmax_t x, intmax_t y, intmax_t min, intmax_t max, bool_t need_const)
{
    bool_t cond = (x > -1 && x <= 1 || y > -1 && y <= 1 ||
        x < 0 && y < 0 && -x <= max/-y ||
        x < 0 && y > 0 &&  x >= min/y ||
        x > 0 && y < 0 &&  y >= min/x ||
        x > 0 && y > 0 &&  x <= max/y);

    if (!cond && need_const)
    {
        apperror(RCWARNING1(ERROR_OVERFLOW_IN_CONST_EXPR));
        cond = TRUE;
    }

    return cond;
}

/****************************************************************************
 *                                                                          *
 * Function: muld                                                           *
 *                                                                          *
 * Purpose : Return true if min <= x*y <= max, false otherwise.             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static bool_t muld(double x, double y, double min, double max, bool_t need_const)
{
    bool_t cond = (x >= -1 && x <= 1 || y >= -1 && y <= 1 ||
        x < 0 && y < 0 && -x <= max/-y ||
        x < 0 && y > 0 &&  x >= min/y ||
        x > 0 && y < 0 &&  y >= min/x ||
        x > 0 && y > 0 &&  x <= max/y);

    if (!cond && need_const)
    {
        apperror(RCWARNING1(ERROR_OVERFLOW_IN_CONST_EXPR));
        cond = TRUE;
    }

    return cond;
}

/****************************************************************************
 *                                                                          *
 * Function: divi                                                           *
 *                                                                          *
 * Purpose : Return true if min <= x/y <= max, false otherwise.             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static bool_t divi(intmax_t x, intmax_t y, intmax_t min, intmax_t max, bool_t need_const)
{
    bool_t cond = (y != 0 && !(x == min && y == -1));

    if (!cond && need_const)
    {
        apperror(RCWARNING1(ERROR_OVERFLOW_IN_CONST_EXPR));
        cond = TRUE;
    }

    return cond;
}

/****************************************************************************
 *                                                                          *
 * Function: divd                                                           *
 *                                                                          *
 * Purpose : Return true if min <= x/y <= max, false otherwise.             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static bool_t divd(double x, double y, double min, double max, bool_t need_const)
{
    bool_t cond;

    if (x < 0) x = -x;
    if (y < 0) y = -y;

    cond = (y != 0 && !(y < 1 && x > max*y));
    if (!cond && need_const)
    {
        apperror(RCWARNING1(ERROR_OVERFLOW_IN_CONST_EXPR));
        cond = TRUE;
    }

    return cond;
}

/****************************************************************************
 *                                                                          *
 * Function: lshi                                                           *
 *                                                                          *
 * Purpose : Return true if result is representable, false otherwise.       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-10-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

static bool_t lshi(intmax_t x, uintmax_t y, int bits, bool_t need_const)
{
    bool_t cond = (x == 0 || y == 0 || (msb(x) + msb(y)) < bits);

    if (!cond && need_const)
    {
        apperror(RCWARNING1(ERROR_OVERFLOW_IN_CONST_EXPR));
        cond = TRUE;
    }

    return cond;
}

/****************************************************************************
 *                                                                          *
 * Function: ispow2                                                         *
 *                                                                          *
 * Purpose : If u > 1 && u == 2^n, return n, otherwise return 0.            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int ispow2(uintmax_t u)
{
    int n;

    if (u > 1 && (u & (u-1)) == 0)
    {
        for (n = 0; u != 0; u >>= 1, n++)
            if (u & 1) return n;
    }

    return 0;
}

/****************************************************************************
 *                                                                          *
 * Function: msb                                                            *
 *                                                                          *
 * Purpose : Return index of the higest bit set (from zero).                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-10-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int msb(uintmax_t u)
{
    int n = 0;

    if (u & 0xFFFFFFFF00000000UI64) { u >>= 32;  n += 32; }
    if (u & 0x00000000FFFF0000UI64) { u >>= 16;  n += 16; }
    if (u & 0x000000000000FF00UI64) { u >>=  8;  n +=  8; }
    if (u & 0x00000000000000F0UI64) { u >>=  4;  n +=  4; }
    if (u & 0x000000000000000CUI64) { u >>=  2;  n +=  2; }
    if (u & 0x0000000000000002UI64) {            n +=  1; }

    return n;
}
