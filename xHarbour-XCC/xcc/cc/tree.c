/****************************************************************************
 *                                                                          *
 * File    : tree.c                                                         *
 *                                                                          *
 * Purpose : ISO C Compiler; Front-end intermediate code management.        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "lcc.h"

static uint_t where = STMT;
static int warn = 0;

#if defined(PODEBUG) || defined(PRERELEASE)
static int nid = 1;  /* identifies trees & nodes in debugging output */
static struct _NODEID {
    bool_t printed;
    TREE *node;
} ids[500];     /* if ids[i].node == e, then e's id is i */
#endif

/* Static function prototypes */
static TREE *root1(TREE *);
static bool_t hascond(TREE *);
static void printtree1(TREE *, int);

/****************************************************************************
 *                                                                          *
 * Function: new_tree                                                       *
 *                                                                          *
 * Purpose : Allocate a new tree node.                                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

TREE *new_tree(int op, TYPE *type, TREE *l, TREE *r)
{
    TREE *e;

    e = memalloc(sizeof(*e), where);
    memset(e, 0, sizeof(*e));

    e->op = op;
    e->type = type;
    e->kids[0] = l;
    e->kids[1] = r;

    return e;
}

/****************************************************************************
 *                                                                          *
 * Function: tree_to_arena                                                  *
 *                                                                          *
 * Purpose : Copy tree and allocate in given arena.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           03-08-14  Created                                              *
 *                                                                          *
 ****************************************************************************/

TREE *tree_to_arena(TREE *e, uint_t a)
{
    if (e)
    {
        TREE *p = memalloc(sizeof(*p), a);
        memset(p, 0, sizeof(*p));

        assert(!e->node);
        p->op = e->op;
        p->type = e->type;
        p->kids[0] = tree_to_arena(e->kids[0], a);
        p->kids[1] = tree_to_arena(e->kids[1], a);
        p->u.v = e->u.v;  /* worst case */

        e = p;
    }

    return e;
}

/****************************************************************************
 *                                                                          *
 * Function: expr_in_arena                                                  *
 *                                                                          *
 * Purpose : Parse expression and allocate in given arena.                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

TREE *expr_in_arena(TREE *(*f)(int), int token, uint_t a)
{
    uint_t save = where;
    TREE *e;

    where = a;
    e = (*f)(token);
    where = save;

    return e;
}

/****************************************************************************
 *                                                                          *
 * Function: root                                                           *
 *                                                                          *
 * Purpose : Return only the tree that has a side effect. See root1().      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-03-14  Optimizations of float assignments added.            *
 *                                                                          *
 ****************************************************************************/

TREE *root(TREE *e)
{
    warn = (nerrs) ? 1 : 0;

    e = root1(e);

    if (options.pragmaopt && e && e->op == ASGN+F && e->type->size == inttype->size)
    {
        if (e->kids[1]->op == CNST+F)
        {
            float f = (float)(double)e->kids[1]->u.v.d;

            /*
             * we force the new type into the tree, rather than rebuild the tree,
             * but it seems to work - and should be faster too.
             */
            e->type = inttype;
            e->op = ASGN+I;
            e->kids[0]->type = ptr(inttype);
            e->kids[1]->type = inttype;
            e->kids[1]->op = CNST+I;
            e->kids[1]->u.v.i = (intmax_t)*(uint_t *)&f;
        }
        else if (e->kids[1]->op == INDIR+F)
        {
            /*
             * we force the new type into the tree, rather than rebuild the tree,
             * but it seems to work - and should be faster too.
             */
            e->type = inttype;
            e->op = ASGN+I;
            e->kids[0]->type = ptr(inttype);
            e->kids[1]->type = inttype;
            e->kids[1]->op = INDIR+I;
        }
    }

    return e;
}

/****************************************************************************
 *                                                                          *
 * Function: root1                                                          *
 *                                                                          *
 * Purpose : Return only the tree that has a side effect.                   *
 *           "a + f()" is changed to "f()" since the addition is useless.   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           01-09-29  Bogus warnings removed (Nelson H. F. Beebe)          *
 *           04-07-15  Added operators INTRIN1{S}, INTRIN2{S}.              *
 *           04-09-09  Bugfix: avoid release of COND code for INTRIN2S.     *
 *           04-11-22  Bugfix: shut up warning for memset() intrinsic.      *
 *           04-12-21  Compensate reference increment from id_tree().       *
 *                                                                          *
 ****************************************************************************/

static TREE *root1(TREE *e)
{
    if (e == NULL)
        return e;

    if (e->type == voidtype)
        warn++;

    /* HACK! try to shut up warning for memset() intrinsic (04-11-22) */
    if (e->type == voidptype && e->kids[0] && generic(e->kids[0]->op) == ASGN)
        warn++;

    switch (generic(e->op))
    {
        case COND:
        {
            TREE *q = e->kids[1];

            assert(q && q->op == RIGHT);

            if (e->u.sym && q->kids[0] && generic(q->kids[0]->op) == ASGN)
                q->kids[0] = root1(q->kids[0]->kids[1]);
            else
                q->kids[0] = root1(q->kids[0]);

            if (e->u.sym && q->kids[1] && generic(q->kids[1]->op) == ASGN)
                q->kids[1] = root1(q->kids[1]->kids[1]);
            else
                q->kids[1] = root1(q->kids[1]);

            e->u.sym = 0;

            if (q->kids[0] == 0 && q->kids[1] == 0)
                e = root1(e->kids[0]);
            break;
        }

        case AND:
        case OR:
            if ((e->kids[1] = root1(e->kids[1])) == 0)
                e = root1(e->kids[0]);
            break;

        case NOT:
            if (warn++ == 0)
                apperror(RCWARNING2(ERROR_EXPRESSION_WO_EFFECT));
            return root1(e->kids[0]);

        case RIGHT:
            if (e->kids[1] == 0)
                return root1(e->kids[0]);

            if (e->kids[0] && e->kids[0]->op == CALL+B &&
                e->kids[1] && e->kids[1]->op == INDIR+B)
                /* avoid premature release of the CALL+B temporary */
                return e->kids[0];

            if (generic(e->kids[1]->op) == INTRIN2S && hascond(e->kids[0]))  /* 04-09-09 */
                /* avoid release of assignments in strcpy(buf, issome() ? "1" : "0") */
                return e;

            if (e->kids[0] && e->kids[0]->op == RIGHT &&
                e->kids[1] == e->kids[0]->kids[0])
                /* de-construct e++ construction */
                return e->kids[0]->kids[1];

            e = new_tree(RIGHT, e->type, root1(e->kids[0]), root1(e->kids[1]));
            return (e->kids[0] || e->kids[1]) ? e : NULL;

        case EQ:
        case NE:
        case GT:
        case GE:
        case LE:
        case LT:
        case ADD:
        case SUB:
        case MUL:
        case DIV:
        case MOD:
        case LSH:
        case RSH:
        case BAND:
        case BOR:
        case BXOR:
        case INTRIN1:
        case INTRIN2:
            if (warn++ == 0)
                apperror(RCWARNING2(ERROR_EXPRESSION_WO_EFFECT));
            e = new_tree(RIGHT, e->type, root1(e->kids[0]), root1(e->kids[1]));
            return (e->kids[0] || e->kids[1]) ? e : NULL;

        case INDIR:
            if (e->type->size == 0 && unqual(e->type) != voidtype)
                apperror(RCWARNING2(ERROR_REFERENCE_ELIDED), e->type);
            if (isptr(e->kids[0]->type) && isvolatile(e->kids[0]->type->type))
                apperror(RCWARNING2(ERROR_VOLATILE_REFERENCE_ELIDED), e->type);
            if (e->op == INDIR+B)  /* HACK! (05-01-02) */
                warn++;
            /* fall through */
        case NEG:
        case BCOM:
        case AFIELD:
            if (warn++ == 0)
                apperror(RCWARNING2(ERROR_EXPRESSION_WO_EFFECT));
            return root1(e->kids[0]);

        case ADDRL:
        case ADDRG:
        case ADDRF:
            if (!need_const)  /* 04-12-21 */
            {
                /* compensate increment in id_tree() */
                e->u.sym->ref -= refinc;
#ifdef PODEBUG
                printf("  symbol %s adjusted to %f in %s\n", e->u.sym->name, e->u.sym->ref, funcsym->name);
#endif
            }
            /* fall through */
        case CNST:
            if (need_const) return e;
            if (warn++ == 0)
                apperror(RCWARNING2(ERROR_EXPRESSION_WO_EFFECT));
            return NULL;

        case CVF:
            if (optype(e->op) == I || e->type->size < e->kids[0]->type->size)
            {
                if (warn++ == 0)
                    apperror(RCWARNING2(ERROR_EXPRESSION_WO_EFFECT));
            }
            return root1(e->kids[0]);

        case CVI:
            if ((optype(e->op) == U || optype(e->op) == I) && e->type->size < e->kids[0]->type->size &&
                specific(e->kids[0]->op) != CALL+I)
            {
                if (warn++ == 0)
                    apperror(RCWARNING2(ERROR_EXPRESSION_WO_EFFECT));
            }
            return root1(e->kids[0]);

        case CVU:
        case CVP:
            if (optype(e->op) == U && e->type->size <  e->kids[0]->type->size ||
                optype(e->op) == I && e->type->size <= e->kids[0]->type->size)
            {
                if (warn++ == 0)
                    apperror(RCWARNING2(ERROR_EXPRESSION_WO_EFFECT));
            }
            return root1(e->kids[0]);

        case CBOOL:
            return root1(e->kids[0]);

        case ARG:
        case ASGN:
        case CALL:
        case JUMP:
        case LABEL:
        case INTRIN1S:  /* with side effects */
        case INTRIN2S:  /* with side effects */
            break;

        default:
            assert(0);
    }

    return e;
}

/****************************************************************************
 *                                                                          *
 * Function: hascond                                                        *
 *                                                                          *
 * Purpose : Return true if the expression contains a COND.                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-09-09  Created                                              *
 *                                                                          *
 ****************************************************************************/

static bool_t hascond(TREE *e)
{
    if (e == NULL)
        return FALSE;

    if (generic(e->op) == COND)
        return TRUE;

    return hascond(e->kids[0]) || hascond(e->kids[1]);
}

/****************************************************************************
 *                                                                          *
 * Function: opname                                                         *
 *                                                                          *
 * Purpose : Return operator name with type suffix.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-03-08  Added operators CEQ, CGE, CGT, CLE, CLT, CNE.        *
 *           04-07-15  Added operators INTRIN1{S}, INTRIN2{S}.              *
 *                                                                          *
 ****************************************************************************/

char *opname(int op)
{
    static char *opnames[] = {
        "",
        "CNST",
        "ARG",
        "ASGN",
        "INDIR",
        "CVC",
        "CVD",
        "CVF",
        "CVI",
        "CVP",
        "CVS",
        "CVU",
        "NEG",
        "CALL",
        "LOAD" /*"*LOAD*"*/,
        "RET",
        "ADDRG",
        "ADDRF",
        "ADDRL",
        "ADD",
        "SUB",
        "LSH",
        "MOD",
        "RSH",
        "BAND",
        "BCOM",
        "BOR",
        "BXOR",
        "DIV",
        "MUL",
        "EQ",
        "GE",
        "GT",
        "LE",
        "LT",
        "NE",
        "JUMP",
        "LABEL",
        "CBOOL",
        "CEQ",
        "CGE",
        "CGT",
        "CLE",
        "CLT",
        "CNE",
        "INTRIN1",
        "INTRIN1S",
        "INTRIN2",
        "INTRIN2S",
        "AND",
        "NOT",
        "OR",
        "COND",
        "RIGHT",
        "AFIELD",
        "VREG"
    };
    static char *suffixes[] = {
        "0", "F", "D", "C", "S", "I", "U", "P", "V", "B",
        "10","11","12","13","14","15"
    };

    if (generic(op) >= AND && generic(op) <= VREG && opsize(op) == 0)
        return opnames[opindex(op)];

    return stringf("%s%s%s",
        (opindex(op) > 0 && opindex(op) < NELEMS(opnames)) ?
            opnames[opindex(op)] : stringd(opindex(op)),
        suffixes[optype(op)],
        (opsize(op)) > 0 ? stringd(opsize(op)) : "");
}

/****************************************************************************
 *                                                                          *
 * Function: nodeid                                                         *
 *                                                                          *
 * Purpose : Return id for given tree node.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#if defined(PODEBUG) || defined(PRERELEASE)
int nodeid(TREE *e)
{
    int i;

    ids[nid].node = e;

    for (i = 1; ids[i].node != e; i++)
        ;

    if (i == nid)
        ids[nid++].printed = FALSE;

    return i;
}
#endif

/****************************************************************************
 *                                                                          *
 * Function: printed                                                        *
 *                                                                          *
 * Purpose : Return pointer to ids[id].printed.                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#if defined(PODEBUG) || defined(PRERELEASE)
bool_t *printed(int id)
{
    if (id)
        return &ids[id].printed;

    nid = 1;
    return NULL;
}
#endif

/****************************************************************************
 *                                                                          *
 * Function: printtree                                                      *
 *                                                                          *
 * Purpose : Print tree p.                                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#if defined(PODEBUG) || defined(PRERELEASE)
void printtree(TREE *e)
{
    (void)printed(0);
    printtree1(e, 1);
}
#endif

/****************************************************************************
 *                                                                          *
 * Function: printtree1                                                     *
 *                                                                          *
 * Purpose : Recursively print tree p.                                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#if defined(PODEBUG) || defined(PRERELEASE)
static void printtree1(TREE *e, int lev)
{
    static char blanks[] = /* 51 spaces */ "                                                   ";
    int i;

    if (e == NULL || *printed(i = nodeid(e)))
        return;

    fprint(stdout, NULL, "#%d%S%S", i, blanks, (i < 10) ? 2 : (i < 100) ? 1 : 0, blanks, lev);
    fprint(stdout, NULL, "opname=%s %t", opname(e->op), e->type);
    *printed(i) = TRUE;

    for (i = 0; i < NELEMS(e->kids); i++)
        if (e->kids[i]) fprint(stdout, NULL, " #%d", nodeid(e->kids[i]));

    if (e->op == AFIELD && e->u.field)
    {
        fprint(stdout, NULL, " %s %d..%d", e->u.field->name, fieldsize(e->u.field) + fieldright(e->u.field), fieldright(e->u.field));
    }
    else if (generic(e->op) == CNST)
    {
        fprint(stdout, NULL, " v2s=%s", value_to_str(e->type, e->u.v));
    }
    else if (e->u.sym)
    {
        fprint(stdout, NULL, " sym=%s", e->u.sym->name);
    }

    if (e->node)
        fprint(stdout, NULL, " node=%p", e->node);

    fprint(stdout, NULL, "\n");

    for (i = 0; i < NELEMS(e->kids); i++)
        printtree1(e->kids[i], lev+1);
}
#endif

