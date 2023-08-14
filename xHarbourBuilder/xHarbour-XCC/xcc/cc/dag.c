/****************************************************************************
 *                                                                          *
 * File    : dag.c                                                          *
 *                                                                          *
 * Purpose : ISO C Compiler; Back-end intermediate code management.         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "lcc.h"
#include "float.h"

/* op must be from a DAG NODE (has proper size) */
#define iscall(op)  (generic(op) == CALL || generic(op) == INTRIN1S || generic(op) == INTRIN2S || (*IR->opcall)(op))

/* number of fastcall arguments */
#define NFASTARGS  2

typedef struct _DAG {
    NODE node;
    struct _DAG *hlink;
} DAG;

int nodecount;

static DAG *buckets[16];

static NODE *forest;

static int wants_prune_temps = -1;

static TREE *firstarg;
static short fastargno;

static NODE **tail;

static int depth = 0;

/* Static function prototypes */
static void list(NODE *);
static void unlist(void);
static DAG *new_dagnode(int, NODE *, NODE *, SYMBOL *);
static NODE *install_dagnode(int, NODE *, NODE *, SYMBOL *);
static void reset_dagnodes(void);
static void kill_dagnodes(SYMBOL *);
static NODE *undag(NODE *);
static NODE *visit(NODE *, bool_t, NODE *);
static NODE *temp_node(NODE *);
static NODE *temp_assignment_node(SYMBOL *, NODE *);
static void walk_code(CODE *, SYMBOL *);
static void label_node(int);
static void fixup_labels(NODE *);
static SYMBOL *equated_label(SYMBOL *);
static NODE *prune_temps(NODE *);
static NODE *replace_temps(NODE *);
static void typedbg(SYMBOL *, void *);
static void printdag1(NODE *, int);
static void printnode(NODE *, int);

/****************************************************************************
 *                                                                          *
 * Function: new_node                                                       *
 *                                                                          *
 * Purpose : Return a new dag node.                                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

NODE *new_node(int op, NODE *l, NODE *r, SYMBOL *sym)
{
    return &new_dagnode(op, l, r, sym)->node;
}

/****************************************************************************
 *                                                                          *
 * Function: new_forest                                                     *
 *                                                                          *
 * Purpose : Build dag from the given tree and append to the code list.     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void new_forest(TREE *tp, int tlab, int flab)
{
    list_nodes(tp, tlab, flab);

    if (forest)
    {
        NODE *list = forest->link;
        forest->link = NULL;

        if (!IR->wants_dag && nerrs == 0)
            list = undag(list);

        new_code(CODE_GEN)->u.forest = list;
        forest = NULL;
    }

    reset_dagnodes();
    memfree(STMT);
}

/****************************************************************************
 *                                                                          *
 * Function: list_nodes                                                     *
 *                                                                          *
 * Purpose : Build dag from the given tree.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           03-09-01  Added alloca usage flag.                             *
 *           03-09-19  Changed mulops_calls into generalized opcall.        *
 *           03-09-27  Bugfix: bitfields less than int were stored as int!  *
 *           04-03-03  Bugfix: must use DAG op in IR->opcall, not TREE op.  *
 *           04-03-08  Added optimization for e ? 1 : 0.                    *
 *           04-07-02  Added annotation of fastcall arguments.              *
 *           04-07-09  Added hack to avoid generating CxxI8, CxxU8.         *
 *           04-07-15  Added operators INTRIN1{S}, INTRIN2{S}.              *
 *           04-12-27  Added better support for e ? c1 : c2.                *
 *           05-01-02  Added support for small structure optimization.      *
 *                                                                          *
 ****************************************************************************/

NODE *list_nodes(TREE *tp, int tlab, int flab)
{
    NODE *p = NULL, *l, *r;
    int op;

    assert(tlab == 0 || flab == 0);

    if (tp == NULL)
        return NULL;

    if (tp->node)
        return tp->node;

    /* optimize tree's with small structures (05-01-02) */
    tp = optimized_struct_tree(tp);

    if (isarray(tp->type))
        op = tp->op + sizeop(voidptype->size);
    else
        op = tp->op + sizeop(tp->type->size);

    switch (generic(tp->op))
    {
        case AND:
            if (depth++ == 0)
                reset_dagnodes();

            if (flab)
            {
                list_nodes(tp->kids[0], 0, flab);
                list_nodes(tp->kids[1], 0, flab);
            }
            else
            {
                list_nodes(tp->kids[0], 0, flab = make_label(1));
                list_nodes(tp->kids[1], tlab, 0);
                label_node(flab);
            }
            depth--;
            break;

        case OR:
            if (depth++ == 0)
                reset_dagnodes();

            if (tlab)
            {
                list_nodes(tp->kids[0], tlab, 0);
                list_nodes(tp->kids[1], tlab, 0);
            }
            else
            {
                tlab = make_label(1);
                list_nodes(tp->kids[0], tlab, 0);
                list_nodes(tp->kids[1], 0, flab);
                label_node(tlab);
            }
            depth--;
            break;

        case NOT:
            return list_nodes(tp->kids[0], flab, tlab);  /* swap labels */

        case COND:  /* e ? e1 : e2 */
        {
            TREE *q = tp->kids[1], *t = tp->kids[0];
            assert(tlab == 0 && flab == 0);

            /* optimize (e ? c1 : c2) to expression without jumps */
            switch (generic(t->op))
            {
                case EQ: op = CEQ; break;
                case NE: op = CNE; break;
                case GT: op = CGT; break;
                case LT: op = CLT; break;
                case GE: op = CGE; break;
                case LE: op = CLE; break;
                default: op = 0; break;
            }
            /* for example CEQI4 means we compare integers, but also return an integer! (LCC requirement) */
            if (options.pragmaopt && op != 0 &&
                q && q->op == RIGHT &&
                q->kids[0] &&
                generic(q->kids[0]->op) == ASGN &&
                generic(q->kids[0]->kids[1]->op) == CNST &&
                q->kids[1] &&
                generic(q->kids[1]->op) == ASGN &&
                generic(q->kids[1]->kids[1]->op) == CNST &&
                isint(t->kids[0]->type) && t->kids[0]->type->size <= inttype->size &&
                isint(q->kids[0]->type) && q->kids[0]->type->size == t->kids[0]->type->size)
            {
                intmax_t c1 = q->kids[0]->kids[1]->u.v.i;
                intmax_t c2 = q->kids[1]->kids[1]->u.v.i;
                VALUE v;

                if (c1 < c2)
                {
                    intmax_t t = c1; c1 = c2; c2 = t;
                    switch (op)
                    {
                        case CEQ: op = CNE; break;
                        case CNE: op = CEQ; break;
                        case CGT: op = CLE; break;
                        case CLT: op = CGE; break;
                        case CGE: op = CLT; break;
                        case CLE: op = CGT; break;
                        default: assert(0);
                    }
                }
                if ((c1 - c2) != 1)
                {
                    switch (op)
                    {
                        case CEQ: op = CNE; break;
                        case CNE: op = CEQ; break;
                        case CGT: op = CLE; break;
                        case CLT: op = CGE; break;
                        case CGE: op = CLT; break;
                        case CLE: op = CGT; break;
                        default: assert(0);
                    }
                }

                l = list_nodes(t->kids[0], 0, 0);
                r = list_nodes(t->kids[1], 0, 0);
                p = new_node(op + opkind(l->op), l, r, NULL);

                if ((*IR->opcall)(p->op))  /* important */
                    funcsym->u.fcn.ncalls++;

                if ((c1 - c2) != 1)
                {
                    v.i = 1;
                    r = new_node(CNST + opkind(p->op), NULL, NULL, constant(inttype, v));
                    p = new_node(SUB + opkind(p->op), p, r, NULL);

                    v.i = c1 - c2;
                    r = new_node(CNST + opkind(p->op), NULL, NULL, constant(inttype, v));
                    p = new_node(BAND + opkind(p->op), p, r, NULL);
                }

                if (c2 != 0)
                {
                    v.i = c2;
                    r = new_node(CNST + opkind(p->op), NULL, NULL, constant(inttype, v));
                    p = new_node(ADD + opkind(p->op), p, r, NULL);
                }

#ifdef PODEBUG
                /* shut up assert's - the generated code should be fine */
                if (opkind(p->op) != opkind(q->kids[0]->op))
                    p = install_dagnode(CVI + ttob(q->kids[0]->type), p, NULL, intconst(q->kids[0]->type->size));
#endif
                break;
            }

            /* normal processing of COND */
            if (tp->u.sym)
                new_local_var(tp->u.sym);

            flab = make_label(2);
            list_nodes(tp->kids[0], 0, flab);
            assert(q && q->op == RIGHT);
            reset_dagnodes();
            list_nodes(q->kids[0], 0, 0);

            if (forest->op == LABEL+V)
            {
                equate_labels(forest->syms[0], find_label(flab+1));
                unlist();
            }

            list(jump(flab+1));
            label_node(flab);
            list_nodes(q->kids[1], 0, 0);

            if (forest->op == LABEL+V)
            {
                equate_labels(forest->syms[0], find_label(flab+1));
                unlist();
            }

            label_node(flab+1);

            if (tp->u.sym)
                p = list_nodes(id_tree(tp->u.sym), 0, 0);
            break;
        }

        case CNST:
        {
            TYPE *ty = unqual(tp->type);
            assert(ty->u.sym);

            if (tlab || flab)
            {
                assert(ty == inttype);
                if (tlab && tp->u.v.i != 0)
                    list(jump(tlab));
                else if (flab && tp->u.v.i == 0)
                    list(jump(flab));
            }
            else if (ty->u.sym->addressed)
                p = list_nodes(static_constant(tp), 0, 0);
            else
                p = install_dagnode(op, NULL, NULL, constant(ty, tp->u.v));
            break;
        }

        case RIGHT:
            if (tp->kids[0] && tp->kids[1] &&
                generic(tp->kids[1]->op) == ASGN &&
                (generic(tp->kids[0]->op) == INDIR &&
                tp->kids[0]->kids[0] == tp->kids[1]->kids[0] ||
                (tp->kids[0]->op == AFIELD &&
                tp->kids[0] == tp->kids[1]->kids[0])))
            {
                assert(tlab == 0 && flab == 0);
                if (generic(tp->kids[0]->op) == INDIR)
                {
                    p = list_nodes(tp->kids[0], 0, 0);
                    list(p);
                    list_nodes(tp->kids[1], 0, 0);
                }
                else
                {
                    assert(generic(tp->kids[0]->kids[0]->op) == INDIR);
                    list(list_nodes(tp->kids[0]->kids[0], 0, 0));
                    p = list_nodes(tp->kids[0], 0, 0);
                    list_nodes(tp->kids[1], 0, 0);
                }
            }
            else if (tp->kids[1])
            {
                list_nodes(tp->kids[0], 0, 0);
                p = list_nodes(tp->kids[1], tlab, flab);
            }
            else
            {
                p = list_nodes(tp->kids[0], tlab, flab);
            }
            break;

        case JUMP:
            assert(tlab == 0 && flab == 0);
            assert(tp->u.sym == 0);
            assert(tp->kids[0]);
            l = list_nodes(tp->kids[0], 0, 0);
            list(new_node(JUMP+V, l, NULL, NULL));
            reset_dagnodes();
            break;

        case CALL:
        {
            TREE *save1 = firstarg;
            short save2 = fastargno;

            firstarg = NULL;
            assert(tlab == 0 && flab == 0);

            assert(isptr(tp->kids[0]->type));
            assert(isfunc(tp->kids[0]->type->type));

            /* annotate fastcall arguments? */
            fastargno = (isfast(tp->kids[0]->type->type)) ? 0 : NFASTARGS;

            if (tp->op == CALL+P &&
                tp->kids[0]->kids[1] &&
                tp->kids[0]->kids[1]->u.sym &&
                strcmp(tp->kids[0]->kids[1]->u.sym->name, "_alloca") == 0)
                funcsym->hasalloca = TRUE;  /* _alloca is special */

            if (tp->op == CALL+B && !IR->wants_callb)
            {
                /* structure call with hidden argument */
                TREE *arg0 = new_tree(ARG+P, tp->kids[1]->type, tp->kids[1], NULL);

                if (IR->left_to_right)
                    firstarg = arg0;

                l = list_nodes(tp->kids[0], 0, 0);
                if (!IR->left_to_right || firstarg)
                {
                    firstarg = NULL;
                    list_nodes(arg0, 0, 0);
                }
                p = new_node(CALL+V, l, NULL, NULL);
            }
            else
            {
                l = list_nodes(tp->kids[0], 0, 0);
                r = list_nodes(tp->kids[1], 0, 0);
                p = new_node(tp->op == CALL+B ? tp->op : op, l, r, NULL);
            }

            p->syms[0] = memalloc(sizeof(*p->syms[0]), funca);
            memset(p->syms[0], 0, sizeof(*p->syms[0]));
            p->syms[0]->type = tp->kids[0]->type->type;
            list(p);
            reset_dagnodes();
            funcsym->u.fcn.ncalls++;

            firstarg = save1;
            fastargno = save2;
            break;
        }

        case ARG:
        {
            short *argnop;

            assert(tlab == 0 && flab == 0);

            if (IR->left_to_right)
            {
                list_nodes(tp->kids[1], 0, 0);
            }

            if (firstarg)
            {
                TREE *arg = firstarg;
                firstarg = NULL;
                list_nodes(arg, 0, 0);
            }

            l = list_nodes(tp->kids[0], 0, 0);
            list(new_node(tp->op == ARG+B ? tp->op : op, l, NULL, NULL));
            forest->syms[0] = intconst(tp->type->size);
            forest->syms[1] = intconst(tp->type->align);

            /* argno above zero are fastcall arguments */
            argnop = &forest->x.argno; *argnop = 0;

            if (!IR->left_to_right)
            {
                list_nodes(tp->kids[1], 0, 0);

                /* annotate fastcall arguments after recursion */
                if (++fastargno <= NFASTARGS)
                    *argnop = fastargno;
            }
            break;
        }

        case EQ:
        case NE:
        case GT:
        case GE:
        case LE:
        case LT:
        {
            assert(tp->u.sym == 0);
            assert(nerrs || tlab || flab);

            l = list_nodes(tp->kids[0], 0, 0);
            r = list_nodes(tp->kids[1], 0, 0);

            assert(nerrs || opkind(l->op) == opkind(r->op));
            assert(nerrs || optype(op) == optype(l->op));

            if (tlab)
            {
                assert(flab == 0);
                op = generic(tp->op) + opkind(l->op);
                list(new_node(op, l, r, find_label(tlab)));
            }
            else if (flab)
            {
                switch (generic(tp->op))
                {
                    case EQ: op = NE; break;
                    case NE: op = EQ; break;
                    case GT: op = LE; break;
                    case LT: op = GE; break;
                    case GE: op = LT; break;
                    case LE: op = GT; break;
                    default: assert(0);
                }

                op = op + opkind(l->op);
                list(new_node(op, l, r, find_label(flab)));
            }

            if ((*IR->opcall)(op))  /* added 03-09-19; fixed 04-03-03 (op <- tp->op) */
                funcsym->u.fcn.ncalls++;

            if (forest && forest->syms[0])
                forest->syms[0]->ref++;
            break;
        }

        case ASGN:
        {
            assert(tlab == 0 && flab == 0);

            if (tp->kids[0]->op == AFIELD)
            {
                TREE *x = tp->kids[0]->kids[0];
                FIELD *field = tp->kids[0]->u.field;

                assert(generic(x->op) == INDIR);

                reset_dagnodes();
                l = list_nodes(lvalue(x), 0, 0);
                if (fieldsize(field) < 8*field->type->size)
                {
                    uint_t fmask = fieldmask(field);
                    uint_t  mask = fmask << fieldright(field);
                    TREE *q = tp->kids[1];

                    if (q->op == CNST+I && q->u.v.i == 0 ||
                        q->op == CNST+U && q->u.v.u == 0)
                    {
                        q = bit_tree(BAND, x, cnst_tree(unsignedtype, (uintmax_t)~mask));
                    }
                    else if (q->op == CNST+I && (q->u.v.i & fmask) == fmask ||
                             q->op == CNST+U && (q->u.v.u & fmask) == fmask)
                    {
                        q = bit_tree(BOR, x, cnst_tree(unsignedtype, (uintmax_t)mask));
                    }
                    else
                    {
                        list_nodes(q, 0, 0);

                        q = bit_tree(BOR,
                            bit_tree(BAND, rvalue(lvalue(x)),
                                cnst_tree(unsignedtype, (uintmax_t)~mask)),
                            bit_tree(BAND, shift_tree(LSH, cast(q, unsignedtype),
                                cnst_tree(unsignedtype, (uintmax_t)fieldright(field))),
                                cnst_tree(unsignedtype, (uintmax_t)mask)));
                    }

                    /* if (x->type->size < q->type->size) */
                    if (q->type->size > x->type->size)
                        q = simplify(CVU, x->type, q, NULL);  /* added 03-09-27 */

                    r = list_nodes(q, 0, 0);
                    op = ASGN + ttob(q->type);
                }
                else
                {
                    r = list_nodes(tp->kids[1], 0, 0);
                    op = ASGN + ttob(tp->kids[1]->type);
                }
            }
            else
            {
                l = list_nodes(tp->kids[0], 0, 0);
                r = list_nodes(tp->kids[1], 0, 0);
            }

            list(new_node(tp->op == ASGN+B ? tp->op : op, l, r, NULL));
            forest->syms[0] = intconst(tp->kids[1]->type->size);
            forest->syms[1] = intconst(tp->kids[1]->type->align);

            if (isaddrop(tp->kids[0]->op) && !tp->kids[0]->u.sym->computed)
                kill_dagnodes(tp->kids[0]->u.sym);
            else
                reset_dagnodes();

            p = list_nodes(tp->kids[1], 0, 0);
            break;
        }

        case BOR:
        case BAND:
        case BXOR:
        case ADD:
        case SUB:
        case MUL:
        case DIV:
        case MOD:
        case RSH:
        case LSH:
            assert(tlab == 0 && flab == 0);
            l = list_nodes(tp->kids[0], 0, 0);
            r = list_nodes(tp->kids[1], 0, 0);
            p = install_dagnode(op, l, r, NULL);
            if ((*IR->opcall)(op))  /* added 03-09-19; fixed 04-03-03 (op <- tp->op) */
            {
                list(p);
                funcsym->u.fcn.ncalls++;
            }
            break;

        case RET:
            assert(tlab == 0 && flab == 0);
            l = list_nodes(tp->kids[0], 0, 0);
            list(new_node(op, l, NULL, NULL));
            break;

        case CVF:
        case CVI:
        case CVP:
        case CVU:
            assert(tlab == 0 && flab == 0);
            assert(optype(tp->kids[0]->op) != optype(tp->op) || tp->kids[0]->type->size != tp->type->size);
            l = list_nodes(tp->kids[0], 0, 0);
            p = install_dagnode(op, l, NULL, intconst(tp->kids[0]->type->size));
            if ((*IR->opcall)(op))  /* added 03-09-19; fixed 04-03-03 (op <- tp->op) */
            {
                list(p);
                funcsym->u.fcn.ncalls++;
            }
            break;

        case CBOOL:
            assert(tlab == 0 && flab == 0);
            l = list_nodes(tp->kids[0], 0, 0);
            p = install_dagnode(op, l, NULL, NULL);
            break;

        case BCOM:
        case NEG:
            assert(tlab == 0 && flab == 0);
            l = list_nodes(tp->kids[0], 0, 0);
            p = install_dagnode(op, l, NULL, NULL);
            if ((*IR->opcall)(op))  /* added 03-09-19; fixed 04-03-03 (op <- tp->op) */
            {
                list(p);
                funcsym->u.fcn.ncalls++;
            }
            break;

        case INDIR:
        {
            TYPE *ty = tp->kids[0]->type;
            assert(tlab == 0 && flab == 0);

            l = list_nodes(tp->kids[0], 0, 0);

            /* this only works for scalar variables */
            if (specific(l->op) == ADDRL+P)
                l->syms[0]->fetched = TRUE;

            if (isptr(ty))
                ty = unqual(ty)->type;

            if (isvolatile(ty) || (isstruct(ty) && unqual(ty)->u.sym->u.s.vfields))
                p = new_node(tp->op == INDIR+B ? tp->op : op, l, NULL, NULL);  /* don't hash */
            else
                p = install_dagnode(tp->op == INDIR+B ? tp->op : op, l, NULL, NULL);
            break;
        }

        case AFIELD:
        {
            TREE *q = tp->kids[0];

            if (tp->type == inttype)
            {
                intmax_t n = fieldleft(tp->u.field);
                q = shift_tree(RSH,
                    shift_tree(LSH, q, cnst_tree(inttype, n)),
                    cnst_tree(inttype, n + fieldright(tp->u.field)));
            }
            else if (fieldsize(tp->u.field) < 8*tp->u.field->type->size)
                q = bit_tree(BAND,
                    shift_tree(RSH, q, cnst_tree(inttype, (intmax_t)fieldright(tp->u.field))),
                    cnst_tree(unsignedtype, (uintmax_t)fieldmask(tp->u.field)));

            assert(tlab == 0 && flab == 0);
            p = list_nodes(q, 0, 0);
            break;
        }

        case ADDRG:
        case ADDRF:
            assert(tlab == 0 && flab == 0);
            p = install_dagnode(tp->op + sizeop(voidptype->size), NULL, NULL, tp->u.sym);
            break;

        case ADDRL:
            assert(tlab == 0 && flab == 0);
            if (tp->u.sym->generated)
                new_local_var(tp->u.sym);
            p = install_dagnode(tp->op + sizeop(voidptype->size), NULL, NULL, tp->u.sym);
            break;

        case INTRIN1:
        case INTRIN2:
            /* intrinsic without side effects */
            assert(tlab == 0 && flab == 0);
            /* assert(tp->kids[0]); -- we use INTRIN1 for no args too! */
            assert(generic(tp->op) == INTRIN2 ? tp->kids[1] != 0 : tp->kids[1] == 0);
            l = list_nodes(tp->kids[0], 0, 0);
            assert((tp->kids[0] != NULL) == (l != NULL));
            r = list_nodes(tp->kids[1], 0, 0);
            assert((tp->kids[1] != NULL) == (r != NULL));
            p = install_dagnode(op, l, r, tp->u.sym);  /* pass on function ID */
            break;

        case INTRIN1S:
        case INTRIN2S:
            /* intrinsic *with* side effects */
            assert(tlab == 0 && flab == 0);
            /* assert(tp->kids[0]); -- we use INTRIN1S for no args too! */
            assert(generic(tp->op) == INTRIN2S ? tp->kids[1] != 0 : tp->kids[1] == 0);
            l = list_nodes(tp->kids[0], 0, 0);
            assert((tp->kids[0] != NULL) == (l != NULL));
            r = list_nodes(tp->kids[1], 0, 0);
            assert((tp->kids[1] != NULL) == (r != NULL));
            p = new_node(op, l, r, tp->u.sym);  /* pass on function ID */
            list(p);
            break;

        default:
            assert(0);
    }

    tp->node = p;
    return p;
}

/****************************************************************************
 *                                                                          *
 * Function: list                                                           *
 *                                                                          *
 * Purpose : List a node, i.e. append it to the forest as a root.           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void list(NODE *p)
{
    if (p && p->link == NULL)
    {
        if (forest)
        {
            p->link = forest->link;
            forest->link = p;
        }
        else
        {
            p->link = p;
        }

        forest = p;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: unlist                                                         *
 *                                                                          *
 * Purpose : Remove the last node from the forest.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void unlist(void)
{
    NODE *p;

    assert(forest);
    assert(forest != forest->link);

    for (p = forest->link; p->link != forest; p = p->link)
        ;

    p->link = forest->link;
    forest = p;
}

/****************************************************************************
 *                                                                          *
 * Function: new_dagnode                                                    *
 *                                                                          *
 * Purpose : Allocate a new dag node.                                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static DAG *new_dagnode(int op, NODE *l, NODE *r, SYMBOL *sym)
{
    DAG *p;

    p = memalloc(sizeof(*p), funca);
    memset(p, 0, sizeof(*p));
    p->node.op = op;

    if ((p->node.kids[0] = l) != NULL)
        ++l->count;

    if ((p->node.kids[1] = r) != NULL)
        ++r->count;

    p->node.syms[0] = sym;

    return p;
}

/****************************************************************************
 *                                                                          *
 * Function: install_dagnode                                                *
 *                                                                          *
 * Purpose : Install a dag node in the hash table. The hash table is used   *
 *           to eliminate 'common sub-expressions' from the trees.          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static NODE *install_dagnode(int op, NODE *l, NODE *r, SYMBOL *sym)
{
    DAG *p;
    int i;

    i = (opindex(op) ^ ((ulong_t)sym >> 2)) & (NELEMS(buckets)-1);
    for (p = buckets[i]; p != NULL; p = p->hlink)
    {
        if (p->node.op == op && p->node.syms[0] == sym &&
            p->node.kids[0] == l && p->node.kids[1] == r)
            return &p->node;
    }

    p = new_dagnode(op, l, r, sym);
    p->hlink = buckets[i];
    buckets[i] = p;
    ++nodecount;

    return &p->node;
}

/****************************************************************************
 *                                                                          *
 * Function: reset_dagnodes                                                 *
 *                                                                          *
 * Purpose : Clear hash table with dag nodes.                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void reset_dagnodes(void)
{
    if (nodecount > 0)
        memset(buckets, 0, sizeof(buckets));
    nodecount = 0;
}

/****************************************************************************
 *                                                                          *
 * Function: kill_dagnodes                                                  *
 *                                                                          *
 * Purpose : Remove all nodes for an assignments rvalue, that has been      *
 *           invalidated through side effects.                              *
 *                                                                          *
 *           c = a + b;                                                     *
 *           a = a/2;                                                       *
 *           d = a + b;  <-- not the same a + b                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-03-23  Updating of sym->assigned added.                     *
 *                                                                          *
 ****************************************************************************/

static void kill_dagnodes(SYMBOL *sym)
{
    int i;

    sym->assigned = TRUE;

    for (i = 0; i < NELEMS(buckets); i++)
    {
        DAG **p;

        for (p = &buckets[i]; *p; )
        {
            if (generic((*p)->node.op) == INDIR &&
                (!isaddrop((*p)->node.kids[0]->op) ||
                (*p)->node.kids[0]->syms[0] == sym))
            {
                *p = (*p)->hlink;
                --nodecount;
            }
            else
            {
                p = &(*p)->hlink;
            }
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: undag                                                          *
 *                                                                          *
 * Purpose : Turn dags back into proper trees.                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static NODE *undag(NODE *forest)
{
    NODE *p;

    tail = &forest;

    for (p = forest; p != NULL; p = p->link)
    {
        if (generic(p->op) == INDIR)
        {
            assert(p->count >= 1);
            visit(p, TRUE, NULL);

            if (p->syms[2])
            {
                assert(p->syms[2]->u.t.cse);
                p->syms[2]->u.t.cse = NULL;
                new_local_var(p->syms[2]);
            }
        }
        else if (iscall(p->op) && p->count >= 1)
        {
            visit(p, TRUE, NULL);
        }
        else
        {
            assert(p->count == 0),
            visit(p, TRUE, NULL);
            *tail = p; tail = &p->link;
        }
    }

    *tail = NULL;
    return forest;
}

/****************************************************************************
 *                                                                          *
 * Function: visit                                                          *
 *                                                                          *
 * Purpose : Traverse the dag rooted at p and return either p or a node     *
 *           for the temporary that holds the value represented by p.       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-03-21  Added recompute optimization: address + offset.      *
 *           04-07-20  Added recompute optimization: CNST+I, CNST+U.        *
 *                                                                          *
 ****************************************************************************/

static NODE *visit(NODE *p, bool_t listed, NODE *q)
{
    if (p)
    {
        if (p->syms[2])
        {
            /* reference common subexpression */
            p = temp_node(p);
        }
        else if (p->count <= 1 && !iscall(p->op) || p->count == 0 && iscall(p->op))
        {
            /* node referenced only once or call made for side effects only */
            p->kids[0] = visit(p->kids[0], FALSE, p);
            p->kids[1] = visit(p->kids[1], FALSE, p);
        }
        else if (specific(p->op) == ADDRL+P || specific(p->op) == ADDRF+P)
        {
            /* cheaper to recompute addresses of locals and parameters */
            assert(!listed);
            p = new_node(p->op, NULL, NULL, p->syms[0]);
            p->count = 1;
        }
        else if (specific(p->op) == CNST+I || specific(p->op) == CNST+U)
        {
            /* cheaper to recompute constants */
            assert(!listed);
            p = new_node(p->op, NULL, NULL, p->syms[0]);
            p->count = 1;
        }
        else if (options.pragmaopt && specific(p->op) == ADD+P && specific(p->kids[0]->op) == INDIR+P &&
            isaddrop(p->kids[0]->kids[0]->op) && generic(p->kids[1]->op) == CNST &&
            (!q || specific(q->op) != ASGN+P))
        {
            /* cheaper to recompute address + offset */
            assert(!listed);
            p = new_node(p->op, p->kids[0], p->kids[1], NULL);
            p->count = 1;
            p->kids[0] = visit(p->kids[0], FALSE, p);
            p->kids[1] = visit(p->kids[1], FALSE, p);
        }
        else if (p->op == INDIR+B)
        {
            /* registers can't hold structures (yes they can - but ignored) */
            p = new_node(p->op, p->kids[0], NULL, NULL);
            p->count = 1;
            p->kids[0] = visit(p->kids[0], FALSE, p);
            p->kids[1] = visit(p->kids[1], FALSE, p);
        }
        else
        {
            /* first encounter with a common subexpression */
            p->kids[0] = visit(p->kids[0], FALSE, p);
            p->kids[1] = visit(p->kids[1], FALSE, p);
            p->syms[2] = make_temp_ident(REGISTER, btot(p->op, opsize(p->op)));
            assert(!p->syms[2]->defined);
            p->syms[2]->ref = 1;
            p->syms[2]->u.t.cse = p;

            *tail = temp_assignment_node(p->syms[2], p);
            tail = &(*tail)->link;

            if (!listed) p = temp_node(p);
        }
    }

    return p;
}

/****************************************************************************
 *                                                                          *
 * Function: temp_node                                                      *
 *                                                                          *
 * Purpose : Build and return the dag (INDIR(ADDRLP p->syms[2])).           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static NODE *temp_node(NODE *p)
{
    SYMBOL *sym = p->syms[2];
    assert(sym);

    if (--p->count == 0)
        p->syms[2] = NULL;

    p = new_node(INDIR + ttob(sym->type),
        new_node(ADDRL + ttob(voidptype), NULL, NULL, sym), NULL, NULL);

    p->count = 1;
    return p;
}

/****************************************************************************
 *                                                                          *
 * Function: temp_assignment_node                                           *
 *                                                                          *
 * Purpose : Generate an assignment to a temporary.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static NODE *temp_assignment_node(SYMBOL *sym, NODE *p)
{
    p = new_node(ASGN + ttob(sym->type),
        new_node(ADDRL + ttob(voidptype), NULL, NULL, sym), p, NULL);

    p->syms[0] = intconst(sym->type->size);
    p->syms[1] = intconst(sym->type->align);

    return p;
}

/****************************************************************************
 *                                                                          *
 * Function: static_constant                                                *
 *                                                                          *
 * Purpose : Convert constant to out-of-line location.                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

TREE *static_constant(TREE *p)
{
    SYMBOL *sym = constant(p->type, p->u.v);
    TREE *e;

    if (sym->u.c.loc == NULL)
        sym->u.c.loc = make_ident(STATIC, p->type, GLOBAL);

    if (isarray(p->type))
    {
        e = simplify(ADDRG, array_to_ptr(p->type), NULL, NULL);
        e->u.sym = sym->u.c.loc;
        sym->ref++;
    }
    else
    {
        e = id_tree(sym->u.c.loc);
        sym->ref++;
    }

    return e;
}

/****************************************************************************
 *                                                                          *
 * Function: best_function_regsyms                                          *
 *                                                                          *
 * Purpose : Make sure the highest reference counts gets the registers.     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           00-11-21  Moved to dag.c from x86.md                           *
 *                                                                          *
 ****************************************************************************/

void best_function_regsyms(SYMBOL *callee[], int limit)
{
    SYMBOL *symtab[32], *p;
    CODE *cp;
    int i, n = 0;

    /* process arguments */
    for (i = 0; (p = callee[i]) != NULL; i++)
    {
        if (p->sclass == REGISTER && !p->addressed && isscalar(p->type))
        {
            assert(n < NELEMS(symtab));
            symtab[n++] = p;
            if (n > limit)
            {
                int i, drop;

                /* find the lowest reference count */
                for (drop = 0, i = 1; i < n; i++)
                    if (symtab[i]->ref < symtab[drop]->ref)
                        drop = i;

                /* don't allocate a register to this symbol */
                symtab[drop]->sclass = AUTO;
                symtab[drop] = symtab[--n];
            }
        }
    }

    /* process block locals (not temporaries) */
    for (cp = codehead.next; cp != NULL; cp = cp->next)
    {
        switch (cp->kind)
        {
            case CODE_BLOCKBEG:
            {
                SYMBOL **sym;

                for (sym = cp->u.block.locals; *sym != NULL; sym++)
                {
                    if ((*sym)->sclass == REGISTER && !(*sym)->addressed && isscalar((*sym)->type))
                    {
                        assert(n < NELEMS(symtab));
                        symtab[n++] = (*sym);
                        if (n > limit)
                        {
                            int i, drop;

                            /* find the lowest reference count */
                            for (drop = 0, i = 1; i < n; i++)
                                if (symtab[i]->ref < symtab[drop]->ref)
                                    drop = i;

                            /* don't allocate a register to this symbol */
                            symtab[drop]->sclass = AUTO;
                            symtab[drop] = symtab[--n];
                        }
                    }
                }
                break;
            }

            case CODE_BLOCKEND:
            {
                SYMBOL **sym;

                for (sym = cp->u.begin->u.block.locals; *sym != NULL; sym++)
                {
                    /* remove symbols that is going out of scope */
                    for (i = 0; i < n; i++)
                    {
                        if (symtab[i] == (*sym))
                        {
                            symtab[i] = symtab[--n];
                            break;
                        }
                    }
                }
                break;
            }

            default:
                break;
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: walk_code                                                      *
 *                                                                          *
 * Purpose : Walk all code paths to find dead code.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-03-13  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void walk_code(CODE *cp, SYMBOL *sym)
{
    if (sym)
    {
        /* searching for a label - already done? */
        if (sym->fetched) return; else sym->fetched = TRUE;
    }

    for (; cp != NULL; cp = cp->next)
    {
        if (!sym)
            cp->count++;

        if (cp->kind == CODE_GEN || cp->kind == CODE_JUMP || cp->kind == CODE_LABEL)
        {
            NODE *p;

            for (p = cp->u.forest; p != NULL; p = p->link)
            {
                switch (generic(p->op))
                {
                    case JUMP:
                        if (!sym && specific(p->kids[0]->op) == ADDRG+P)
                        {
                            /* found a jump - locate the label and walk from there */
                            assert(p->kids[0]->syms[0]);
                            walk_code(codehead.next, equated_label(p->kids[0]->syms[0]));
                            /* this is an unconditional jump - we are done */
                            return;
                        }
                        break;

                    case LABEL:
                        if (sym && equated_label(p->syms[0]) == sym)
                        {
                            /* found a label - start walking */
                            cp->count++;
                            sym = NULL;
                        }
                        break;

                    case EQ:
                    case GE:
                    case GT:
                    case LE:
                    case LT:
                    case NE:
                        if (!sym)
                        {
                            /* found a conditional jump - locate the label and walk from there */
                            assert(p->syms[0]);
                            walk_code(codehead.next, equated_label(p->syms[0]));
                            /* this is a conditional jump - continue */
                        }
                        break;
                }
            }
        }
        else if (cp->kind == CODE_SWITCH && !sym)
        {
            int i;

            /* locate switch labels and walk from there */
            for (i = 0; i < cp->u.swtch.size; i++)
                walk_code(codehead.next, equated_label(cp->u.swtch.labels[i]));

            /* locate default switch label and walk from there */
            walk_code(codehead.next, equated_label(cp->u.swtch.deflab));
        }
        else if (cp->kind == CODE_ASM && !sym && cp->u.asm.sym)
        {
            /* don't cause unnecessary warnings */
            cp->u.asm.sym->assigned = cp->u.asm.sym->fetched = TRUE;
        }
    }

#ifdef PODEBUG
    if (sym) printf("Failed to locate label %s\n", sym->x.name);
#endif
}

/****************************************************************************
 *                                                                          *
 * Function: generate_function_code                                         *
 *                                                                          *
 * Purpose : Generate code for a functions body.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           03-08-14  Support for variable-length arrays added.            *
 *           04-03-13  Call to walk_code() and dead code elimination added. *
 *           04-03-22  Warning about unused/unassigned locals added.        *
 *           04-04-22  Bugfix: cp->next can be NULL.                        *
 *           04-07-19  Also check x.regnode for argument assignment.        *
 *                                                                          *
 ****************************************************************************/

void generate_function_code(SYMBOL *caller[], SYMBOL *callee[])
{
    COORDINATE save;
    CODE *cp;

    if (wants_prune_temps == -1)
        wants_prune_temps = !IR->wants_dag;

    save = src;

    /**/
    {
        SYMBOL *p, *q;
        int i;

        cp = codehead.next->next;
        codelist = codehead.next;

        /* assign arguments */
        for (i = 0; (p = callee[i]) != NULL && (q = caller[i]) != NULL; i++)
        {
            TYPE *ty;

            /* checking the backend's private extension isn't OK, but really helps */
            if (p->sclass != q->sclass || p->type != q->type || p->x.regnode != q->x.regnode)
                new_forest(assignment(p, id_tree(q)), 0, 0);

            /* remember the size of incoming variable-length arrays */
            for (ty = p->type; isptr(ty); ty = ty->type)
                ;
            if (isvla(ty))
                new_forest(vlasize_tree(ty, -1), 0, 0);
        }

        codelist->next = cp;
        cp->prev = codelist;
    }

    if (options.dbglevel > 1 && IR->dbgsym)
    {
        SYMBOL *p, *q;
        int i;

        /* emit debugging information */
        for (i = 0; (p = callee[i]) != NULL && (q = caller[i]) != NULL; i++)
        {
            (*IR->dbgsym)(p);

            if (p->sclass != q->sclass || p->type != q->type)
                (*IR->dbgsym)(q);
        }

        set_segment(TEXT);
    }

    if (options.pragmaopt)
    {
        int warn = 0;

        /* walk all paths to find unreachable code */
        walk_code(codehead.next, NULL);

        for (cp = codehead.next; nerrs <= 0 && cp != NULL; cp = cp->next)
        {
            if (cp->kind == CODE_DEFPOINT)
            {
                /* remember current location */
                src = cp->u.point.src;
            }
            else if (cp->kind == CODE_BLOCKBEG)
            {
                SYMBOL **sym;

                for (sym = cp->u.block.locals; *sym != NULL; sym++)
                {
                    /* warn about initialized but unused, or used but uninitialized, locals */
                    if ((*sym)->ref != 0 && !(*sym)->addressed && isscalar((*sym)->type))
                    {
                        src = (*sym)->src;

                        if (!(*sym)->assigned)
                            apperror(RCWARNING1(ERROR_UNASSIGNED_LOCAL), (*sym)->name);
                        else if (!(*sym)->fetched)
                            apperror(RCWARNING1(ERROR_UNUSED_LOCAL), (*sym)->name);
                    }
                }
            }
            else if (cp->count == 0 && (cp->kind == CODE_GEN || cp->kind == CODE_JUMP || cp->kind == CODE_LABEL))
            {
                CODE *cpt;

                /* make sure we're not in a SEH block */
                for (cpt = cp->prev; cpt != NULL && cpt->kind != CODE_BLOCKBEG; cpt = cpt->prev)
                    ;
                if (cpt == NULL || cpt->u.block.seh.data == NULL)
                {
                    /* remove unreachable code */
                    if (warn++ == 0)
                        apperror(RCWARNING2(ERROR_UNREACHABLE_CODE_REMOVED));

                    cp->prev->next = cp->next;
                    if (cp->next)  /* bugfix 04-04-22 */
                        cp->next->prev = cp->prev;
                    cp = cp->prev;
                    continue;
                }
            }
        }
    }

    for (cp = codehead.next; nerrs <= 0 && cp != NULL; cp = cp->next)
    {
        switch (cp->kind)
        {
            case CODE_ADDRESS:
                (*IR->address)(cp->u.addr.sym, cp->u.addr.base, cp->u.addr.offset);
                break;

            case CODE_BLOCKBEG:
            {
                SYMBOL **sym;

                (*IR->blockbeg)(&cp->u.block.x);

                for (sym = cp->u.block.locals; *sym != NULL; sym++)
                {
                    if ((*sym)->ref != 0 || options.dbglevel > 1)
                        (*IR->local)(*sym);
                }
                break;
            }

            case CODE_BLOCKEND:
                (*IR->blockend)(&cp->u.begin->u.block.x);
                break;

            case CODE_DEFPOINT:
                src = cp->u.point.src;
                break;

            case CODE_GEN:
            case CODE_JUMP:
            case CODE_LABEL:
                if (wants_prune_temps) cp->u.forest = prune_temps(cp->u.forest);
                fixup_labels(cp->u.forest);
                if (cp->u.forest)
                    cp->u.forest = (*IR->gen)(cp->u.forest);
                break;

            case CODE_LOCAL:
                (*IR->local)(cp->u.var);
                break;

            case CODE_SWITCH:
            case CODE_ASM:
            case CODE_UNWIND:
                break;

            default:
                assert(0);
        }
    }

    src = save;
}

/****************************************************************************
 *                                                                          *
 * Function: emit_function_code                                             *
 *                                                                          *
 * Purpose : Emit code from generate_function_code().                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           01-01-10  Source coordinates might be 0/NULL.                  *
 *           01-01-11  SEH unwind support added.                            *
 *                                                                          *
 ****************************************************************************/

void emit_function_code(void)
{
    COORDINATE save;
    CODE *cp;
    int skip = 0;

    save = src;

    for (cp = codehead.next; nerrs <= 0 && cp != NULL; cp = cp->next)
    {
        if (skip && cp->kind != CODE_BLOCKEND) continue;

        switch (cp->kind)
        {
            case CODE_ADDRESS:
                break;

            case CODE_BLOCKBEG:
                if (cp->u.block.seh.data != NULL)
                {
                    if (IR->sehbeg)  /* SEH supported? */
                        (*IR->sehbeg)(cp->u.block.seh.type, cp->u.block.seh.data);
                    else
                        skip = cp->u.block.seh.type == SEH_EXCEPT;
                }
                if (options.dbglevel > 1 && IR->dbgblock && !skip)
                {
                    (*IR->dbgblock)('{', cp->u.block.level - LOCAL, cp->u.block.locals);
                    set_segment(TEXT);
                }
                break;

            case CODE_BLOCKEND:
                if (cp->u.begin->u.block.seh.data != NULL)
                {
                    if (IR->sehend)  /* SEH supported? */
                        (*IR->sehend)(cp->u.begin->u.block.seh.type, cp->u.begin->u.block.seh.data);
                    else if (skip)
                    {
                        skip = 0;
                        break;
                    }
                }
                if (options.dbglevel > 1 && IR->dbgblock)
                {
                    CODE *bp = cp->u.begin;

                    for_each_symbol(bp->u.block.identifiers, bp->u.block.level, typedbg, NULL);
                    for_each_symbol(bp->u.block.types, bp->u.block.level, typedbg, NULL);

                    (*IR->dbgblock)('}', bp->u.block.level - LOCAL, bp->u.block.locals);
                    set_segment(TEXT);
                }
                break;

            case CODE_DEFPOINT:
                src = cp->u.point.src;
                if (options.dbglevel > 0 && IR->dbgline && src.file && src.y)
                {
                    (*IR->dbgline)(&cp->u.point.src);
                    set_segment(TEXT);
                }
                break;

            case CODE_GEN:
            case CODE_JUMP:
            case CODE_LABEL:
                if (cp->u.forest)
                    (*IR->emit)(cp->u.forest);
                break;

            case CODE_LOCAL:
                if (options.dbglevel > 1 && IR->dbgsym)
                {
                    (*IR->dbgsym)(cp->u.var);
                    set_segment(TEXT);
                }
                break;

            case CODE_SWITCH:
            {
                int i;

                define_global(cp->u.swtch.table, LIT);
                (*IR->defaddress)(equated_label(cp->u.swtch.labels[0]));

                for (i = 1; i < cp->u.swtch.size; i++)
                {
                    intmax_t k = cp->u.swtch.values[i-1];
                    while (++k < cp->u.swtch.values[i])
                        assert(k < INTMAX_MAX),

                    (*IR->defaddress)(equated_label(cp->u.swtch.deflab));
                    (*IR->defaddress)(equated_label(cp->u.swtch.labels[i]));
                }
                set_segment(TEXT);
                break;
            }

            case CODE_ASM:
                if (cp->u.asm.src.file != NULL)
                    print("#line %u \"%s\"\n", cp->u.asm.src.y, cp->u.asm.src.file);

                as.emitter.emitinline(cp->u.asm.lab ? cp->u.asm.lab->x.name : NULL,
                    cp->u.asm.text, cp->u.asm.sym, cp->u.asm.cl);
                break;

            case CODE_UNWIND:
                if (cp->u.seh && cp->u.seh->type == SEH_FINALLY && IR->unwind)
                    (*IR->unwind)(cp->u.seh);
                break;

            default:
                assert(0);
        }
    }

    src = save;
}

/****************************************************************************
 *                                                                          *
 * Function: jump                                                           *
 *                                                                          *
 * Purpose : Add a jump.                                                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

NODE *jump(int lab)
{
    SYMBOL *sym = find_label(lab);

    sym->ref++;

    return new_node(JUMP+V,
        new_node(ADDRG+ttob(voidptype), NULL, NULL, sym),
        NULL, NULL);
}

/****************************************************************************
 *                                                                          *
 * Function: label_node                                                     *
 *                                                                          *
 * Purpose : Add a label node to the forest.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void label_node(int lab)
{
    assert(lab);

    if (forest && forest->op == LABEL+V)
        equate_labels(find_label(lab), forest->syms[0]);
    else
        list(new_node(LABEL+V, NULL, NULL, find_label(lab)));

    reset_dagnodes();
}

/****************************************************************************
 *                                                                          *
 * Function: fixup_labels                                                   *
 *                                                                          *
 * Purpose : Fixup references to equated labels.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void fixup_labels(NODE *p)
{
    for (; p != NULL; p = p->link)
    {
        switch (generic(p->op))
        {
            case JUMP:
                if (specific(p->kids[0]->op) == ADDRG+P)
                    p->kids[0]->syms[0] = equated_label(p->kids[0]->syms[0]);
                break;

            case LABEL:
                assert(p->syms[0] == equated_label(p->syms[0]));
                break;

            case EQ:
            case GE:
            case GT:
            case LE:
            case LT:
            case NE:
                assert(p->syms[0]);
                p->syms[0] = equated_label(p->syms[0]);
                break;
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: equated_label                                                  *
 *                                                                          *
 * Purpose : Find ultimate equated label, if any.                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static SYMBOL *equated_label(SYMBOL *sym)
{
    SYMBOL *tmp;

    for (tmp = sym->u.lab.equatedto; tmp != NULL; tmp = tmp->u.lab.equatedto)
        assert(sym != tmp);

    while (sym->u.lab.equatedto != NULL)
        sym = sym->u.lab.equatedto;

    return sym;
}

/****************************************************************************
 *                                                                          *
 * Function: prune_temps                                                    *
 *                                                                          *
 * Purpose : Prune away redundant temporaries.                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static NODE *prune_temps(NODE *forest)
{
    NODE *p, **tail = &forest;
    int count = 0;

    for (p = forest; p != NULL; p = p->link)
    {
        if (count > 0)
        {
            p->kids[0] = replace_temps(p->kids[0]);
            p->kids[1] = replace_temps(p->kids[1]);
        }

        if (generic(p->op) == ASGN &&
            generic(p->kids[0]->op) == ADDRL &&
            p->kids[0]->syms[0]->temporary &&
            p->kids[0]->syms[0]->u.t.cse == p->kids[1])
        {
            SYMBOL *sym = p->kids[0]->syms[0];

            if (!sym->defined)
                (*IR->local)(sym);
            sym->defined = TRUE;

            if ((generic(p->kids[1]->op) == INDIR &&
                isaddrop(p->kids[1]->kids[0]->op) &&
                p->kids[1]->kids[0]->syms[0]->sclass == REGISTER) ||
               ((generic(p->kids[1]->op) == INDIR &&
                isaddrop(p->kids[1]->kids[0]->op)) && sym->sclass == AUTO) ||
                (generic(p->kids[1]->op) == ADDRG && sym->sclass == AUTO))
            {
                sym->u.t.replace = TRUE;
                count++;
                continue;  /* and omit the assignment */
            }
        }

        /* keep the assignment and other roots */
        *tail = p; tail = &(*tail)->link;
    }

    *tail = NULL;

    assert(*tail == NULL);
    return forest;
}

/****************************************************************************
 *                                                                          *
 * Function: replace_temps                                                  *
 *                                                                          *
 * Purpose : Remove any temporaries marked as redundant.                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static NODE *replace_temps(NODE *p)
{
    if (p && (generic(p->op) == INDIR &&
        generic(p->kids[0]->op) == ADDRL &&
        p->kids[0]->syms[0]->temporary &&
        p->kids[0]->syms[0]->u.t.replace))
    {
        p = p->kids[0]->syms[0]->u.t.cse;

        if (generic(p->op) == INDIR && isaddrop(p->kids[0]->op))
            p = new_node(p->op, new_node(p->kids[0]->op, NULL, NULL, p->kids[0]->syms[0]), NULL, NULL);
        else if (generic(p->op) == ADDRG)
            p = new_node(p->op, NULL, NULL, p->syms[0]);
        else
            assert(0);

        p->count = 1;
    }
    else if (p)
    {
        p->kids[0] = replace_temps(p->kids[0]);
        p->kids[1] = replace_temps(p->kids[1]);
    }

    return p;
}

/****************************************************************************
 *                                                                          *
 * Function: typedbg                                                        *
 *                                                                          *
 * Purpose : Emit debugging entries for sym.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void typedbg(SYMBOL *sym, void *cl)
{
    if (!isfunc(sym->type) && (sym->sclass == EXTERN || sym->sclass == STATIC) && IR->dbgsym)
        (*IR->dbgsym)(sym);
    else if ((sym->sclass == TYPEDEF || sym->sclass == 0) && IR->dbgtype)
        (*IR->dbgtype)(sym);
}

/****************************************************************************
 *                                                                          *
 * Function: printdag                                                       *
 *                                                                          *
 * Purpose : Print dag p on fd, or the node list if p == 0.                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#if defined(PODEBUG) || defined(PRERELEASE)
void printdag(NODE *p)
{
    printed(0);

    if (p == NULL)
    {
        if ((p = forest) != NULL)
        {
            do
            {
                p = p->link;
                printdag1(p, 0);
            } while (p != forest);
        }
    }
    else if (*printed(nodeid((TREE *)p)))
    {
        fprint(stdout, NULL, "node'%d printed above\n", nodeid((TREE *)p));
    }
    else
    {
        printdag1(p, 0);
    }
}
#endif

/****************************************************************************
 *                                                                          *
 * Function: printdag1                                                      *
 *                                                                          *
 * Purpose : Recursively print dag p.                                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#if defined(PODEBUG) || defined(PRERELEASE)
static void printdag1(NODE *p, int lev)
{
    int id;
    int i;

    if (p == 0 || *printed(id = nodeid((TREE *)p)))
        return;

    *printed(id) = TRUE;

    for (i = 0; i < NELEMS(p->kids); i++)
        printdag1(p->kids[i], lev+1);

    printnode(p, lev);
}
#endif

/****************************************************************************
 *                                                                          *
 * Function: printnode                                                      *
 *                                                                          *
 * Purpose : Print fields of dag p.                                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#if defined(PODEBUG) || defined(PRERELEASE)
static void printnode(NODE *p, int lev)
{
    if (p)
    {
        int id = nodeid((TREE *)p);
        int i;

        fprint(stdout, NULL, "%c%d%s", (lev == 0) ? '*' : '+', id,
            &"   "[id < 10 ? 0 : id < 100 ? 1 : 2]);
        fprint(stdout, NULL, "%s count=%d", opname(p->op), p->count);

        for (i = 0; i < NELEMS(p->kids) && p->kids[i]; i++)
            fprint(stdout, NULL, " kid#%d", nodeid((TREE *)p->kids[i]));

        if (generic(p->op) == CALL && p->syms[0] && p->syms[0]->type)
        {
            fprint(stdout, NULL, " {%t}", p->syms[0]->type);
            fprint(stdout, NULL, " (%d)", p->syms[0]->u.c.v.i);
        }
        else
        {
            for (i = 0; i < NELEMS(p->syms) && p->syms[i]; i++)
            {
                if (p->syms[i]->name)
                    fprint(stdout, NULL, " name=%s {%t}", p->syms[i]->name, p->syms[i]->type);
                else
                    fprint(stdout, NULL, " [%p]", p->syms[i]);
            }
        }

        if (p->syms[2] && p->syms[2]->x.regnode)
        {
            // char *regs[] = { "EDI", "ESI", "EBX", "ECX", "EDX", "EAX" };
            char *regs[] = { "r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8", "r9", "r10", "fp", "ip", "sp", "lr", "pc" };
            REGNODE rn = p->syms[2]->x.regnode;
            if (rn->number >= 0 && rn->number <= 5)
                fprint(stdout, NULL, " reg=%s", regs[rn->number]);
            else
                fprint(stdout, NULL, " reg=#%d", rn->number);
            if (rn->vbl) fprint(stdout, NULL, "!%s", rn->vbl->name);
        }

        fprint(stdout, NULL, "\n");
    }
}
#endif
