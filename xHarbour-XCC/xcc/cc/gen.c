/****************************************************************************
 *                                                                          *
 * File    : gen.c                                                          *
 *                                                                          *
 * Purpose : ISO C Compiler; Back-end support.                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-03-08  Added operators CEQ, CGE, CGT, CLE, CLT, CNE.        *
 *           04-07-15  Added operators INTRIN1{S}, INTRIN2{S}.              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "lcc.h"

#define readsreg(p) \
    (generic((p)->op) == INDIR && (p)->kids[0]->op == VREG+P)

#define setsrc(d)  ((d) && (d)->x.regnode && \
    (d)->x.regnode->set == src->x.regnode->set && \
    (d)->x.regnode->mask & src->x.regnode->mask)

#define relink(a, b)  ((b)->x.prev = (a), (a)->x.next = (b))

int offset;             /* current local offset */
int maxoffset;          /* max local offset */

int framesize;

int argoffset;          /* current argument offset */
int maxargoffset;       /* max argument offset */

int dalign, salign;
int dflag = 0;          /* debug flag */

int swap;

static char _needs_register[] = {
    0,                      /* unused */
    1,                      /* CNST */
    0, 0,                   /* ARG ASGN */
    1,                      /* INDIR  */
    0, 0, 1, 1,             /*  -  - CVF CVI */
    1, 0, 1, 1,             /* CVP - CVU NEG */
    1,                      /* CALL */
    1,                      /* LOAD */
    0,                      /* RET */
    1, 1, 1,                /* ADDRG ADDRF ADDRL */
    1, 1, 1, 1, 1,          /* ADD SUB LSH MOD RSH */
    1, 1, 1, 1,             /* BAND BCOM BOR BXOR */
    1, 1,                   /* DIV MUL */
    0, 0, 0, 0, 0, 0,       /* EQ GE GT LE LT NE */
    0, 0,                   /* JUMP LABEL */
    1,                      /* CBOOL */
    1, 1, 1, 1, 1, 1,       /* CEQ CGE CGT CLE CLT CNE */
    1, 1, 1, 1              /* INTRIN1 INTRIN1S INTRIN2 INTRIN2S */
};

NODE *head;

uint_t freemask[2];
uint_t usedmask[2];
uint_t tmask[2];
uint_t vmask[2];

/* Static function prototypes */
static void blkunroll(int, int, int, int, int, int, int []);
static int getrule(NODE *, int);
static bool_t register_equate(NODE *);
static bool_t ismoveself(NODE *);
static void docall(NODE *);
static void rewrite(NODE *);
static void before_labelling(NODE *);
static void reduce_tree(NODE *, int);
static NODE *reuse_expression(NODE *, int);
static NODE **prune(NODE *, NODE *[]);
static void linearize_dag(NODE *, NODE *);
static void free_register(SYMBOL *);
static void allocate_register(NODE *);
static SYMBOL *get_register(SYMBOL *, uint_t [], NODE *);
static SYMBOL *find_unused_register(SYMBOL *, uint_t []);
static SYMBOL *use_unused_register(SYMBOL *);
static SYMBOL *best_spillee(SYMBOL *, uint_t [], NODE *);
static bool_t uses_register(NODE *, REGNODE);
static void spill_register(SYMBOL *, NODE *);
static void generate_spill(SYMBOL *, NODE *, SYMBOL *);
static void generate_reload(NODE *, SYMBOL *, int);
static int reprune(NODE **, int, int, NODE *);
static void dumptree(NODE *);
static void dumpcover(NODE *, int, int);
static void dumprule(int);
static void dumpregs(char *, char *, char *);

/****************************************************************************
 *                                                                          *
 * Function: mkreg                                                          *
 *                                                                          *
 * Purpose : Allocate a symbol for a single register.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

SYMBOL *mkreg(char *name, int n, int mask, int set)
{
    SYMBOL *rsym;

    rsym = memalloc(sizeof(*rsym), PERM);
    memset(rsym, 0, sizeof(*rsym));
    rsym->name = rsym->x.name = stringf(name, n);

    rsym->x.regnode = memalloc(sizeof(*rsym->x.regnode), PERM);
    memset(rsym->x.regnode, 0, sizeof(*rsym->x.regnode));
    rsym->x.regnode->number = n;
    rsym->x.regnode->mask = mask<<n;
    rsym->x.regnode->set = set;

    return rsym;
}

/****************************************************************************
 *                                                                          *
 * Function: mkwildcard                                                     *
 *                                                                          *
 * Purpose : Allocate a symbol for a group of register symbols.             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

SYMBOL *mkwildcard(SYMBOL **syms)
{
    SYMBOL *rsym;

    rsym = memalloc(sizeof(*rsym), PERM);
    memset(rsym, 0, sizeof(*rsym));
    rsym->name = rsym->x.name = "wildcard";
    rsym->x.wildcard = syms;

    return rsym;
}

/****************************************************************************
 *                                                                          *
 * Function: mkauto                                                         *
 *                                                                          *
 * Purpose : Calculate stack offset for a local variable.                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           03-08-14  Support for variable-length arrays added.            *
 *                                                                          *
 ****************************************************************************/

void mkauto(SYMBOL *sym)
{
    assert(sym->sclass == AUTO);

    offset = roundup(offset + (isvla(sym->type) ? voidptype->size : sym->type->size),
        isstruct(sym->type) ? IR->structmetric.align : sym->type->align);
    sym->x.offset = -offset;
    sym->x.name = stringd(-offset);
}

/****************************************************************************
 *                                                                          *
 * Function: mkparm                                                         *
 *                                                                          *
 * Purpose : Calculate stack offset for a argument.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

int mkparm(int align, int size)
{
    int n = roundup(argoffset, align);
    argoffset = n + size;
    return n;
}

/****************************************************************************
 *                                                                          *
 * Function: INTERFACE::blockbeg                                            *
 *                                                                          *
 * Purpose : Remember register state at the beginning of a block.           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void blockbeg(ENV *e)
{
    e->offset = offset;
    e->freemask[IREG] = freemask[IREG];
    e->freemask[FREG] = freemask[FREG];
}

/****************************************************************************
 *                                                                          *
 * Function: INTERFACE::blockend                                            *
 *                                                                          *
 * Purpose : Restore register state at the end of a block.                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void blockend(ENV *e)
{
    if (offset > maxoffset)
        maxoffset = offset;

    offset = e->offset;
    freemask[IREG] = e->freemask[IREG];
    freemask[FREG] = e->freemask[FREG];
}

/****************************************************************************
 *                                                                          *
 * Function: blkcopy                                                        *
 *                                                                          *
 * Purpose : Entry point for the block-copy generator.                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void blkcopy(int dreg, int doff, int sreg, int soff, int size, int tmp[])
{
    assert(size >= 0);

    if (size == 0)
    {
        return;
    }
    else if (size <= 2)
    {
        blkunroll(size, dreg, doff, sreg, soff, size, tmp);
    }
    else if (size == 3)
    {
        blkunroll(2, dreg, doff,   sreg, soff,   2, tmp);
        blkunroll(1, dreg, doff+2, sreg, soff+2, 1, tmp);
    }
    else if (size <= 16)
    {
        blkunroll(4, dreg, doff, sreg, soff, size&~3, tmp);
        blkcopy(dreg, doff+(size&~3), sreg, soff+(size&~3), size&3, tmp);
    }
    else
    {
        (*IR->x.blkloop)(dreg, doff, sreg, soff, size, tmp);
    }
}

/****************************************************************************
 *                                                                          *
 * Function: blkunroll                                                      *
 *                                                                          *
 * Purpose : Unroll a block-copy loop.                                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void blkunroll(int k, int dreg, int doff, int sreg, int soff, int size, int tmp[])
{
    int i;

    assert(IR->x.max_unaligned_load);

    if (k > IR->x.max_unaligned_load && (k > salign || k > dalign))
        k = IR->x.max_unaligned_load;

    for (i = 0; i+k < size; i += 2*k)
    {
        (*IR->x.blkfetch)(k, soff+i,   sreg, tmp[0]);
        (*IR->x.blkfetch)(k, soff+i+k, sreg, tmp[1]);
        (*IR->x.blkstore)(k, doff+i,   dreg, tmp[0]);
        (*IR->x.blkstore)(k, doff+i+k, dreg, tmp[1]);
    }

    if (i < size)
    {
        (*IR->x.blkfetch)(k, i+soff, sreg, tmp[0]);
        (*IR->x.blkstore)(k, i+doff, dreg, tmp[0]);
    }
}

/****************************************************************************
 *                                                                          *
 * Function: may_recalc_temp                                                *
 *                                                                          *
 * Purpose : Return true if a temporary value may be recalculated.          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

bool_t may_recalc_temp(NODE *p)
{
    int op;

    assert(p && p->syms[RX]);

    if (p->syms[RX]->u.t.cse == NULL)
        return FALSE;

    op = generic(p->syms[RX]->u.t.cse->op);
    if (op == CNST || op == ADDRF || op == ADDRG || op == ADDRL)
    {
        p->x.may_recalc = TRUE;
        return TRUE;
    }

    return FALSE;
}

/****************************************************************************
 *                                                                          *
 * Function: range                                                          *
 *                                                                          *
 * Purpose : Perform range checking; return cost.                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

int range(NODE *p, int lo, int hi)
{
    SYMBOL *sym = p->syms[0];

    switch (specific(p->op))
    {
        case ADDRF+P: case ADDRL+P:
            return (sym->x.offset >= lo && sym->x.offset <= hi) ? 0 : LBURG_MAX;
        case CNST+I:
            return (sym->u.c.v.i >= lo && sym->u.c.v.i <= hi) ? 0 : LBURG_MAX;
        case CNST+U:
            return (sym->u.c.v.u >= (unsigned)lo && sym->u.c.v.u <= (unsigned)hi) ? 0 : LBURG_MAX;
        case CNST+P:
            return (sym->u.c.v.p == 0 && lo <= 0 && hi >= 0) ? 0 : LBURG_MAX;
    }

    return LBURG_MAX;
}

/****************************************************************************
 *                                                                          *
 * Function: notarget                                                       *
 *                                                                          *
 * Purpose : Check for register targeting; return cost.                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#if 0
int notarget(NODE *p)
{
    return p->syms[RX]->x.wildcard ? 0 : LBURG_MAX;
}
#endif

/****************************************************************************
 *                                                                          *
 * Function: getrule                                                        *
 *                                                                          *
 * Purpose : Find a matching rule.                                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int getrule(NODE *p, int nt)
{
    int rulenum;

    assert(p);

    rulenum = (*IR->x._rule)(p->x.state, nt);
    if (!rulenum)
    {
        fprint(stdout, NULL, "(%x->op=%s at %w is corrupt.)\n", p, opname(p->op), &src);
        fflush(stdout);
        assert(0);
    }

    return rulenum;
}

/****************************************************************************
 *                                                                          *
 * Function: emitasm                                                        *
 *                                                                          *
 * Purpose : Emit assembly code for a matching rule.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-07-19  Revised code for self clobbering fix (a = b - a).    *
 *                                                                          *
 ****************************************************************************/

void emitasm(NODE *p, int nt)
{
    int rulenum;
    short *nts;
    char *fmt;
    NODE *kids[10];

    p = reuse_expression(p, nt);

    rulenum = getrule(p, nt);
    nts = IR->x._nts[rulenum];
    fmt = IR->x._templates[rulenum];
    assert(fmt);

    if (IR->x._isinstruction[rulenum] && p->x.emitted)
    {
        print("%s", p->syms[RX]->x.name);
    }
    else if (*fmt == '#')
    {
        (*IR->x.emit2)(p);
    }
    else
    {
        if (*fmt == '?')
        {
            fmt++;

            /* compare actual names, otherwise it sometimes fail (not sure why) */
            if (p->x.kids[1] && strcmp(p->syms[RX]->x.name, p->x.kids[1]->syms[RX]->x.name) == 0)
            {
                while (*fmt++ != '?')
                    ;
            }
            else
            {
                assert(p->kids[0]);
                if (p->syms[RX] == p->x.kids[0]->syms[RX])
                {
                    while (*fmt++ != '\n')
                        ;
                }
            }
        }

        for ((*IR->x._kids)(p, rulenum, kids); *fmt != '\0' && *fmt != '?'; fmt++)
        {
            if (*fmt != '%')
                print("%c", *fmt);
            else if (*++fmt == 'F')
                print("%d", framesize);
            else if (*fmt >= '0' && *fmt <= '9')
                emitasm(kids[*fmt - '0'], nts[*fmt - '0']);
            else if (*fmt >= 'a' && *fmt < 'a' + NELEMS(p->syms))
                print("%s", p->syms[*fmt - 'a']->x.name);
            else
                print("%c", *fmt);
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: INTERFACE::emit                                                *
 *                                                                          *
 * Purpose : Emit the linearized forest.                                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void emit(NODE *p)
{
    for (; p != NULL; p = p->x.next)
    {
        assert(p->x.registered);

        if (p->x.equatable && register_equate(p) || ismoveself(p))
            ;
        else
            emitasm(p, p->x.inst);

        p->x.emitted = TRUE;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: register_equate                                                *
 *                                                                          *
 * Purpose : Eliminates some register-to-register copies.                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static bool_t register_equate(NODE *q)
{
    SYMBOL *src = q->x.kids[0]->syms[RX];
    SYMBOL *tmp = q->syms[RX];
    NODE *p;
    int n = 0;

    debug(fprint(stdout, NULL, "(register_equate(%x): tmp=%s src=%s)\n", q, tmp->x.name, src->x.name));

    for (p = q->x.next; p != NULL; p = p->x.next)
    {
        if (p->x.copy && p->syms[RX] == src && p->x.kids[0]->syms[RX] == tmp)
            debug(fprint(stdout, NULL, "(register_equate arm 0 at %x)\n", p)), p->syms[RX] = tmp;
        else if (setsrc(p->syms[RX]) && !ismoveself(p) && !readsreg(p))
            return FALSE;
        else if (p->x.spills)
            return FALSE;
        else if (generic(p->op) == CALL && p->x.next)
            return FALSE;
        else if (p->op == LABEL+V && p->x.next)
            return FALSE;
        else if (p->syms[RX] == tmp && readsreg(p))
            debug(fprint(stdout, NULL, "(register_equate arm 5 at %x)\n", p)), n++;
        else if (p->syms[RX] == tmp)
            break;
    }

    debug(fprint(stdout, NULL, "(register_equate arm 7 at %x)\n", p));

    assert(n > 0);
    for (p = q->x.next; p != NULL; p = p->x.next)
    {
        if (p->syms[RX] == tmp && readsreg(p))
        {
            p->syms[RX] = src;
            if (--n <= 0) break;
        }
    }

    return TRUE;
}

/****************************************************************************
 *                                                                          *
 * Function: ismoveself                                                     *
 *                                                                          *
 * Purpose : Return true if instruction copy a register to itself.          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static bool_t ismoveself(NODE *p)
{
    assert(p);
    assert(!p->x.copy || p->syms[RX] && p->x.kids[0] && p->x.kids[0]->syms[RX]);
    return p->x.copy && p->syms[RX]->x.name == p->x.kids[0]->syms[RX]->x.name;
}

/****************************************************************************
 *                                                                          *
 * Function: move                                                           *
 *                                                                          *
 * Purpose : Set register copy flag, return cost.                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

int move(NODE *p)
{
    p->x.copy = TRUE;
    return 1;
}

/****************************************************************************
 *                                                                          *
 * Function: INTERFACE::gencode                                             *
 *                                                                          *
 * Purpose : Generate code, i guess...                                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

NODE *gencode(NODE *forest)
{
    int i;
    NODE sentinel;
    NODE *dummy;
    NODE *p;

    head = forest;
    for (p = forest; p != NULL; p = p->link)
    {
        assert(p->count == 0);

        if (generic(p->op) == CALL)
            docall(p);
        else if (generic(p->op) == ASGN && generic(p->kids[1]->op) == CALL)
            docall(p->kids[1]);
        else if (generic(p->op) == ARG)
            (*IR->x.doarg)(p);

        rewrite(p);
        p->x.listed = TRUE;
    }

    for (p = forest; p != NULL; p = p->link)
        prune(p, &dummy);

    relink(&sentinel, &sentinel);

    for (p = forest; p != NULL; p = p->link)
        linearize_dag(p, &sentinel);

    forest = sentinel.x.next;
    assert(forest);
    sentinel.x.next->x.prev = NULL;
    sentinel.x.prev->x.next = NULL;

    /* build a list of all the nodes that use each temporary */
    for (p = forest; p != NULL; p = p->x.next)
    {
        for (i = 0; i < NELEMS(p->x.kids) && p->x.kids[i]; i++)
        {
            assert(p->x.kids[i]->syms[RX]);
            if (p->x.kids[i]->syms[RX]->temporary)
            {
                p->x.kids[i]->x.prevuse = p->x.kids[i]->syms[RX]->x.lastuse;
                p->x.kids[i]->syms[RX]->x.lastuse = p->x.kids[i];
            }
        }
    }

    /* allocate a register for each node */
    for (p = forest; p != NULL; p = p->x.next)
    {
        allocate_register(p);
        if (p->x.listed && _needs_register[opindex(p->op)] && (*IR->x.rmap)(opkind(p->op)))
        {
            assert(generic(p->op) == CALL || generic(p->op) == LOAD || generic(p->op) == INTRIN1S || generic(p->op) == INTRIN2S);
            free_register(p->syms[RX]);
        }
    }

    return forest;
}

/****************************************************************************
 *                                                                          *
 * Function: docall                                                         *
 *                                                                          *
 * Purpose : End the list of arguments for a CALL node.                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void docall(NODE *p)
{
    p->syms[1] = p->syms[0];
    p->syms[0] = intconst(argoffset);

    if (argoffset > maxargoffset)
        maxargoffset = argoffset;

    argoffset = 0;
}

/****************************************************************************
 *                                                                          *
 * Function: rewrite                                                        *
 *                                                                          *
 * Purpose : Drives before_labelling(), IR->_label() and reduce_tree().     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void rewrite(NODE *p)
{
    assert(p->x.inst == 0);

    before_labelling(p);

    debug(dumptree(p));
    debug(fprint(stdout, NULL, "\n"));

    /*
     * Make a bottom-up, left-to-right pass over the subject tree p
     * computing the rules that cover the tree with the minimum cost,
     * if there is such a cover.
     */
    (*IR->x._label)(p);

    debug(dumpcover(p, 1, 0));

    /* reduce the subject tree */
    reduce_tree(p, 1);
}

/****************************************************************************
 *                                                                          *
 * Function: before_labelling                                               *
 *                                                                          *
 * Purpose : Change the tree to cope with register variables and special    *
 *           targets.                                                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void before_labelling(NODE *p)
{
    if (p == NULL)
        return;

    before_labelling(p->kids[0]);
    before_labelling(p->kids[1]);

    if (_needs_register[opindex(p->op)])
        set_register(p, (*IR->x.rmap)(opkind(p->op)));  /* wildcard */

    switch (generic(p->op))
    {
        case ADDRF:
        case ADDRL:
            if (p->syms[0]->sclass == REGISTER)
                p->op = VREG+P;
            break;

        case INDIR:
            if (p->kids[0]->op == VREG+P)
                set_register(p, p->kids[0]->syms[0]);
            break;

        case ASGN:
            if (p->kids[0]->op == VREG+P)
                target_register(p, 1, p->kids[0]->syms[0]);
            break;

        case CVI:
        case CVU:
        case CVP:
            if (optype(p->op) != F &&
                opsize(p->op) <= p->syms[0]->u.c.v.i)
                p->op = LOAD + opkind(p->op);
            break;
    }

    (IR->x.target)(p);
}

/****************************************************************************
 *                                                                          *
 * Function: reduce_tree                                                    *
 *                                                                          *
 * Purpose : Select the cheapest implementation.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void reduce_tree(NODE *p, int nt)
{
    int rulenum, i;
    short *nts;
    NODE *kids[10];

    p = reuse_expression(p, nt);

    rulenum = getrule(p, nt);
    nts = IR->x._nts[rulenum];
    (*IR->x._kids)(p, rulenum, kids);

    for (i = 0; nts[i]; i++)
        reduce_tree(kids[i], nts[i]);

    if (IR->x._isinstruction[rulenum])
    {
        assert(p->x.inst == 0 || p->x.inst == nt);
        if (p->x.inst != 0 && p->x.inst != nt)  /* extra check needed for non-debug builds */
            apperror(RCWARNING(ERROR_INTERNAL), "reduce_tree()");

        p->x.inst = nt;
        if (p->syms[RX] && p->syms[RX]->temporary)
        {
            debug(fprint(stdout, NULL, "(using %s)\n", p->syms[RX]->name));
            p->syms[RX]->x.usecount++;
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: reuse_expression                                               *
 *                                                                          *
 * Purpose : Reuse a previously calculated value (from a temp).             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static NODE *reuse_expression(NODE *p, int nt)
{
    struct _state { short cost[1]; };
    SYMBOL *rsym = p->syms[RX];

    if (generic(p->op) == INDIR && p->kids[0]->op == VREG+P &&
        rsym->u.t.cse && p->x.may_recalc &&
        ((struct _state *)rsym->u.t.cse->x.state)->cost[nt] == 0)
        return rsym->u.t.cse;
    else
        return p;
}

/****************************************************************************
 *                                                                          *
 * Function: prune                                                          *
 *                                                                          *
 * Purpose : Projects subinstructions out of the tree.                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static NODE **prune(NODE *p, NODE *pp[])
{
    if (p == NULL)
        return pp;

    p->x.kids[0] = p->x.kids[1] = p->x.kids[2] = NULL;

    if (p->x.inst == 0)
    {
        return prune(p->kids[1], prune(p->kids[0], pp));
    }
    else if (p->syms[RX] && p->syms[RX]->temporary && p->syms[RX]->x.usecount < 2)
    {
        p->x.inst = 0;
        debug(fprint(stdout, NULL, "(clobbering %s)\n", p->syms[RX]->name));
        return prune(p->kids[1], prune(p->kids[0], pp));
    }
    else
    {
        prune(p->kids[1], prune(p->kids[0], &p->x.kids[0]));
        *pp = p;
        return pp + 1;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: linearize_dag                                                  *
 *                                                                          *
 * Purpose : Linearize the dag at p.                                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void linearize_dag(NODE *p, NODE *next)
{
    int i;

    for (i = 0; i < NELEMS(p->x.kids) && p->x.kids[i]; i++)
        linearize_dag(p->x.kids[i], next);

    relink(next->x.prev, p);
    relink(p, next);

    debug(fprint(stdout, NULL, "(listing %x)\n", p));
}

/****************************************************************************
 *                                                                          *
 * Function: target_register                                                *
 *                                                                          *
 * Purpose : Target register for a node.                                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void target_register(NODE *p, int n, SYMBOL *rsym)
{
    NODE *q = p->kids[n];

    assert(q);
    assert(rsym);
    assert(rsym->sclass == REGISTER || !rsym->x.wildcard);
    assert(q->syms[RX]);

    if (rsym != q->syms[RX] && !q->syms[RX]->x.wildcard)
    {
        q = new_node(LOAD + opkind(q->op), q, NULL, q->syms[0]);

        if (rsym->u.t.cse == p->kids[n])
            rsym->u.t.cse = q;

        p->kids[n] = p->x.kids[n] = q;
        q->x.kids[0] = q->kids[0];
    }

    set_register(q, rsym);

    assert(rsym->x.name);
    debug(fprint(stdout, NULL, "(targeting %x->x.kids[%d]=%x to %s)\n", p, n, p->kids[n], rsym->x.name));
}

/****************************************************************************
 *                                                                          *
 * Function: set_register                                                   *
 *                                                                          *
 * Purpose : Set register for a node.                                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void set_register(NODE *p, SYMBOL *rsym)
{
    p->syms[RX] = rsym;
}

/****************************************************************************
 *                                                                          *
 * Function: free_register                                                  *
 *                                                                          *
 * Purpose : Free a register and flag it as available.                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void free_register(SYMBOL *rsym)
{
    assert(rsym && rsym->x.regnode);
    freemask[rsym->x.regnode->set] |= rsym->x.regnode->mask;
    /* don't clear usedmask */
    debug(dumpregs("(freeing %s)\n", rsym->x.name, NULL));
}

/****************************************************************************
 *                                                                          *
 * Function: register_variable                                              *
 *                                                                          *
 * Purpose : Try to assign a local variable to a register.                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

int register_variable(SYMBOL *sym, SYMBOL *regs)
{
    SYMBOL *rsym;

    assert(sym);

    if (sym->sclass != REGISTER)
    {
        return FALSE;
    }
    else if (!isscalar(sym->type))
    {
        sym->sclass = AUTO;
        return FALSE;
    }
    else if (sym->temporary)
    {
        sym->x.name = "?";
        return TRUE;
    }
    else if ((rsym = find_unused_register(regs, vmask)) != NULL)
    {
        sym->x.regnode = rsym->x.regnode;
        sym->x.regnode->vbl = sym;
        sym->x.name = rsym->x.name;
        debug(dumpregs("(allocating %s to symbol %s)\n", sym->x.name, sym->name));
        return TRUE;
    }
    else
    {
        sym->sclass = AUTO;
        return FALSE;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: allocate_register                                              *
 *                                                                          *
 * Purpose : Handle register allocation for a single node.                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-07-19  Revised code for self clobbering fix (a = b - a).    *
 *                                                                          *
 ****************************************************************************/

static void allocate_register(NODE *p)
{
    uint_t mask[2];
    int i;

    mask[IREG] = tmask[IREG];
    mask[FREG] = tmask[FREG];
    assert(p);

    debug(fprint(stdout, NULL, "(regalloc %x)\n", p));

    for (i = 0; i < NELEMS(p->x.kids) && p->x.kids[i]; i++)
    {
        NODE *kid = p->x.kids[i];
        SYMBOL *rsym = kid->syms[RX];
        assert(rsym && kid->x.registered);

        /* if a child yields a register variable, or if the register holds a
         * common subexpression for which other uses remain, then its register
         * must not be freed.
         */
        if (rsym->sclass != REGISTER && rsym->x.lastuse == kid)
            free_register(rsym);
    }

    if (!p->x.registered && _needs_register[opindex(p->op)] && (*IR->x.rmap)(opkind(p->op)))
    {
        SYMBOL *sym = p->syms[RX], *set = sym;
        assert(sym);

        if (sym->temporary)
            set = (*IR->x.rmap)(opkind(p->op));

        assert(set);
        if (set->sclass != REGISTER)
        {
            SYMBOL *rsym;

            if (*IR->x._templates[getrule(p, p->x.inst)] == '?')
            {
                /* prevent reallocation of all input registers but the first */
                for (i = 1; i < NELEMS(p->x.kids) && p->x.kids[i]; i++)
                {
                    SYMBOL *rsym = p->x.kids[i]->syms[RX];
                    assert(p->x.kids[i]->x.registered);
                    assert(rsym && rsym->x.regnode);
                    /*
                     * "The code generators must take care that no node targets the same
                     * register as any of its children except the first one."
                     * (the self clobbering fix should take care of this - the assert is not valid)
                     */
                    /* assert(rsym != sym || sym->x.wildcard); */
                    mask[rsym->x.regnode->set] &= ~rsym->x.regnode->mask;
                }
            }

            rsym = get_register(set, mask, p);
            if (sym->temporary)
            {
                NODE *q;

                rsym->x.lastuse = sym->x.lastuse;
                for (q = sym->x.lastuse; q != NULL; q = q->x.prevuse)
                {
                    q->syms[RX] = rsym;
                    q->x.registered = TRUE;

                    if (sym->u.t.cse && q->x.copy)
                        q->x.equatable = TRUE;
                }
            }
            else
            {
                p->syms[RX] = rsym;
                rsym->x.lastuse = p;
            }

            debug(dumpregs("(allocating %s to node %x)\n", rsym->x.name, (char *)p));
        }
    }

    p->x.registered = TRUE;
    (*IR->x.clobber)(p);
}

/****************************************************************************
 *                                                                          *
 * Function: get_register                                                   *
 *                                                                          *
 * Purpose : Allocate a register for a node.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static SYMBOL *get_register(SYMBOL *sym, uint_t mask[], NODE *p)
{
    SYMBOL *rsym = find_unused_register(sym, mask);
    if (rsym == NULL)
    {
        rsym = best_spillee(sym, mask, p);
        assert(rsym && rsym->x.regnode);
        spill(rsym->x.regnode->mask, rsym->x.regnode->set, p);
        rsym = find_unused_register(sym, mask);
    }

    assert(rsym && rsym->x.regnode);
    rsym->x.regnode->vbl = NULL;
    return rsym;
}

/****************************************************************************
 *                                                                          *
 * Function: find_unused_register                                           *
 *                                                                          *
 * Purpose : Try to find an unused register (in set limited by mask).       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static SYMBOL *find_unused_register(SYMBOL *rs, uint_t mask[])
{
    int i;

    if (rs->x.wildcard == NULL)
        return use_unused_register(rs);

    for (i = 31; i >= 0; i--)
    {
        SYMBOL *rsym = rs->x.wildcard[i];

        if (rsym != NULL &&
            !(rsym->x.regnode->mask & ~mask[rsym->x.regnode->set]) &&
            use_unused_register(rsym))
        {
            return rsym;
        }
    }

    return NULL;
}

/****************************************************************************
 *                                                                          *
 * Function: use_unused_register                                            *
 *                                                                          *
 * Purpose : Use the given register if it's available.                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static SYMBOL *use_unused_register(SYMBOL *rsym)
{
    REGNODE rn = rsym->x.regnode;
    int n = rn->set;

    if (rn->mask & ~freemask[n])
        return NULL;
    else
    {
        freemask[n] &= ~rn->mask;
        usedmask[n] |=  rn->mask;
        return rsym;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: best_spillee                                                   *
 *                                                                          *
 * Purpose : Identify register to be spilled.                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static SYMBOL *best_spillee(SYMBOL *set, uint_t mask[], NODE *here)
{
    SYMBOL *best_rsym = NULL;

    assert(set);
    if (!set->x.wildcard)
    {
        best_rsym = set;
    }
    else
    {
        int best_dist = -1;
        int i;

        for (i = 31; i >= 0; i--)
        {
            SYMBOL *rsym = set->x.wildcard[i];

            if (rsym != NULL && rsym->x.lastuse && (rsym->x.regnode->mask &
                tmask[rsym->x.regnode->set] & mask[rsym->x.regnode->set]))
            {
                REGNODE rn = rsym->x.regnode;
                int dist = 0;
                NODE *p;

                for (p = here; p && !uses_register(p, rn); p = p->x.next)
                    dist++;

                if (p && dist > best_dist)
                {
                    best_dist = dist;
                    best_rsym = rsym;
                }
            }
        }
    }

    assert(best_rsym); /* Must be able to spill something. Reconfigure the
        register allocator to ensure that we can allocate a register for
        all nodes without spilling the node's necessary input regs. */

    if (!best_rsym)  /* extra check needed for non-debug builds */
        apperror(RCFATAL(ERROR_INTERNAL), "best_spillee");

    if (best_rsym->x.regnode->vbl)
    {
        printf("can't spill register variable: %s (%d) %s\n",
            best_rsym->name, best_rsym->x.regnode->number,
            best_rsym->x.regnode->vbl->name);
        apperror(RCFATAL(ERROR_INTERNAL), "best_spillee");
    }

    assert(best_rsym->x.regnode->vbl == NULL); /* Can't spill register
        variables because the reload site might be in other blocks.
        Reconfigure the register allocator to ensure that this register
        is never allocated to a variable. */

    return best_rsym;
}

/****************************************************************************
 *                                                                          *
 * Function: uses_register                                                  *
 *                                                                          *
 * Purpose : Return true if the register is used in the tree.               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static bool_t uses_register(NODE *p, REGNODE rn)
{
    int i;

    for (i = 0; i < NELEMS(p->x.kids); i++)
    {
        if (p->x.kids[i] && p->x.kids[i]->x.registered &&
            rn->set == p->x.kids[i]->syms[RX]->x.regnode->set &&
           (rn->mask & p->x.kids[i]->syms[RX]->x.regnode->mask))
        {
            return TRUE;
        }
    }

    return FALSE;
}

/****************************************************************************
 *                                                                          *
 * Function: spill                                                          *
 *                                                                          *
 * Purpose : Spill manager.                                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void spill(uint_t mask, int n, NODE *here)
{
    int i;
    NODE *p;

    here->x.spills = TRUE;
    usedmask[n] |= mask;
    if (mask & ~freemask[n])
    {
        assert( /* It makes no sense for a node to clobber() its target */
            here->x.registered == FALSE || /* call isn't coming through clobber() */
            here->syms[RX] == NULL ||
            here->syms[RX]->x.regnode == NULL ||
            here->syms[RX]->x.regnode->set != n ||
           (here->syms[RX]->x.regnode->mask & mask) == 0);

        for (p = here; p != NULL; p = p->x.next)
        {
            for (i = 0; i < NELEMS(p->x.kids) && p->x.kids[i]; i++)
            {
                SYMBOL *rsym = p->x.kids[i]->syms[RX];
                assert(rsym);

                if (p->x.kids[i]->x.registered &&
                    rsym->x.regnode->set == n &&
                    rsym->x.regnode->mask & mask)
                {
                    spill_register(rsym, here);
                }
            }
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: spill_register                                                 *
 *                                                                          *
 * Purpose : Handle spilling and reloading of a register.                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void spill_register(SYMBOL *rsym, NODE *here)
{
    NODE *p = rsym->x.lastuse;
    SYMBOL *tmp;

    assert(p);
    while (p->x.prevuse)
        assert(rsym == p->syms[RX]), p = p->x.prevuse;
    assert(p->x.registered && !readsreg(p));

    tmp = define_local(AUTO, optype(p->op), opsize(p->op));
    generate_spill(rsym, p, tmp);

    for (p = here->x.next; p != NULL; p = p->x.next)
    {
        int i;
        for (i = 0; i < NELEMS(p->x.kids) && p->x.kids[i]; i++)
        {
            NODE *q = p->x.kids[i];

            if (q->x.registered && q->syms[RX] == rsym)
                generate_reload(p, tmp, i);
        }
    }

    /* we are free for duty */
    free_register(rsym);
}

/****************************************************************************
 *                                                                          *
 * Function: generate_spill                                                 *
 *                                                                          *
 * Purpose : Generate code to spill a register to temporary storage.        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void generate_spill(SYMBOL *rsym, NODE *last, SYMBOL *tmp)
{
    NODE *p, *q;
    SYMBOL *sym;
    uint_t ty;

    debug(fprint(stdout, NULL, "(spilling %s to local %s)\n", rsym->x.name, tmp->x.name));
    debug(fprint(stdout, NULL, "(generate_spill: "));
    debug(dumptree(last));
    debug(fprint(stdout, NULL, ")\n"));

    ty = opkind(last->op);

    sym = memalloc(sizeof(*sym), funca);
    memset(sym, 0, sizeof(*sym));
    sym->sclass = REGISTER;
    sym->name = sym->x.name = rsym->x.name;
    sym->x.regnode = rsym->x.regnode;

    q = new_node(ADDRL+P + sizeop(IR->ptrmetric.size), NULL, NULL, sym);
    q = new_node(INDIR + ty, q, NULL, NULL);
    p = new_node(ADDRL+P + sizeop(IR->ptrmetric.size), NULL, NULL, tmp);
    p = new_node(ASGN + ty, p, q, NULL);
    p->x.spills = TRUE;

    rewrite(p);
    prune(p, &q);
    q = last->x.next;
    linearize_dag(p, q);

    for (p = last->x.next; p != q; p = p->x.next)
    {
        allocate_register(p);
        assert(!p->x.listed || !_needs_register[opindex(p->op)] || !(*IR->x.rmap)(opkind(p->op)));
    }
}

/****************************************************************************
 *                                                                          *
 * Function: generate_reload                                                *
 *                                                                          *
 * Purpose : Generate code to reload a register from temporary storage.     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void generate_reload(NODE *p, SYMBOL *tmp, int i)
{
    NODE *q;
    int ty;

    debug(fprint(stdout, NULL, "(replacing %x with a reload from %s)\n", p->x.kids[i], tmp->x.name));
    debug(fprint(stdout, NULL, "(generate_reload: "));
    debug(dumptree(p->x.kids[i]));
    debug(fprint(stdout, NULL, ")\n"));

    ty = opkind(p->x.kids[i]->op);
    q = new_node(ADDRL+P + sizeop(IR->ptrmetric.size), NULL, NULL, tmp);
    p->x.kids[i] = new_node(INDIR + ty, q, NULL, NULL);

    rewrite(p->x.kids[i]);
    prune(p->x.kids[i], &q);
    reprune(&p->kids[1], reprune(&p->kids[0], 0, i, p), i, p);
    prune(p, &q);
    linearize_dag(p->x.kids[i], p);
}

/****************************************************************************
 *                                                                          *
 * Function: reprune                                                        *
 *                                                                          *
 * Purpose : Incremental version of prune, after reloads has been added.    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int reprune(NODE **pp, int k, int n, NODE *p)
{
    NODE x, *q = *pp;

    if (q == NULL || k > n)
        return k;
    else if (q->x.inst == 0)
        return reprune(&q->kids[1], reprune(&q->kids[0], k, n, p), n, p);

    if (k == n)
    {
        debug(fprint(stdout, NULL, "(reprune changes %x from %x to %x)\n", pp, *pp, p->x.kids[n]));
        *pp = p->x.kids[n];
        x = *p;
        (IR->x.target)(&x);
    }

    return k + 1;
}

/****************************************************************************
 *                                                                          *
 * Function: register_number                                                *
 *                                                                          *
 * Purpose : Return register number for the given node.                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

int register_number(NODE *p)
{
    assert(p && p->syms[RX] && p->syms[RX]->x.regnode);
    return p->syms[RX]->x.regnode->number;
}

/****************************************************************************
 *                                                                          *
 * Function: register_location                                              *
 *                                                                          *
 * Purpose : Reserved for future use.                                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#if 0
uint_t register_location(SYMBOL *rsym)
{
    assert(rsym && rsym->sclass == REGISTER && rsym->x.regnode);
    return rsym->x.regnode->set<<8 | rsym->x.regnode->number;
}
#endif

/****************************************************************************
 *                                                                          *
 * Function: dumptree                                                       *
 *                                                                          *
 * Purpose : Debugging.                                                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-03-08  Added operators CEQ, CGE, CGT, CLE, CLT, CNE.        *
 *           04-07-15  Added operators INTRIN1{S}, INTRIN2{S}.              *
 *                                                                          *
 ****************************************************************************/

#if defined(PODEBUG) || defined(PRERELEASE)
static void dumptree(NODE *p)
{
    if (p->op == VREG+P && p->syms[0])
    {
        fprint(stdout, NULL, "VREGP(%s)", p->syms[0]->name);
        return;
    }
    else if (generic(p->op) == LOAD)
    {
        fprint(stdout, NULL, "%s(", opname(p->op));
        dumptree(p->kids[0]);
        fprint(stdout, NULL, ")");
        return;
    }

    fprint(stdout, NULL, "%s(", opname(p->op));
    switch (generic(p->op))
    {
        case CNST:
        case LABEL:
        case ADDRG:
        case ADDRF:
        case ADDRL:
            if (p->syms[0])
                fprint(stdout, NULL, "%s", p->syms[0]->name);
            break;

        case RET:
            if (p->kids[0])
                dumptree(p->kids[0]);
            break;

        case CVF:
        case CVI:
        case CVP:
        case CVU:
        case CBOOL:
        case JUMP:
        case ARG:
        case BCOM:
        case NEG:
        case INDIR:
        case INTRIN1:
        case INTRIN1S:  /* with side effects */
            dumptree(p->kids[0]);
            break;

        case CALL:
            if (optype(p->op) != B)
            {
                dumptree(p->kids[0]);
                break;
            }
            /* else fall through */
        case EQ:
        case NE:
        case GT:
        case GE:
        case LE:
        case LT:
        case CEQ:
        case CNE:
        case CGT:
        case CGE:
        case CLE:
        case CLT:
        case ASGN:
        case BOR:
        case BAND:
        case BXOR:
        case RSH:
        case LSH:
        case ADD:
        case SUB:
        case DIV:
        case MUL:
        case MOD:
        case INTRIN2:
        case INTRIN2S:  /* with side effects */
            dumptree(p->kids[0]);
            fprint(stdout, NULL, ", ");
            dumptree(p->kids[1]);
            break;

        default:
            assert(0);
    }
    fprint(stdout, NULL, ")");
}
#endif

/****************************************************************************
 *                                                                          *
 * Function: dumpcover                                                      *
 *                                                                          *
 * Purpose : Debugging.                                                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#if defined(PODEBUG)
static void dumpcover(NODE *p, int nt, int in)
{
    int rulenum, i;
    short *nts;
    NODE *kids[10];

    p = reuse_expression(p, nt);

    rulenum = getrule(p, nt);
    nts = IR->x._nts[rulenum];
    fprint(stdout, NULL, "dumpcover(%x) = ", p);

    for (i = 0; i < in; i++)
        fprint(stdout, NULL, " ");

    dumprule(rulenum);
    (*IR->x._kids)(p, rulenum, kids);

    for (i = 0; nts[i]; i++)
        dumpcover(kids[i], nts[i], in+1);
}
#endif

/****************************************************************************
 *                                                                          *
 * Function: dumprule                                                       *
 *                                                                          *
 * Purpose : Debugging.                                                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#if defined(PODEBUG)
static void dumprule(int rulenum)
{
    assert(rulenum);

    fprint(stdout, NULL, "%s / %s", IR->x._string[rulenum], IR->x._templates[rulenum]);

    if (!IR->x._isinstruction[rulenum])
        fprint(stdout, NULL, "\n");
}
#endif

/****************************************************************************
 *                                                                          *
 * Function: dumpregs                                                       *
 *                                                                          *
 * Purpose : Debugging.                                                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#if defined(PODEBUG)
static void dumpregs(char *msg, char *a, char *b)
{
    fprint(stdout, NULL, msg, a, b);
    fprint(stdout, NULL, "(free[IREG]=%x)\n", freemask[IREG]);
    fprint(stdout, NULL, "(free[FREG]=%x)\n", freemask[FREG]);
}
#endif

