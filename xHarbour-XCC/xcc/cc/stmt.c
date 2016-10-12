/****************************************************************************
 *                                                                          *
 * File    : stmt.c                                                         *
 *                                                                          *
 * Purpose : ISO C Compiler; Statement parsing.                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "lcc.h"

#define SWSIZE  512

CODE codehead = { CODE_START };
CODE *codelist = &codehead;

TABLE *stmtlabs;

/* Locals */
static float density = 0.5;

/* Static function prototypes */
static void parse_if_statement(int, int, SWTCH *, SEH *, int);
static void parse_while_statement(int, SWTCH *, SEH *, int);
static void parse_do_statement(int, SWTCH *, SEH *, int);
static void parse_for_statement(int, SWTCH *, SEH *, int);
static int fold_for_declaration_condition(CODE *, TREE *);
static int fold_for_condition(TREE *, TREE *);
static void parse_break_statement(int, SWTCH *);
static void parse_continue_statement(int);
static void parse_switch_statement(int, int, SEH *, int);
static void parse_case_statement(int, SWTCH *);
static void parse_default_statement(SWTCH *);
static void add_case_label(SWTCH *, intmax_t, int);
static void generate_switch_table(SWTCH *);
static void generate_switch_code(SWTCH *, int [], int, int);
static void case_cmp(int, SYMBOL *, intmax_t, int);
static void parse_return_statement(SEH *);
static SYMBOL *local_addr(TREE *);
static void parse_goto_statement(void);
static void add_statement_label(void);
static void branch_to_label(int);
static bool_t is_same_label(SYMBOL *, SYMBOL *);
static void parse_assembler_statement(void);
static void parse_asm_insn(char *);
static void parse_try_statement(int, SWTCH *, int);
static void parse_leave_statement(SEH *);

/****************************************************************************
 *                                                                          *
 * Function: new_code                                                       *
 *                                                                          *
 * Purpose : Add a pseudo code to the queue.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

CODE *new_code(int kind)
{
    CODE *cp;

    if (!reachable_code(kind))
        apperror(RCWARNING1(ERROR_UNREACHABLE_CODE));

    cp = memalloc(sizeof(*cp), funca);
    memset(cp, 0, sizeof(*cp));

    cp->kind = kind;
    cp->prev = codelist;
    cp->next = NULL;
    codelist->next = cp;
    codelist = cp;

    return cp;
}

/****************************************************************************
 *                                                                          *
 * Function: reachable_code                                                 *
 *                                                                          *
 * Purpose : Return true if the current queue position can be reached.      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

bool_t reachable_code(int kind)
{
    if (kind > CODE_START)
    {
        CODE *cp;

        for (cp = codelist; cp->kind < CODE_LABEL; cp = cp->prev)
            ;

        if (cp->kind == CODE_JUMP || cp->kind == CODE_SWITCH)
            return FALSE;
    }

    return TRUE;
}

/****************************************************************************
 *                                                                          *
 * Function: new_local_var                                                  *
 *                                                                          *
 * Purpose : Add a local variable to the code queue.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void new_local_var(SYMBOL *sym)
{
    if (!sym->defined)
    {
        new_code(CODE_LOCAL)->u.var = sym;
        sym->defined = TRUE;
        sym->scope = scope;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: new_execution_point                                            *
 *                                                                          *
 * Purpose : Add a execution point to the code queue.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void new_execution_point(COORDINATE *p)
{
    CODE *cp;

    cp = new_code(CODE_DEFPOINT);
    cp->u.point.src = (p != NULL) ? *p : src;
#ifdef PROF
    cp->u.point.point = npoints;
#endif

#ifdef PROF
    /* Get count from profiler */
    if (ncalled > 0)
    {
        int n = findcount(cp->u.point.src.file, cp->u.point.src.x, cp->u.point.src.y);
        if (n > 0) refinc = (float)n / ncalled;
    }
#endif

#ifdef XREF
    if (options.xreflevel > 1)
        add_locus(identifiers, &cp->u.point.src);
#endif

#ifdef PROF
    if (events.points && reachable_code(CODE_GEN))
    {
        TREE *e = NULL;

        apply(events.points, &cp->u.point.src, &e);
        if (e != NULL) list_nodes(e, 0, 0);
    }
#endif
}

/****************************************************************************
 *                                                                          *
 * Function: parse_statement                                                *
 *                                                                          *
 * Purpose : Parse statements.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-11-27  Bugfix: allow declarations after label (C99).        *
 *                                                                          *
 ****************************************************************************/

void parse_statement(int loop, SWTCH *swp, SEH *seh, int lev)
{
    float ref = refinc;

    if (lev == 127)  /* old limit 15 */
        apperror(RCWARNING2(ERROR_MORE_THAN_X_NESTED_STMT), 127);

    switch (tok)
    {
        case IF:
            parse_if_statement(make_label(2), loop, swp, seh, lev + 1);
            break;

        case WHILE:
            parse_while_statement(make_label(3), swp, seh, lev + 1);
            break;

        case DO:
            parse_do_statement(make_label(3), swp, seh, lev + 1);
            break;

        case FOR:
            parse_for_statement(make_label(4), swp, seh, lev + 1);
            break;

        case BREAK:
            parse_break_statement(loop, swp);
            break;

        case CONTINUE:
            parse_continue_statement(loop);
            break;

        case SWITCH:
            parse_switch_statement(make_label(2), loop, seh, lev + 1);
            break;

        case CASE:
            parse_case_statement(make_label(1), swp);
            parse_statement(loop, swp, seh, lev);
            break;

        case DEFAULT:
            parse_default_statement(swp);
            parse_statement(loop, swp, seh, lev);
            break;

        case RETURN:
            parse_return_statement(seh);
            break;

        case '{':
            parse_compound_statement(loop, swp, seh, lev + 1);
            break;

        case ';':
            new_execution_point(NULL);
            tok = gettok();
            break;

        case GOTO:
            parse_goto_statement();
            break;

        case ASM:  /* inline assembler extension */
            parse_assembler_statement();
            break;

        case TRY:  /* Microsoft extension */
            parse_try_statement(loop, swp, lev + 1);
            break;

        case LEAVE:  /* Microsoft extension */
            parse_leave_statement(seh);
            break;

        case ID:
            if (getchr() == ':')
            {
                add_statement_label();
                if (kind[tok] == IF || kind[tok] == ID)  /* added 04-11-27 */
                    parse_statement(loop, swp, seh, lev);
                break;
            }
            /* fall through */

        default:
            new_execution_point(NULL);
            if (kind[tok] != ID)
            {
                apperror(RCERROR(ERROR_UNRECOGNIZED_STMT));
                tok = gettok();
            }
            else
            {
                TREE *e = expr0(0);

                list_nodes(e, 0, 0);

                if (nodecount == 0 || nodecount > 200 || options.dbglevel > 0)
                    new_forest(NULL, 0, 0);

                memfree(STMT);
            }
            expect(';');
            break;
    }

    if (kind[tok] != CHAR_ && kind[tok] != STATIC &&
        kind[tok] != IF && kind[tok] != ID && tok != '}' && tok != EOI)
    {
        static char stop[] = { CHAR_, STATIC, IF, ID, '}', 0 };
        apperror(RCERROR(ERROR_ILLEGAL_STMT_TERMINATION));
        skipto(0, stop);
    }

    refinc = ref;
}

/****************************************************************************
 *                                                                          *
 * Function: parse_if_statement                                             *
 *                                                                          *
 * Purpose : if ( expression ) statement [ else statement ]                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void parse_if_statement(int lab, int loop, SWTCH *swp, SEH *seh, int lev)
{
    tok = gettok();
    expect('(');

    new_execution_point(NULL);
    new_forest(condexpr(')'), 0, lab);

    refinc /= 2.0;
    parse_statement(loop, swp, seh, lev);

    if (tok == ELSE)
    {
        tok = gettok();

        branch_to_label(lab+1);
        new_label(lab);
        parse_statement(loop, swp, seh, lev);

        if (find_label(lab+1)->ref)
            new_label(lab+1);
    }
    else
    {
        new_label(lab);
    }
}

/****************************************************************************
 *                                                                          *
 * Function: parse_while_statement                                          *
 *                                                                          *
 * Purpose : while ( expression ) statement                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void parse_while_statement(int lab, SWTCH *swp, SEH *seh, int lev)
{
    COORDINATE pt;
    TREE *e;

    refinc *= 10.0;

    tok = gettok();
    expect('(');

    new_forest(NULL, 0, 0);

    pt = src;

    e = expr_in_arena(condexpr, ')', funca);

    branch_to_label(lab+1);
    new_label(lab);
    parse_statement(lab, swp, seh, lev);
    new_label(lab+1);

    new_execution_point(&pt);

    new_forest(e, lab, 0);

    if (find_label(lab+2)->ref)
        new_label(lab+2);
}

/****************************************************************************
 *                                                                          *
 * Function: parse_do_statement                                             *
 *                                                                          *
 * Purpose : do statement while ( expression )                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void parse_do_statement(int lab, SWTCH *swp, SEH *seh, int lev)
{
    refinc *= 10.0;

    tok = gettok();

    new_label(lab);
    parse_statement(lab, swp, seh, lev);
    new_label(lab+1);

    expect(WHILE);
    expect('(');

    new_execution_point(NULL);

    new_forest(condexpr(')'), lab, 0);

    if (find_label(lab+2)->ref)
        new_label(lab+2);

    expect(';');
}

/****************************************************************************
 *                                                                          *
 * Function: parse_for_statement                                            *
 *                                                                          *
 * Purpose : for ( [expr1] ; [expr2] ; [expr3] ) statement                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-03-21  Call to fold_for_declaration_condition() added.      *
 *                                                                          *
 ****************************************************************************/

static void parse_for_statement(int lab, SWTCH *swp, SEH *seh, int lev)
{
    TREE *e1 = NULL;
    TREE *e2 = NULL;
    TREE *e3 = NULL;
    COORDINATE pt2;
    COORDINATE pt3;
    int once = 0;
    CODE *cp = NULL;

    tok = gettok();
    expect('(');

    new_execution_point(NULL);

    if (kind[tok] == CHAR_ || kind[tok] == STATIC || istypename(tok, toksym))
        cp = parse_for_declaration();  /* open new style for scope */
    else if (kind[tok] == ID)
        e1 = expr_in_arena(expr0, ';', funca);
    else
        expect(';');

    new_forest(e1, 0, 0);

    pt2 = src;

    refinc *= 10.0;

    if (kind[tok] == ID)
        e2 = expr_in_arena(condexpr, ';', funca);
    else
        expect(';');

    pt3 = src;

    if (kind[tok] == ID)
    {
        e3 = expr_in_arena(expr0, ')', funca);
    }
    else
    {
        static char stop[] = { IF, ID, '}', 0 };
        follow(')', stop);
    }

    if (e2)
    {
        once = cp ? fold_for_declaration_condition(cp, e2) : fold_for_condition(e1, e2);
        if (!once) branch_to_label(lab+3);
    }

    new_label(lab);
    parse_statement(lab, swp, seh, lev);
    new_label(lab+1);

    new_execution_point(&pt3);

    if (e3)
        new_forest(e3, 0, 0);

    if (e2)
    {
        if (!once) new_label(lab+3);
        new_execution_point(&pt2);
        new_forest(e2, lab, 0);
    }
    else
    {
        new_execution_point(&pt2);
        branch_to_label(lab);
    }

    if (cp) close_for_scope(cp);

    if (find_label(lab+2)->ref)
        new_label(lab+2);
}

/****************************************************************************
 *                                                                          *
 * Function: fold_for_declaration_condition                                 *
 *                                                                          *
 * Purpose : Check if initial test in for(ty e1;e2;e3) S is necessary.      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-03-21  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int fold_for_declaration_condition(CODE *cp, TREE *e2)
{
    NODE *q = NULL;
    SYMBOL *sym;
    int op;

    if (e2 == NULL)
        return 0;

    for (; cp != NULL; cp = cp->next)
    {
        switch (cp->kind)
        {
            case CODE_BLOCKBEG:
            case CODE_DEFPOINT:
                continue;

            case CODE_GEN:
            {
                NODE *p;

                for (p = cp->u.forest; p != NULL; p = p->link)
                    if (q) return 0; else q = p;
                continue;
            }

            default:
                return 0;
        }
    }

    if (q && generic(q->op) == ASGN && isaddrop(q->kids[0]->op) &&
        generic(q->kids[1]->op) == CNST)
    {
        sym = q->kids[0]->syms[0];
        q = q->kids[1];
    }
    else
    {
        return 0;
    }

    op = generic(e2->op);
    if ((op == LE || op == LT || op == EQ || op == NE || op == GT || op == GE) &&
        generic(e2->kids[0]->op) == INDIR &&
        e2->kids[0]->kids[0]->u.sym == sym && e2->kids[1]->op == specific(q->op))
    {
        TREE *e = new_tree(specific(q->op), btot(q->op, opsize(q->op)), NULL, NULL);
        e->u.v = q->syms[0]->u.c.v;
        e = simplify(/*op*/e2->op, e2->type, e, e2->kids[1]);
        if (e->op == CNST+I) return (int)e->u.v.i;
    }

    return 0;
}

/****************************************************************************
 *                                                                          *
 * Function: fold_for_condition                                             *
 *                                                                          *
 * Purpose : Check if initial test in for(e1;e2;e3) S is necessary.         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-07-19  Bugfix: use e2->op (not op) in simplify() call.      *
 *                                                                          *
 ****************************************************************************/

static int fold_for_condition(TREE *e1, TREE *e2)
{
    SYMBOL *sym;
    int op;

    if (e1 == NULL || e2 == NULL)
        return 0;

    if (generic(e1->op) == ASGN && isaddrop(e1->kids[0]->op) &&
        generic(e1->kids[1]->op) == CNST)
    {
        sym = e1->kids[0]->u.sym;
        e1 = e1->kids[1];
    }
    else
    {
        return 0;
    }

    op = generic(e2->op);
    if ((op == LE || op == LT || op == EQ || op == NE || op == GT || op == GE) &&
        generic(e2->kids[0]->op) == INDIR &&
        e2->kids[0]->kids[0]->u.sym == sym && e2->kids[1]->op == e1->op)
    {
        e1 = simplify(/*op*/e2->op, e2->type, e1, e2->kids[1]);
        if (e1->op == CNST+I) return (int)e1->u.v.i;
    }

    return 0;
}

/****************************************************************************
 *                                                                          *
 * Function: parse_break_statement                                          *
 *                                                                          *
 * Purpose : break statement.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void parse_break_statement(int loop, SWTCH *swp)
{
    new_forest(NULL, 0, 0);

    new_execution_point(NULL);

    if (swp != NULL && swp->lab > loop)
        branch_to_label(swp->lab+1);
    else if (loop)
        branch_to_label(loop+2);
    else
        apperror(RCERROR(ERROR_ILLEGAL_BREAK_STMT));

    tok = gettok();
    expect(';');
}

/****************************************************************************
 *                                                                          *
 * Function: parse_continue_statement                                       *
 *                                                                          *
 * Purpose : continue statement.                                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void parse_continue_statement(int loop)
{
    new_forest(NULL, 0, 0);

    new_execution_point(NULL);

    if (loop)
        branch_to_label(loop+1);
    else
        apperror(RCERROR(ERROR_ILLEGAL_CONTINUE_STMT));

    tok = gettok();
    expect(';');
}

/****************************************************************************
 *                                                                          *
 * Function: parse_switch_statement                                         *
 *                                                                          *
 * Purpose : switch ( expression ) statement                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void parse_switch_statement(int lab, int loop, SEH *seh, int lev)
{
    TREE *e;
    SWTCH sw;
    CODE *head;
    CODE *tail;

    tok = gettok();
    expect('(');

    new_execution_point(NULL);

    e = expr(')');
    if (!isint(e->type))
    {
        apperror(RCERROR(ERROR_ILLEGAL_TYPE_IN_SWITCH), e->type);
        e = retype(e, inttype);
    }

    e = cast(e, promote_type(e->type));
    if (generic(e->op) == INDIR && isaddrop(e->kids[0]->op) &&
        e->kids[0]->u.sym->type == e->type && !isvolatile(e->kids[0]->u.sym->type))
    {
        sw.sym = e->kids[0]->u.sym;
        new_forest(NULL, 0, 0);
    }
    else
    {
        sw.sym = make_ident(REGISTER, e->type, scope);
        new_local_var(sw.sym);
        new_forest(assignment(sw.sym, e), 0, 0);
    }

    head = new_code(CODE_SWITCH);

    sw.lab = lab;
    sw.deflab = NULL;
    sw.ncases = 0;
    sw.size = SWSIZE;
    sw.values = memarray(SWSIZE, sizeof(*sw.values), funca);
    sw.labels = memarray(SWSIZE, sizeof(*sw.labels), funca);
    refinc /= 10.0;

    parse_statement(loop, &sw, seh, lev);

    if (sw.deflab == NULL)
    {
        sw.deflab = find_label(lab);
        new_label(lab);

        if (sw.ncases == 0)
            apperror(RCWARNING1(ERROR_NO_SWITCH_CASES));
    }

    if (find_label(lab+1)->ref)
        new_label(lab+1);

    tail = codelist;
    codelist = head->prev;
    codelist->next = head->prev = NULL;

    if (sw.ncases > 0)
        generate_switch_table(&sw);

    branch_to_label(lab);

    assert(head->next);
    head->next->prev = codelist;
    codelist->next = head->next;
    codelist = tail;
}

/****************************************************************************
 *                                                                          *
 * Function: parse_case_statement                                           *
 *                                                                          *
 * Purpose : case constant-expression :                                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void parse_case_statement(int lab, SWTCH *swp)
{
    if (swp == NULL)
        apperror(RCERROR(ERROR_ILLEGAL_CASE_LABEL));

    new_label(lab);

    while (tok == CASE)
    {
        TREE *e;

        tok = gettok();
        e = constexpr(0);

        if (generic(e->op) == CNST && isint(e->type))
        {
            if (swp)
            {
                need_const++;
                e = cast(e, swp->sym->type);
                if (e->type->op == UNSIGNED)
                    e->u.v.i = extend(e->u.v.u, e->type);
                need_const--;

                add_case_label(swp, e->u.v.i, lab);
            }
        }
        else
        {
            apperror(RCERROR(ERROR_CASE_LABEL_NOT_INTCONST));
        }

        /**/
        {
            static char stop[] = { IF, ID, 0 };
            follow(':', stop);
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: parse_default_statement                                        *
 *                                                                          *
 * Purpose : default statement.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void parse_default_statement(SWTCH *swp)
{
    if (swp == NULL)
        apperror(RCERROR(ERROR_ILLEGAL_DEFAULT_LABEL));
    else if (swp->deflab)
        apperror(RCERROR(ERROR_EXTRA_DEFAULT_LABEL));
    else
    {
        swp->deflab = find_label(swp->lab);
        new_label(swp->deflab->u.lab.label);
    }

    tok = gettok();
    expect(':');
}

/****************************************************************************
 *                                                                          *
 * Function: add_case_label                                                 *
 *                                                                          *
 * Purpose : Add a label to the current switch list.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void add_case_label(SWTCH *swp, intmax_t val, int lab)
{
    int k;

    if (swp->ncases >= swp->size)
    {
        intmax_t *vals = swp->values;
        SYMBOL **labs = swp->labels;

        swp->size *= 2;
        swp->values = memarray(swp->size, sizeof(*swp->values), funca);
        swp->labels = memarray(swp->size, sizeof(*swp->labels), funca);

        for (k = 0; k < swp->ncases; k++)
        {
            swp->values[k] = vals[k];
            swp->labels[k] = labs[k];
        }
    }

    for (k = swp->ncases; k > 0 && swp->values[k-1] >= val; k--)
    {
        swp->values[k] = swp->values[k-1];
        swp->labels[k] = swp->labels[k-1];
    }

    if (k < swp->ncases && swp->values[k] == val)
        apperror(RCERROR(ERROR_DUPLICATE_CASE_LABEL), val);

    swp->values[k] = val;
    swp->labels[k] = find_label(lab);
    ++swp->ncases;

    if (swp->ncases == 1024)  /* old limit 258 */
        apperror(RCWARNING2(ERROR_MORE_THAN_X_SWITCH_CASES), 1023);
}

/****************************************************************************
 *                                                                          *
 * Function: generate_switch_table                                          *
 *                                                                          *
 * Purpose : Partition case labels into buckets, initiate code generation.  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#define den(i,j)  ((j-buckets[i]+1.0)/(v[j]-v[buckets[i]]+1))

static void generate_switch_table(SWTCH *swp)
{
    intmax_t *v = swp->values;
    int *buckets, k, n;

    buckets = memarray(swp->ncases + 1, sizeof(*buckets), funca);

    for (n = k = 0; k < swp->ncases; k++, n++)
    {
        buckets[n] = k;

        while (n > 0 && den(n - 1, k) >= density)
            n--;
    }

    buckets[n] = swp->ncases;
    generate_switch_code(swp, buckets, 0, n - 1);
}

/****************************************************************************
 *                                                                          *
 * Function: generate_switch_code                                           *
 *                                                                          *
 * Purpose : Generate switch decision code for buckets b[lb..ub].           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void generate_switch_code(SWTCH *swp, int b[], int lb, int ub)
{
    intmax_t *v = swp->values;
    int k = (lb + ub)/2;
    int hilab;
    int lolab;
    int l;
    int u;

    if (k > lb && k < ub)
    {
        lolab = make_label(1);
        hilab = make_label(1);
    }
    else if (k > lb)
    {
        lolab = make_label(1);
        hilab = swp->deflab->u.lab.label;
    }
    else if (k < ub)
    {
        lolab = swp->deflab->u.lab.label;
        hilab = make_label(1);
    }
    else
    {
        lolab = hilab = swp->deflab->u.lab.label;
    }

    l = b[k];
    u = b[k+1] - 1;

    if (u - l + 1 <= 3)
    {
        int i;

        for (i = l; i <= u; i++)
            case_cmp(EQ, swp->sym, v[i], swp->labels[i]->u.lab.label);

        if (k > lb && k < ub)
            case_cmp(GT, swp->sym, v[u], hilab);
        else if (k > lb)
            case_cmp(GT, swp->sym, v[u], hilab);
        else if (k < ub)
            case_cmp(LT, swp->sym, v[l], lolab);
        else
            assert(lolab == hilab),

        branch_to_label(lolab);
        new_forest(NULL, 0, 0);
    }
    else
    {
        TYPE *ty = signedint_type(swp->sym->type);
        SYMBOL *table = make_ident(STATIC, new_array(voidptype, u - l + 1, 0), GLOBAL);
        TREE *e;

        (*IR->defsymbol)(table);

        if (!isunsigned(swp->sym->type) || v[l] != 0)
            case_cmp(LT, swp->sym, v[l], lolab);

        case_cmp(GT, swp->sym, v[u], hilab);

        e = (*optree['-'])(SUB, cast(id_tree(swp->sym), ty), cnst_tree(ty, (intmax_t)v[l]));
        if (e->type->size < unsignedptrtype->size)
            e = cast(e, unsignedlongtype);

        new_forest(new_tree(JUMP, voidtype,
            rvalue((*optree['+'])(ADD, pointer(id_tree(table)), e)), NULL),
            0, 0);

        new_code(CODE_SWITCH);
        codelist->u.swtch.table = table;
        codelist->u.swtch.sym = swp->sym;
        codelist->u.swtch.deflab = swp->deflab;
        codelist->u.swtch.size = u - l + 1;
        codelist->u.swtch.values = &v[l];
        codelist->u.swtch.labels = &swp->labels[l];

        if ((v[u] - v[l] + 1) >= 10000)
            apperror(RCWARNING2(ERROR_HUGE_SWITCH_TABLE));
    }

    if (k > lb)
    {
        assert(lolab != swp->deflab->u.lab.label);
        new_label(lolab);
        generate_switch_code(swp, b, lb, k - 1);
    }

    if (k < ub)
    {
        assert(hilab != swp->deflab->u.lab.label);
        new_label(hilab);
        generate_switch_code(swp, b, k + 1, ub);
    }
}

/****************************************************************************
 *                                                                          *
 * Function: case_cmp                                                       *
 *                                                                          *
 * Purpose : Generate code for 'if (p op n) goto lab' for integer n.        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void case_cmp(int op, SYMBOL *sym, intmax_t n, int lab)
{
    TYPE *ty = signedint_type(sym->type);

    list_nodes(equality_tree(op, cast(id_tree(sym), ty), cnst_tree(ty, n)), lab, 0);
}

/****************************************************************************
 *                                                                          *
 * Function: parse_return_statement                                         *
 *                                                                          *
 * Purpose : return statement.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           01-01-11  SEH unwind support added.                            *
 *                                                                          *
 ****************************************************************************/

static void parse_return_statement(SEH *seh)
{
    TYPE *rty = func_return(funcsym->type);

    tok = gettok();

    new_execution_point(NULL);

    if (seh && seh->type == SEH_TRY)
        new_code(CODE_UNWIND)->u.seh = seh;

    if (tok != ';')
    {
        if (rty == voidtype)
        {
            apperror(RCERROR(ERROR_EXTRANEOUS_RETURN_VALUE));
            expr(0);
            return_value(NULL);
        }
        else
        {
            return_value(expr(0));
        }
    }
    else
    {
        if (rty != voidtype)
        {
            apperror(RCERROR(ERROR_MISSING_RETURN_VALUE));
            return_value(cnst_tree(inttype, 0));  /* 03-09-21 */
        }
        else
        {
            return_value(NULL);
        }
    }

    branch_to_label(funcsym->u.fcn.label);
    expect(';');
}

/****************************************************************************
 *                                                                          *
 * Function: return_value                                                   *
 *                                                                          *
 * Purpose : Return e from the current function.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void return_value(TREE *e)
{
    TYPE *ty;

    if (e == NULL)
    {
#ifdef PROF
        if (events.returns)
            apply(events.returns, funcsym, NULL);
#endif
        return;
    }

    e = pointer(e);

    if ((ty = check_assignment(func_return(funcsym->type), e)) == NULL)
    {
        apperror(RCERROR(ERROR_ILLEGAL_RETURN_TYPE2), e->type, func_return(funcsym->type));
        return;
    }

    e = cast(e, ty);

    if (retv)  /* return value location for structs */
    {
        if (is_callb(e))
        {
            e = new_tree(RIGHT, e->type,
                new_tree(CALL+B, e->type,
                e->kids[0]->kids[0], id_tree(retv)),
                rvalue(id_tree(retv)));
        }
        else
        {
            TYPE *ty = retv->type->type;
            assert(isstruct(ty));
            if (ty->u.sym->u.s.cfields)
            {
                ty->u.sym->u.s.cfields = FALSE;
                e = assignment_tree(ASGN, rvalue(id_tree(retv)), e);
                ty->u.sym->u.s.cfields = TRUE;
            }
            else
            {
                e = assignment_tree(ASGN, rvalue(id_tree(retv)), e);
            }
        }

        new_forest(e, 0, 0);

#ifdef PROF
        if (events.returns)
            apply(events.returns, funcsym, rvalue(id_tree(retv)));
#endif
        return;
    }

#ifdef PROF
    if (events.returns)
    {
        SYMBOL *sym = make_ident(AUTO, e->type, scope);

        new_local_var(sym);
        new_forest(assignment(sym, e), 0, 0);
        apply(events.returns, funcsym, id_tree(sym));
        e = id_tree(sym);
    }
#endif

    if (!isfloat(e->type))
        e = cast(e, promote_type(e->type));

    if (isptr(e->type))
    {
        SYMBOL *sym = local_addr(e);

        if (sym && (sym->computed || sym->generated))
            apperror(sym->scope == PARAM ?
                RCWARNING1(ERROR_ILLEGAL_RETURN_VALUE_P2) :
                RCWARNING1(ERROR_ILLEGAL_RETURN_VALUE_L2));
        else if (sym)
            apperror(sym->scope == PARAM ?
                RCWARNING1(ERROR_ILLEGAL_RETURN_VALUE_P1) :
                RCWARNING1(ERROR_ILLEGAL_RETURN_VALUE_L1),
                sym->name);
    }

    new_forest(new_tree(mkop(RET,e->type), e->type, e, NULL), 0, 0);
}

/****************************************************************************
 *                                                                          *
 * Function: local_addr                                                     *
 *                                                                          *
 * Purpose : Returns q if e yields the address of local/parameter q;        *
 *           otherwise returns NULL.                                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-07-15  Added operators INTRIN1S, INTRIN2S.                  *
 *                                                                          *
 ****************************************************************************/

static SYMBOL *local_addr(TREE *e)
{
    if (e == NULL)
        return NULL;

    switch (generic(e->op))
    {
        case INDIR:
        case CALL:
        case ARG:
        case INTRIN1S:
        case INTRIN2S:
            return NULL;

        case ADDRL:
        case ADDRF:
            return e->u.sym;

        case RIGHT:
        case ASGN:
            if (e->kids[1])
                return local_addr(e->kids[1]);
            return local_addr(e->kids[0]);

        case COND:
        {
            SYMBOL *sym;
            assert(e->kids[1] && e->kids[1]->op == RIGHT);
            if ((sym = local_addr(e->kids[1]->kids[0])) != NULL)
                return sym;
            return local_addr(e->kids[1]->kids[1]);
        }

        default:
        {
            SYMBOL *sym;
            if (e->kids[0] && (sym = local_addr(e->kids[0])) != NULL)
                return sym;
            return local_addr(e->kids[1]);
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: parse_goto_statement                                           *
 *                                                                          *
 * Purpose : goto identifier ;                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-11-27  Remember count of seen VLA's.                        *
 *                                                                          *
 ****************************************************************************/

static void parse_goto_statement(void)
{
    new_forest(NULL, 0, 0);

    new_execution_point(NULL);

    tok = gettok();
    if (tok == ID)
    {
        SYMBOL *sym;

        sym = lookup_symbol(tokstr, stmtlabs);
        if (sym == NULL)
        {
            sym = install_symbol(tokstr, &stmtlabs, 0, funca);
            sym->scope = LABELS;
            sym->u.lab.label = make_label(1);
            sym->u.lab.funcvla = funcvla;
            sym->src = src;
        }

#ifdef XREF
        if (options.xreflevel > 1)
            use_symbol(sym, src);
#endif
        branch_to_label(sym->u.lab.label);

        tok = gettok();
    }
    else
    {
        apperror(RCERROR(ERROR_MISSING_LABEL_IN_GOTO));
    }

    expect(';');
}

/****************************************************************************
 *                                                                          *
 * Function: add_statement_label                                            *
 *                                                                          *
 * Purpose : Handle label :                                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-11-27  Verify count of seen VLA's.                          *
 *                                                                          *
 ****************************************************************************/

static void add_statement_label(void)
{
    SYMBOL *sym;

    sym = lookup_symbol(tokstr, stmtlabs);
    if (sym == NULL)
    {
        sym = install_symbol(tokstr, &stmtlabs, 0, funca);
        sym->scope = LABELS;
        sym->u.lab.label = make_label(1);
        sym->src = src;
    }
    else if (sym->u.lab.funcvla != funcvla)
    {
        /* check forward jumps */
        COORDINATE pt = src; src = sym->src;
        apperror(RCERROR(ERROR_INVALID_JUMP_PAST_VLA));
        src = pt;
    }

    if (sym->defined)
        apperror(RCERROR(ERROR_REDEFINITION_OF_LABEL), sym->name, &sym->src);

    sym->defined = TRUE;
    new_label(sym->u.lab.label);

    tok = gettok();
    expect(':');
}

/****************************************************************************
 *                                                                          *
 * Function: new_label                                                      *
 *                                                                          *
 * Purpose : Add a label to the code queue.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void new_label(int lab)
{
    SYMBOL *sym = find_label(lab);
    CODE *cp;

    assert(lab);

    new_forest(NULL, 0, 0);

    new_code(CODE_LABEL)->u.forest = new_node(LABEL+V, NULL, NULL, sym);

    for (cp = codelist->prev; cp->kind <= CODE_LABEL; cp = cp->prev)
        ;

    while (cp->kind == CODE_JUMP &&
        cp->u.forest->kids[0] &&
        specific(cp->u.forest->kids[0]->op) == ADDRG+P &&
        cp->u.forest->kids[0]->syms[0] == sym)
    {
        assert(cp->u.forest->kids[0]->syms[0]->u.lab.label == lab);

        sym->ref--;

        assert(cp->next);
        assert(cp->prev);

        cp->prev->next = cp->next;
        cp->next->prev = cp->prev;
        cp = cp->prev;

        while (cp->kind <= CODE_LABEL)
            cp = cp->prev;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: branch_to_label                                                *
 *                                                                          *
 * Purpose : Jump to lab.                                                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void branch_to_label(int lab)
{
    SYMBOL *sym = find_label(lab);
    CODE *cp;

    assert(lab);

    new_forest(NULL, 0, 0);

    new_code(CODE_LABEL)->u.forest = jump(lab);

    for (cp = codelist->prev; cp->kind < CODE_LABEL; cp = cp->prev)
        ;

    while (cp->kind == CODE_LABEL && cp->u.forest->op == LABEL+V &&
        !is_same_label(cp->u.forest->syms[0], sym))
    {
        equate_labels(cp->u.forest->syms[0], sym);

        assert(cp->next);
        assert(cp->prev);

        cp->prev->next = cp->next;
        cp->next->prev = cp->prev;
        cp = cp->prev;

        while (cp->kind < CODE_LABEL)
            cp = cp->prev;
    }

    if (cp->kind == CODE_JUMP || cp->kind == CODE_SWITCH)
    {
        sym->ref--;

        codelist->prev->next = NULL;
        codelist = codelist->prev;
    }
    else
    {
        codelist->kind = CODE_JUMP;

        if (cp->kind == CODE_LABEL && cp->u.forest->op == LABEL+V &&
            is_same_label(cp->u.forest->syms[0], sym))
        {
            apperror(RCWARNING1(ERROR_INFINITE_LOOP));
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: equate_labels                                                  *
 *                                                                          *
 * Purpose : Refer one label symbol to another.                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void equate_labels(SYMBOL *old, SYMBOL *new)
{
    assert(old->u.lab.equatedto == NULL);

    old->u.lab.equatedto = new;
    new->ref++;
}

/****************************************************************************
 *                                                                          *
 * Function: is_same_label                                                  *
 *                                                                          *
 * Purpose : Does the two symbols refer to the same label?                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static bool_t is_same_label(SYMBOL *sym, SYMBOL *dst)
{
    assert(dst && sym);

    for ( ; dst != NULL; dst = dst->u.lab.equatedto)
        if (sym == dst) return TRUE;

    return FALSE;
}

/****************************************************************************
 *                                                                          *
 * Function: parse_assembler_statement                                      *
 *                                                                          *
 * Purpose : __asm { assembly code }                                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           03-09-01  Added inline assembler flag.                         *
 *                                                                          *
 ****************************************************************************/

static void parse_assembler_statement(void)
{
    apperror(RCWARNING2(ERROR_NON_PORTABLE_ASSEMBLY));

    new_forest(NULL, 0, 0);

    new_execution_point(NULL);

    if (getchr() == '{')
    {
        gettok();
        do
        {
            parse_asm_insn(getinp());
        } while (getchr() != '}');
        gettok();
    }
    else
    {
        parse_asm_insn(getinp());
    }

    tok = gettok();

    funcsym->assembler = TRUE;
}

/****************************************************************************
 *                                                                          *
 * Function: parse_asm_insn                                                 *
 *                                                                          *
 * Purpose : Parse a single inline assembler instruction.                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-11-20  Added warn flag - just in case.                      *
 *                                                                          *
 ****************************************************************************/

static void parse_asm_insn(char *s)
{
    static int warn = 0;
    char *name;
    SYMBOL *sym;
    CODE *cp;

    if (OF == NULL)
    {
        /* no output format means no specific target - we are probably just checking syntax */
        if (warn++ == 0)
            apperror(RCWARNING1(ERROR_NON_PARSEABLE_ASSEMBLY));
        return;
    }

    if (!as.parser.parseinline)
    {
        /* no parser - the target does not support inline assembly */
        apperror(RCERROR(ERROR_ASSEMBLY_NOT_SUPPORTED));
        return;
    }

    cp = new_code(CODE_ASM);

    as.parser.parseinline(s, &name, &cp->u.asm.text, &cp->u.asm.sym, &cp->u.asm.cl);

    sym = NULL;
    if (name != NULL)
    {
        sym = lookup_symbol(name, stmtlabs);
        if (sym == NULL)
        {
            sym = install_symbol(name, &stmtlabs, 0, funca);
            sym->scope = LABELS;
            sym->u.lab.label = make_label(1);
            sym->src = src;
        }

        if (sym->defined)
            apperror(RCERROR(ERROR_REDEFINITION_OF_LABEL), sym->name, &sym->src);

        sym->defined = TRUE;
        sym = find_label(sym->u.lab.label);
    }

    cp->u.asm.lab = sym;
    cp->u.asm.src = src;
}

/****************************************************************************
 *                                                                          *
 * Function: parse_try_statement                                            *
 *                                                                          *
 * Purpose : __try { statement* } ( __except(expression) { statement* }     *
 *                                | __finally { statement* } )              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           02-04-24  Bugfix added (see below).                            *
 *                                                                          *
 ****************************************************************************/

static void parse_try_statement(int loop, SWTCH *swp, int lev)
{
    static int level = 0;
    SEH *seh;

    if (options.microsoft)
        apperror(RCWARNING2(ERROR_NON_PORTABLE_SEH));
    else
        apperror(RCERROR(ERROR_SEH_NOT_SUPPORTED));

    if (funcsym->attr.naked)
        apperror(RCERROR(ERROR_ILLEGAL_WITH_NAKED_ATTRIB), TRY);

    ++level;

    seh = memalloc(sizeof(*seh), funca);
    seh->index = listelems(funcsym->u.fcn.sehlist);
    seh->previndex = (level) ? seh->index-1 : -1;
    seh->level = lev;
    seh->label = make_label(1);
    seh->sym = NULL;
    funcsym->u.fcn.sehlist = listappend(seh, funcsym->u.fcn.sehlist);

    tok = gettok();

    seh->type = SEH_TRY;
    parse_compound_statement(loop, swp, seh, lev);

    if (tok == EXCEPT)
    {
        TREE *e;

        expect(EXCEPT);
        expect('(');

        sehinfo.acceptinfo = TRUE;
        sehinfo.acceptcode = TRUE;

        e = expr_in_arena(expr, ')', funca);

        if (IR->sehbeg)
        {
            e = cast(e, inttype);
            new_forest(new_tree(mkop(RET,e->type), e->type, e, NULL), 0, 0);
        }

        sehinfo.acceptinfo = FALSE;

        seh->type = SEH_EXCEPT;
        parse_compound_statement(loop, swp, seh, lev);

        sehinfo.acceptcode = FALSE;
    }
    else
    {
        expect(FINALLY);

        /*
         * Bugfix 02-04-24:
         * Need dummy code to separate the compound statements, so that
         * jumps in the try-block are not equated into the finally-block.
         * We acomplish this by setting the CODE_UNWIND argument u.seh
         * to NULL, which -in the end- does nothing.
         * There are probably a much cleaner way of handling this, but
         * this solves the immediate problem!
         */
        new_code(CODE_UNWIND)->u.seh = NULL;

        seh->type = SEH_FINALLY;
        parse_compound_statement(loop, swp, seh, lev);
    }

    --level;
}

/****************************************************************************
 *                                                                          *
 * Function: parse_leave_statement                                          *
 *                                                                          *
 * Purpose : __leave statement.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void parse_leave_statement(SEH *seh)
{
    if (options.microsoft)
        apperror(RCWARNING2(ERROR_NON_PORTABLE_SEH));
    else
        apperror(RCERROR(ERROR_SEH_NOT_SUPPORTED));

    new_forest(NULL, 0, 0);

    new_execution_point(NULL);

    if (seh != NULL)
        branch_to_label(seh->label);
    else
        apperror(RCERROR(ERROR_ILLEGAL_LEAVE_STMT));

    tok = gettok();
    expect(';');
}

