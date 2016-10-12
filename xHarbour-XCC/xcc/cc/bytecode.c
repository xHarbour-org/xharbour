/****************************************************************************
 *                                                                          *
 * File    : bytecode.c                                                     *
 *                                                                          *
 * Purpose : ISO C Compiler; Coca-Cola bytecode.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "lcc.h"

#define I(f) b_##f

static void I(segment)(int n)
{
    static int cseg;

    if (cseg != n)
    {
        switch (cseg = n)
        {
            case TEXT: printf(".code\n"); return;
            case DATA: printf(".data\n"); return;
            case BSS:  printf(".bss\n");  return;
            case LIT:  printf(".const\n");  return;
            default: assert(0);
        }
    }
}

static void I(address)(SYMBOL *q, SYMBOL *p, long n)
{
    q->x.name = stringf("%s%s%D", p->x.name, n > 0 ? "+" : "", (intmax_t)n);
}

static void I(defaddress)(SYMBOL *p)
{
    printf("I(defaddress): address %s\n", p->x.name);
}

static void I(defconst)(int suffix, int size, VALUE v)
{
    printf("I(defconst): ");
    switch (suffix)
    {
        case I:
            if (size > sizeof(int))
                printf("byte x %d: %D\n", size, v.i);
            else
                printf("byte x %d: %d\n", size, v.i);
            return;

        case U:
            if (size > sizeof(uint_t))
                printf("byte x %d: %U\n", size, v.u);
            else
                printf("byte x %d: %u\n", size, v.u);
            return;

        case P:
            printf("byte x %d: %U\n", size, v.p);
            return;

        case F:
            if (size == 4)
            {
                float f = (float)v.d;
                printf("byte x 4: %u\n", *(uint_t *)&f);
            }
            else
            {
                uint_t *p = (uint_t *)&v.d;
                printf("byte x 4: %u\n", p[swap]);
                printf("byte x 4: %u\n", p[1-swap]);
            }
            return;
    }
    assert(0);
}

static void I(defstring)(int len, char *str)
{
    char *s;

    printf("I(defstring):\n");
    for (s = str; s < str + len; s++)
        printf("byte 1 %d\n", (*s)&0377);
}

static void I(defsymbol)(SYMBOL *p)
{
    if (p->scope == CONSTANTS)
    {
        switch (optype(ttob(p->type)))
        {
            case I: p->x.name = stringf("%D", p->u.c.v.i); break;
            case U: p->x.name = stringf("%U", p->u.c.v.u); break;
            case P: p->x.name = stringf("%U", p->u.c.v.p); break;
            default: assert(0);
        }
    }
    else if (p->scope >= LOCAL && p->sclass == STATIC)
    {
        p->x.name = stringf("@%d", make_label(1));
    }
    else if (p->scope == LABELS || p->generated)
    {
        p->x.name = stringf("@%s", p->name);
    }
    else
    {
        p->x.name = p->name;
    }
}

static void dumptree(NODE *p)
{
    switch (specific(p->op))
    {
        case ASGN+B:
            assert(p->kids[0]);
            assert(p->kids[1]);
            assert(p->syms[0]);
            dumptree(p->kids[0]);
            dumptree(p->kids[1]);
            printf("%s(%d)\n", opname(p->op), p->syms[0]->u.c.v.u);
            return;

        case RET+V:
            assert(!p->kids[0]);
            assert(!p->kids[1]);
            printf("%s\n", opname(p->op));
            return;
    }

    switch (generic(p->op))
    {
        case CNST:
        case ADDRG:
        case ADDRF:
        case ADDRL:
        case LABEL:
            assert(!p->kids[0]);
            assert(!p->kids[1]);
            assert(p->syms[0] && p->syms[0]->x.name);
            printf("%s(%s)\n", opname(p->op), p->syms[0]->x.name);
            return;

        case CVF:
        case CVI:
        case CVP:
        case CVU:
            assert(p->kids[0]);
            assert(!p->kids[1]);
            assert(p->syms[0]);
            dumptree(p->kids[0]);
            printf("%s(%d)\n", opname(p->op), p->syms[0]->u.c.v.i);
            return;

        case ARG:
        case BCOM:
        case NEG:
        case INDIR:
        case JUMP:
        case RET:
            assert(p->kids[0]);
            assert(!p->kids[1]);
            dumptree(p->kids[0]);
            printf("%s\n", opname(p->op));
            return;

        case CALL:
            assert(p->kids[0]);
            assert(!p->kids[1]);
            assert(optype(p->op) != B);
            dumptree(p->kids[0]);
            printf("%s\n", opname(p->op));
            return;

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
            assert(p->kids[0]);
            assert(p->kids[1]);
            dumptree(p->kids[0]);
            dumptree(p->kids[1]);
            printf("%s\n", opname(p->op));
            return;

        case EQ:
        case NE:
        case GT:
        case GE:
        case LE:
        case LT:
            assert(p->kids[0]);
            assert(p->kids[1]);
            assert(p->syms[0]);
            assert(p->syms[0]->x.name);
            dumptree(p->kids[0]);
            dumptree(p->kids[1]);
            printf("%s(%s)\n", opname(p->op), p->syms[0]->x.name);
            return;
    }
    assert(0);
}

static void I(emit)(NODE *p)
{
    printf("I(emit)\n");
    for (; p; p = p->link)
        dumptree(p);
}

static void I(export)(SYMBOL *p)
{
    printf("I(export) - %s\n", p->x.name);
}

static void I(function)(SYMBOL *f, SYMBOL *caller[], SYMBOL *callee[], int ncalls)
{
    int i;

    (*IR->segment)(TEXT);

    offset = 0;
    for (i = 0; caller[i] && callee[i]; i++)
    {
        offset = roundup(offset, caller[i]->type->align);
        caller[i]->x.name = callee[i]->x.name = stringf("%d", offset);
        caller[i]->x.offset = callee[i]->x.offset = offset;
        offset += caller[i]->type->size;
    }
    maxargoffset = maxoffset = argoffset = offset = 0;
    generate_function_code(caller, callee);
    printf("proc %s - maxoffs=%d, maxargoffs=%d\n", f->x.name, maxoffset, maxargoffset);
    emit_function_code();
    printf("endproc %s - maxoffs=%d, maxargoffs=%d\n", f->x.name, maxoffset, maxargoffset);
}

static void gen02(NODE *p)
{
    assert(p);
    if (generic(p->op) == ARG)
    {
        assert(p->syms[0]);
        argoffset += (long)(p->syms[0]->u.c.v.i < 4 ? 4 : p->syms[0]->u.c.v.i);
    }
    else if (generic(p->op) == CALL)
    {
        maxargoffset = (argoffset > maxargoffset ? argoffset : maxargoffset);
        argoffset = 0;
    }
}

static void gen01(NODE *p)
{
    if (p)
    {
        gen01(p->kids[0]);
        gen01(p->kids[1]);
        gen02(p);
    }
}

static NODE *I(gen)(NODE *p)
{
    NODE *q;

    assert(p);
    printf("I(gen)\n");
    for (q = p; q; q = q->link)
        gen01(q);
    return p;
}

static void I(global)(SYMBOL *p)
{
    printf("I(global)\n");
    printf("align %d\n", p->type->align > 4 ? 4 : p->type->align);
    printf("LABELV %s\n", p->x.name);
}

static void I(import)(SYMBOL *p)
{
    printf("I(import) - %s\n", p->x.name);
}

static void I(local)(SYMBOL *p)
{
    offset = roundup(offset, p->type->align);
    p->x.name = stringf("%d", offset);
    p->x.offset = offset;
    offset += p->type->size;
}

static void I(progbeg)(void) {}

static void I(progend)(void) {}

static void I(space)(int n)
{
    printf("I(space) %d\n", n);
}

static void I(sehbeg)(int n, SEH *seh)
{
    printf("I(sehbeg) %d\n", n);
}

static void I(sehend)(int n, SEH *seh)
{
    printf("I(sehend) %d\n", n);
}

static void I(unwind)(SEH *seh)
{
    printf("I(unwind) seh\n");
}

static int I(opcall)(int op)
{
    return 0;
}

static void I(dbgline)(COORDINATE *cp)
{
    static char *prevfile;
    static uint_t prevline;

    if (cp->file && (prevfile == NULL || strcmp(prevfile, cp->file) != 0))
    {
        printf("file \"%s\"\n", prevfile = cp->file);
        prevline = 0;
    }

    if (cp->y != prevline)
        printf("line %d\n", prevline = cp->y);
}

#define b_blockbeg blockbeg
#define b_blockend blockend

INTERFACE bytecodeIR = {
        4, 4, 0,        /* _Bool */
        1, 1, 0,        /* char */
        2, 2, 0,        /* short */
        4, 4, 0,        /* int */
        4, 4, 0,        /* long */
        4, 4, 0,        /* long long */
        4, 4, 1,        /* float */
        8, 8, 1,        /* double */
        8, 8, 1,        /* long double */
        4, 4, 0,        /* T* */
        0, 4, 0,        /* struct */
        0,              /* little_endian */
        0,              /* wants_callb */
        0,              /* wants_argb */
        0,              /* wants_optb */
        0, /*1,*/       /* left_to_right */
        0,              /* wants_dag */
        0,              /* unsigned_char */
        0,              /* wants_intrinsic */
        I(address),
        I(blockbeg),
        I(blockend),
        I(defaddress),
        I(defconst),
        I(defstring),
        I(defsymbol),
        I(emit),
        I(export),
        I(function),
        I(gen),
        I(global),
        I(import),
        I(local),
        I(progbeg),
        I(progend),
        I(segment),
        I(space),
        I(sehbeg),
        I(sehend),
        I(unwind),
        I(opcall),
        0,              /* I(dbgblock) */
        0,              /* I(dbgend) */
        0,              /* I(dbgfend) */
        0,              /* I(dbginit) */
        I(dbgline),
        0,              /* I(dbgsym) */
        0,              /* I(dbgtype) */
};
