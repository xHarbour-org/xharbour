/****************************************************************************
 *                                                                          *
 * File    : sym.c                                                          *
 *                                                                          *
 * Purpose : ISO C Compiler; Symbol table management.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop
#include <stdio.h>

#include "lcc.h"

/* symbol table entry */
typedef struct _ENTRY ENTRY;
typedef struct _ENTRY {
    SYMBOL sym;             /* the symbol */
    ENTRY *link;            /* next entry on hash chain */
} ENTRY;

/* symbol table */
struct _TABLE {
    int level;              /* scope level for this table */
    TABLE *previous;        /* table for previous scope */
    ENTRY *buckets[256];    /* hash table */
    SYMBOL *all;
};

#define HASHSIZE  NELEMS(((TABLE *)0)->buckets)

/* Locals */
static int tempid;
static TABLE cns = { CONSTANTS };
static TABLE ext = { GLOBAL };
static TABLE ids = { GLOBAL };
static TABLE tys = { GLOBAL };

/* Globals */
TABLE *constants = &cns;
TABLE *externals = &ext;
TABLE *identifiers = &ids;
TABLE *globals = &ids;
TABLE *types = &tys;
TABLE *labels;

int scope = GLOBAL;

#ifdef XREF
LIST *loci;
LIST *symbols;
#endif

/****************************************************************************
 *                                                                          *
 * Function: new_symbol_table                                               *
 *                                                                          *
 * Purpose : Create a new table with predecessor tp, scope level.           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

TABLE *new_symbol_table(TABLE *tp, int lev)
{
    TABLE *ntp;

    ntp = memalloc(sizeof(*ntp), funca);
    memset(ntp, 0, sizeof(*ntp));

    ntp->previous = tp;
    ntp->level = lev;

    if (tp) ntp->all = tp->all;

    return ntp;
}

/****************************************************************************
 *                                                                          *
 * Function: install_symbol                                                 *
 *                                                                          *
 * Purpose : Install a name in table *tpp.                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

SYMBOL *install_symbol(const char *name, TABLE **tpp, int lev, uint_t arena)
{
    uint_t h = (ulong_t)name & (HASHSIZE-1);
    TABLE *tp = *tpp;
    ENTRY *p;

    assert(lev == 0 || lev >= tp->level);

    if (lev > 0 && tp->level < lev)
        tp = *tpp = new_symbol_table(tp, lev);

    p = memalloc(sizeof(*p), arena);
    memset(p, 0, sizeof(*p));

    p->sym.name = (char *)name;
    p->sym.scope = lev;
    p->sym.up = tp->all;
    tp->all = &p->sym;
    p->link = tp->buckets[h];
    tp->buckets[h] = p;

    return &p->sym;
}

/****************************************************************************
 *                                                                          *
 * Function: relocate_symbol                                                *
 *                                                                          *
 * Purpose : Relocate a symbol from one table to another.                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

SYMBOL *relocate_symbol(const char *name, TABLE *src, TABLE *dst)
{
    uint_t h = (ulong_t)name & (HASHSIZE-1);
    ENTRY *p;
    ENTRY **q;
    SYMBOL **r;

    for (q = &src->buckets[h]; *q; q = &(*q)->link)
        if (name == (*q)->sym.name) break;
    assert(*q);

    /*
     * Remove the entry from src's hash chain
     * and from its list of all symbols.
     */
    p = *q;
    *q = (*q)->link;
    for (r = &src->all; *r && *r != &p->sym; r = &(*r)->up)
        ;
    assert(*r == &p->sym);
    *r = p->sym.up;

    /*
     * Insert the entry into dst's hash chain
     * and into its list of all symbols.
     * Return the symbol-table entry.
     */
    p->link = dst->buckets[h];
    dst->buckets[h] = p;
    p->sym.up = dst->all;
    dst->all = &p->sym;

    return &p->sym;
}

/****************************************************************************
 *                                                                          *
 * Function: lookup_symbol                                                  *
 *                                                                          *
 * Purpose : Lookup name in table tp, return pointer to entry.              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

SYMBOL *lookup_symbol(const char *name, TABLE *tp)
{
    uint_t h = (ulong_t)name & (HASHSIZE-1);
    ENTRY *p;

    assert(tp);
    do
    {
        for (p = tp->buckets[h]; p != NULL; p = p->link)
            if (name == p->sym.name) return &p->sym;
    } while ((tp = tp->previous) != NULL);

    return NULL;
}

/****************************************************************************
 *                                                                          *
 * Function: for_each_symbol                                                *
 *                                                                          *
 * Purpose : Call apply(p) for each entry p in table tp.                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void for_each_symbol(TABLE *tp, int lev, void (*apply)(SYMBOL *, void *), void *cl)
{
    assert(tp);

    while (tp && tp->level > lev)
        tp = tp->previous;

    if (tp && tp->level == lev)
    {
        COORDINATE sav;
        SYMBOL *sym;

        sav = src;

        for (sym = tp->all; sym && sym->scope == lev; sym = sym->up)
        {
            src = sym->src;
            (*apply)(sym, cl);
        }

        src = sav;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: all_symbols                                                    *
 *                                                                          *
 * Purpose : Return current tail of the list of visible symbols             *
 *           (also the most recently added symbol).                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

SYMBOL *all_symbols(TABLE *tp)
{
    return tp->all;
}

/****************************************************************************
 *                                                                          *
 * Function: enter_scope                                                    *
 *                                                                          *
 * Purpose : Enter a scope (surprise!).                                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void enter_scope(void)
{
    if (++scope == LOCAL)
        tempid = 0;
}

/****************************************************************************
 *                                                                          *
 * Function: leave_scope                                                    *
 *                                                                          *
 * Purpose : Leave a scope (surprise!)                                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void leave_scope(void)
{
    remove_types_from_scope(scope);

    if (types->level == scope)
        types = types->previous;

    if (identifiers->level == scope)
    {
        if (options.warnlevel >= 2)
        {
            SYMBOL *sym;
            int n = 0;

            for (sym = identifiers->all;
                 sym && sym->scope == scope;
                 sym = sym->up)
            {
                if (++n > 511)  /* old limit 127 */
                {
                    apperror(RCWARNING2(ERROR_MORE_THAN_X_BLOCK_IDENT), 511);
                    break;
                }
            }
        }

        identifiers = identifiers->previous;
    }

    assert(scope >= GLOBAL);
    scope--;
}

/****************************************************************************
 *                                                                          *
 * Function: make_symbol                                                    *
 *                                                                          *
 * Purpose : Produce a symbol for name.                                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-12-30  Created                                              *
 *                                                                          *
 ****************************************************************************/

SYMBOL *make_symbol(int sclass, const char *name, TYPE *ty)
{
    SYMBOL *p;

    if (sclass == EXTERN)
    {
        p = install_symbol(string(name), &globals, GLOBAL, PERM);
    }
    else
    {
        p = memalloc(sizeof(*p), PERM);
        memset(p, 0, sizeof(*p));

        p->name = string(name);
        p->scope = GLOBAL;
    }

    p->sclass = sclass;
    p->type = ty;

    (*IR->defsymbol)(p);
    p->defined = 1;

    return p;
}

/****************************************************************************
 *                                                                          *
 * Function: make_ident                                                     *
 *                                                                          *
 * Purpose : Produce a system generated identifier.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

SYMBOL *make_ident(int sclass, TYPE *ty, int lev)
{
    SYMBOL *sym;

    sym = memalloc(sizeof(*sym), (lev >= LOCAL) ? funca : PERM);
    memset(sym, 0, sizeof(*sym));

    sym->name = stringd(make_label(1));
    sym->scope = lev;
    sym->sclass = sclass;
    sym->type = ty;
    sym->generated = TRUE;

    if (lev == GLOBAL)
        (*IR->defsymbol)(sym);

    return sym;
}

/****************************************************************************
 *                                                                          *
 * Function: make_temp_ident                                                *
 *                                                                          *
 * Purpose : Produce a system generated temporary identifier.               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

SYMBOL *make_temp_ident(int sclass, TYPE *ty)
{
    SYMBOL *sym;

    sym = memalloc(sizeof(*sym), funca);
    memset(sym, 0, sizeof(*sym));

    sym->name = stringd(++tempid);
    sym->scope = (scope < LOCAL) ? LOCAL : scope;
    sym->sclass = sclass;
    sym->type = ty;
    sym->temporary = TRUE;
    sym->generated = TRUE;

    return sym;
}

/****************************************************************************
 *                                                                          *
 * Function: define_global                                                  *
 *                                                                          *
 * Purpose : Define a global or static variable.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void define_global(SYMBOL *sym, int seg)
{
    sym->u.seg = seg;
    set_segment(sym->u.seg);

    if (sym->sclass != STATIC)
        (*IR->export)(sym);

    (*IR->global)(sym);
    sym->defined = TRUE;
}

/****************************************************************************
 *                                                                          *
 * Function: define_local                                                   *
 *                                                                          *
 * Purpose : Define a local variable.                                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

SYMBOL *define_local(int sclass, int op, int size)
{
    SYMBOL *sym = make_temp_ident(sclass, btot(op, size));

    (*IR->local)(sym);
    sym->defined = TRUE;

    return sym;
}

/****************************************************************************
 *                                                                          *
 * Function: make_label                                                     *
 *                                                                          *
 * Purpose : Produce a system generated label.                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

int make_label(int n)
{
    static int label = 1;

    label += n;
    return label - n;
}

/****************************************************************************
 *                                                                          *
 * Function: find_label                                                     *
 *                                                                          *
 * Purpose : Lookup a label entry. Install a new one if not found.          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

SYMBOL *find_label(int lab)
{
    uint_t h = lab & (HASHSIZE-1);
    ENTRY *p;

    for (p = labels->buckets[h]; p != NULL; p = p->link)
        if (lab == p->sym.u.lab.label) return &p->sym;

    p = memalloc(sizeof(*p), funca);
    memset(p, 0, sizeof(*p));

    p->sym.name = stringd(lab);
    p->sym.scope = LABELS;
    p->sym.up = labels->all;
    labels->all = &p->sym;
    p->link = labels->buckets[h];
    labels->buckets[h] = p;
    p->sym.generated = TRUE;
    p->sym.u.lab.label = lab;

    (*IR->defsymbol)(&p->sym);

    return &p->sym;
}

/****************************************************************************
 *                                                                          *
 * Function: intconst                                                       *
 *                                                                          *
 * Purpose : Make an integer constant.                                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

SYMBOL *intconst(int n)
{
    VALUE v;

    v.i = n;
    return constant(inttype, v);
}

/****************************************************************************
 *                                                                          *
 * Function: strconst                                                       *
 *                                                                          *
 * Purpose : Make a string constant.                                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

SYMBOL *strconst(const char *str)
{
    VALUE v;
    SYMBOL *sym;

    v.p = (char *)str;
    sym = constant(new_array(chartype, strlen(v.p)+1, 0), v);
    if (!sym->u.c.loc) sym->u.c.loc = make_ident(STATIC, sym->type, GLOBAL);

    return sym;
}

/****************************************************************************
 *                                                                          *
 * Function: constant                                                       *
 *                                                                          *
 * Purpose : Install and return constant value of type ty.                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-07-07  Added check for cbstring option (drivers).           *
 *                                                                          *
 ****************************************************************************/

SYMBOL *constant(TYPE *ty, VALUE v)
{
    uint_t h = (uint_t)v.u & (HASHSIZE-1);
    ENTRY *p;

    ty = unqual(ty);

    for (p = constants->buckets[h]; p != NULL && !options.cbstring; p = p->link)
    {
        if (is_same_type(ty, p->sym.type, TRUE))
        {
            switch (ty->op)
            {
                case INT_:     if (v.i == p->sym.u.c.v.i) return &p->sym; else break;
                case UNSIGNED: if (v.u == p->sym.u.c.v.u) return &p->sym; else break;
                case FLOAT_:   if (v.d == p->sym.u.c.v.d) return &p->sym; else break;
                case FUNCTION: if (v.g == p->sym.u.c.v.g) return &p->sym; else break;
                case ARRAY:    /* fall through */
                case POINTER:  if (v.p == p->sym.u.c.v.p) return &p->sym; else break;
                default: assert(0);
            }
        }
    }

    p = memalloc(sizeof(*p), PERM);
    memset(p, 0, sizeof(*p));

    p->sym.name = value_to_str(ty, v);
    p->sym.scope = CONSTANTS;
    p->sym.type = ty;
    p->sym.sclass = STATIC;
    p->sym.u.c.v = v;
    p->link = constants->buckets[h];
    p->sym.up = constants->all;
    constants->all = &p->sym;
    constants->buckets[h] = p;

    if (ty->u.sym && !ty->u.sym->addressed)
        (*IR->defsymbol)(&p->sym);
    p->sym.defined = TRUE;

    return &p->sym;
}

/****************************************************************************
 *                                                                          *
 * Function: value_to_str                                                   *
 *                                                                          *
 * Purpose : Return string for the constant v of type ty.                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

char *value_to_str(TYPE *ty, VALUE v)
{
    ty = unqual(ty);

    switch (ty->op)
    {
        case INT_:     return stringd(v.i);
        case UNSIGNED: return stringf((v.u & ~0x7FFF) ? "0x%X" : "%U", v.u);
        case FLOAT_:   return stringf("%g", (double)v.d);
        case FUNCTION: return stringf("%p", v.g);
        case POINTER:  return stringf("%p", v.p);
        case ARRAY:
            if (ty->type == chartype || ty->type == signedchartype || ty->type == unsignedchartype)
                return v.p;
            else
                return stringf("%p", v.p);
    }

    assert(0);
    return NULL;
}

/****************************************************************************
 *                                                                          *
 * Function: add_locus                                                      *
 *                                                                          *
 * Purpose : Remember location of the most recently added symbol.           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#ifdef XREF
void add_locus(TABLE *tp, COORDINATE *cp)
{
    loci = listappend(cp, loci);
    symbols = listappend(all_symbols(tp), symbols);
}
#endif

/****************************************************************************
 *                                                                          *
 * Function: use_symbol                                                     *
 *                                                                          *
 * Purpose : Add src to the list of uses for sym.                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#ifdef XREF
void use_symbol(SYMBOL *sym, COORDINATE src)
{
    COORDINATE *cp;

    cp = memalloc(sizeof(*cp), PERM);
    *cp = src;
    sym->uses = listappend(cp, sym->uses);
}
#endif

/****************************************************************************
 *                                                                          *
 * Function: find_type_symbol                                               *
 *                                                                          *
 * Purpose : Find type ty in identifiers.                                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

SYMBOL *find_type_symbol(TYPE *ty)
{
    TABLE *tp = identifiers;
    assert(tp);

    do
    {
        int i;

        for (i = 0; i < HASHSIZE; i++)
        {
            ENTRY *p;

            for (p = tp->buckets[i]; p != NULL; p = p->link)
            {
                if (p->sym.type == ty && p->sym.sclass == TYPEDEF)
                    return &p->sym;
            }
        }
    } while ((tp = tp->previous) != NULL);

    return NULL;
}
