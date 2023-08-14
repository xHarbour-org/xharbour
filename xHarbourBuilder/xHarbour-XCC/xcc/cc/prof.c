/****************************************************************************
 *                                                                          *
 * File    : prof.c                                                         *
 *                                                                          *
 * Purpose : ISO C Compiler; Basic block profiling support.                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-12-30  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "lcc.h"

#ifdef PROF

/*
 * Note! Should we exclude inline functions from profiling...?
 */

struct CALLSITE {
    char *file, *name;
    union COORDINATE {
        unsigned int coord;
        struct { unsigned int y:16, x:10, index:6; } le;
        struct { unsigned int index:6, x:10, y:16; } be;
    } u;
};

struct FUNC {
    struct FUNC *link;
    struct caller *callers;
    char *name;
    union COORDINATE src;
};

struct MAP {  /* source code map; 200 coordinates/map */
    int size;
    union COORDINATE u[200];
};

int npoints;                /* # of execution points */
int ncalled = -1;           /* #times prof.out says current function was called */
static SYMBOL *proflink;    /* symbol for file's struct _bbdata */
static SYMBOL *counters;    /* symbol for _counters */
static LIST *maplist;       /* list of struct MAP *'s */
static LIST *filelist;      /* list of file names */
static SYMBOL *funclist;    /* list of struct FUNC *'s */
static SYMBOL *afunc;       /* current function's struct FUNC */

/* Static function prototypes */
static void prof_entry(SYMBOL *, SYMBOL *);
static void prof_exit(SYMBOL *, SYMBOL *, TREE *);
static void prof_func(SYMBOL *, SYMBOL *);
static void prof_vars(SYMBOL *);
static void prof_call(SYMBOL *, COORDINATE *, TREE **);
static void prof_incr(SYMBOL *, COORDINATE *, TREE **);
static int fileindex(const char *);
static int padding(int, int);

/****************************************************************************
 *                                                                          *
 * Function: prof_init                                                      *
 *                                                                          *
 * Purpose : Initialize the basic block profiling.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-12-30  Created                                              *
 *                                                                          *
 ****************************************************************************/

void prof_init(void)
{
    proflink = make_ident(STATIC, new_array(voidptype, 0, 0), GLOBAL);

    attach((APPLYFN)prof_entry, proflink, &events.entry);
    attach((APPLYFN)prof_exit, proflink, &events.returns);
    attach((APPLYFN)prof_func, proflink, &events.exit);
    attach((APPLYFN)prof_vars, proflink, &events.end);

    counters = make_ident(STATIC, new_array(inttype, 0, 0), GLOBAL);

    attach((APPLYFN)prof_call, counters, &events.calls);
    attach((APPLYFN)prof_incr, counters, &events.points);

    maplist = listappend(memalloc(sizeof(struct MAP), PERM), maplist);
    ((struct MAP *)maplist->data)->size = 0;
}

/****************************************************************************
 *                                                                          *
 * Function: prof_entry                                                     *
 *                                                                          *
 * Purpose : Return tree for _prologue(&afunc, &proflink).                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-12-30  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void prof_entry(SYMBOL *proflink, SYMBOL *f)
{
    static SYMBOL *prologue;

    afunc = make_ident(STATIC, new_array(voidptype, 4, 0), GLOBAL);
    if (prologue == 0)
    {
        prologue = make_symbol(EXTERN, "_prologue", func_type(inttype, voidptype, voidptype, NULL));
        prologue->defined = 0;
    }

    new_forest(vcall(prologue, voidtype, pointer(id_tree(afunc)), pointer(id_tree(proflink)), NULL), 0, 0);
}

/****************************************************************************
 *                                                                          *
 * Function: prof_exit                                                      *
 *                                                                          *
 * Purpose : Return tree for _epilogue(&afunc).                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-12-30  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void prof_exit(SYMBOL *proflink, SYMBOL *f, TREE *e)
{
    static SYMBOL *epilogue;

    if (epilogue == 0)
    {
        epilogue = make_symbol(EXTERN, "_epilogue", func_type(inttype, voidptype, NULL));
        epilogue->defined = 0;
    }

    new_forest(vcall(epilogue, voidtype, pointer(id_tree(afunc)), NULL), 0, 0);
}

/****************************************************************************
 *                                                                          *
 * Function: prof_func                                                      *
 *                                                                          *
 * Purpose : Emit function name and src coordinates.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-12-30  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void prof_func(SYMBOL *proflink, SYMBOL *f)
{
    union COORDINATE u;
    VALUE v;

    define_global(afunc, DATA);
    init_pointer(funclist);
    init_pointer(NULL);
    init_pointer(strconst(f->name)->u.c.loc);

    if (IR->little_endian)
    {
        u.le.x = f->u.fcn.pt.x;
        u.le.y = f->u.fcn.pt.y;
        u.le.index = fileindex(f->u.fcn.pt.file);
    }
    else
    {
        u.be.x = f->u.fcn.pt.x;
        u.be.y = f->u.fcn.pt.y;
        u.be.index = fileindex(f->u.fcn.pt.file);
    }

    (*IR->defconst)(U, unsignedtype->size, (v.u = u.coord, v));
    padding(3*voidptype->size + unsignedtype->size, afunc->type->align);

    funclist = afunc;
}

/****************************************************************************
 *                                                                          *
 * Function: prof_vars                                                      *
 *                                                                          *
 * Purpose : Emit definition for basic block counting data.                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-12-30  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void prof_vars(SYMBOL *proflink)
{
    int i, j, n;
    VALUE v;
    struct MAP **mp;
    SYMBOL *coords, *files, **p;

    n = (npoints <= 0) ? 1 : npoints;
    counters->type = new_array(inttype, n, 0);
    define_global(counters, BSS);
    (*IR->space)(counters->type->size);

    files = make_ident(STATIC, new_array(charptype, 1, 0), GLOBAL);
    define_global(files, LIT);
    for (p = listvector(&filelist, PERM); *p; p++)
        init_pointer((*p)->u.c.loc);
    init_pointer(NULL);

    coords = make_ident(STATIC, new_array(unsignedtype, n, 0), GLOBAL);
    define_global(coords, LIT);
    for (i = n, mp = listvector(&maplist, PERM); *mp; i -= (*mp)->size, mp++)
    {
        for (j = 0; j < (*mp)->size; j++)
            (*IR->defconst)(U, unsignedtype->size, (v.u = (*mp)->u[j].coord, v));
    }
    if (i > 0)
        (*IR->space)(i*coords->type->type->size);

    (*IR->defconst)(U, unsignedtype->size, (v.u = 0, v));
    define_global(proflink, DATA);
    init_pointer(NULL);

    (*IR->defconst)(U, inttype->size, (v.u = n, v));
    padding(voidptype->size + inttype->size, proflink->type->align);

    init_pointer(counters);
    init_pointer(coords);
    init_pointer(files);
    init_pointer(funclist);
}

/****************************************************************************
 *                                                                          *
 * Function: prof_call                                                      *
 *                                                                          *
 * Purpose : Build tree to set _CALLSITE at call site *cp.                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-12-30  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void prof_call(SYMBOL *counters, COORDINATE *cp, TREE **e)
{
    static SYMBOL *caller;
    VALUE v;
    union COORDINATE u;
    SYMBOL *p = make_ident(STATIC, new_array(voidptype, 0, 0), GLOBAL);
    TREE *t;

    define_global(p, LIT);
    init_pointer(cp->file ? strconst(cp->file)->u.c.loc : (SYMBOL *)0);
    init_pointer(strconst(funcsym->name)->u.c.loc);

    if (IR->little_endian)
    {
        u.le.x = cp->x;
        u.le.y = cp->y;
    }
    else
    {
        u.be.x = cp->x;
        u.be.y = cp->y;
    }

    (*IR->defconst)(U, unsignedtype->size, (v.u = u.coord, v));
    padding(2*voidptype->size + unsignedtype->size, p->type->align);

    if (caller == 0)
    {
        caller = make_symbol(EXTERN, "_caller", ptr(voidptype));
        caller->defined = 0;
    }

    for (t = *e; generic(t->op) != CALL; t = t->kids[0])
        assert(t->op == RIGHT || !t->kids[1]);
    assert(generic(t->op) == CALL);

    t = new_tree(t->op, t->type,
        new_tree(RIGHT, t->kids[0]->type,
        t->kids[0],
        new_tree(RIGHT, t->kids[0]->type, assignment(caller, id_tree(p)), t->kids[0])),
        t->kids[1]);

    for ( ; generic((*e)->op) != CALL; e = &(*e)->kids[0])
        ;

    *e = t;
}

/****************************************************************************
 *                                                                          *
 * Function: prof_incr                                                      *
 *                                                                          *
 * Purpose : Build tree to increment execution point at *cp.                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-12-30  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void prof_incr(SYMBOL *counters, COORDINATE *cp, TREE **e)
{
    struct MAP *mp = maplist->data;
    TREE *t;

    if (need_const)
        return;

    /* append *cp to source map */
    if (mp->size >= NELEMS(mp->u))
    {
        mp = memalloc(sizeof(*mp), PERM);
        mp->size = 0;
        maplist = listappend(mp, maplist);
    }

    if (IR->little_endian)
    {
        mp->u[mp->size].le.x = cp->x;
        mp->u[mp->size].le.y = cp->y;
        mp->u[mp->size++].le.index = fileindex(cp->file);
    }
    else
    {
        mp->u[mp->size].be.x = cp->x;
        mp->u[mp->size].be.y = cp->y;
        mp->u[mp->size++].be.index = fileindex(cp->file);
    }

    t = increment_tree('+', rvalue((*optree['+'])(ADD, pointer(id_tree(counters)),
        const_tree(npoints++, inttype))), const_tree(1, inttype));

    if (*e)
        *e = new_tree(RIGHT, (*e)->type, t, *e);
    else
        *e = t;
}

/****************************************************************************
 *                                                                          *
 * Function: fileindex                                                      *
 *                                                                          *
 * Purpose : Add file to list of file names, return its index.              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-12-30  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int fileindex(const char *file)
{
    if (file)
    {
        LIST *lp;
        int i = 1;

        if ((lp = filelist) != NULL)
        {
            do
            {
                lp = lp->link;
                if (((SYMBOL *)lp->data)->u.c.v.p == file)
                    return i;
                i++;
            } while (lp != filelist);
        }

        filelist = listappend(strconst(file), filelist);
        return i;
    }

    return 0;
}

/****************************************************************************
 *                                                                          *
 * Function: padding                                                        *
 *                                                                          *
 * Purpose : Emit space, if necessary, to make size%align == 0.             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-12-30  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int padding(int size, int align)
{
    if (size % align)
    {
        (*IR->space)(align - size % align);
        size = roundup(size, align);
    }
    return size;
}

#endif  /* PROF */
