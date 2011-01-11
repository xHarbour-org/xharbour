/****************************************************************************
 *                                                                          *
 * File    : init.c                                                         *
 *                                                                          *
 * Purpose : ISO C Compiler; Initializer handling.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-12-05  Rewritten to support designated initializers.        *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "lcc.h"

/* 
 * Known limitations:
 *
 * 1) Bitfield initializers must be constant.
 *
 * 2) An entire struct/union can't be initialized, the following should really work:
 *
 * int f(void)
 * {
 *    struct s1 { int i, j; } xs1 = { 5 };
 *    struct s2 { struct s1 s1; } xs2 = { .s1 = { 1, 2 }, .s1 = xs1, .s1.j = 3 };
 *    return xs2.s1.i;
 * }
 */

/* initializer element description */
typedef struct _ELEM ELEM;
typedef struct _ELEM {
    int offset;     // element byte offset.
    TREE *e;        // expression (cast to proper type).
};

/* Static function prototypes */
static TYPE *parse_initializer(TYPE *, int, int, LIST **);
static void initializer_end(int, char []);
static int parse_struct_initializer(int, TYPE *, int, int, LIST **, bool_t);
static int parse_bitfield_initializer(FIELD *, FIELD *, int, LIST **);
static int parse_array_initializer(int, TYPE *, int, int, LIST **, bool_t);
static void string_to_array(int, void *, TYPE *, int, LIST **);
static TREE *initializer_tree(TREE *, TREE *, TYPE *, int, LIST *, bool_t);
static TREE *memset_tree(TREE *, int);
static int emit_constant_initializer(TYPE *, int, LIST *);
static int emit_constant_tree(TREE *);
static bool_t is_constant_tree(TREE *);
static int constant_elems(LIST *);
static int assignment_elems(TYPE *);
static LIST *add_elem(int, TREE *, LIST *);
static TREE *get_elem(int, TYPE *, LIST *);
static ELEM *find_elem(int, LIST *);

/****************************************************************************
 *                                                                          *
 * Function: set_segment                                                    *
 *                                                                          *
 * Purpose : Switch to segment seg, if necessary.                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void set_segment(int seg)
{
    static int curseg = 0;

    if (curseg != seg)
        (*IR->segment)(seg);

    curseg = seg;
}

/****************************************************************************
 *                                                                          *
 * Function: init_pointer                                                   *
 *                                                                          *
 * Purpose : Initialize a pointer to sym or to 0 if sym==0.                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-12-30  Created                                              *
 *                                                                          *
 ****************************************************************************/

#ifdef PROF
void init_pointer(SYMBOL *sym)
{
    if (sym)
    {
        (*IR->defaddress)(sym);
        sym->ref++;
    }
    else
    {
        static VALUE v;
        (*IR->defconst)(P, voidptype->size, v);
    }
}
#endif

/****************************************************************************
 *                                                                          *
 * Function: global_initializer                                             *
 *                                                                          *
 * Purpose : Initialize a global or static variable (constant expr).        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-12-05  Created                                              *
 *           04-12-29  Cache read-only value for optimization.              *
 *                                                                          *
 ****************************************************************************/

TYPE *global_initializer(SYMBOL *sym, TYPE *ty)
{
    LIST *elems = NULL;

    need_const++;
    ty = parse_initializer(ty, 0, 0, &elems);
    need_const--;

    if (constant_elems(elems) == listelems(elems))
    {
        /* make sure we emit to the correct segment! */
        set_segment(sym->u.seg);
        emit_constant_initializer(ty, 0, elems);

        /* cache const values for optimization (LIT == STATIC) -
         * we still emit the value, in case it's address is taken. */
        if (sym->u.seg == LIT && isscalar(sym->type) && !ty->u.sym->addressed)
        {
            assert(listelems(elems) == 1);
            sym->e = tree_to_arena(get_elem(0, ty, elems), PERM);
        }
    }
    else
    {
        /* will probably never get here */
        apperror(RCERROR(ERROR_INIT_MUST_BE_CONSTANT));
    }

    return ty;
}

/****************************************************************************
 *                                                                          *
 * Function: local_initializer                                              *
 *                                                                          *
 * Purpose : Initialize a local variable (possibly non-constant expr).      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-12-05  Created                                              *
 *           04-12-30  Assign all elements of optimized structures.         *
 *                                                                          *
 ****************************************************************************/

#define sparse(a,x)  ((a) > 5 && (((x) + 1.0) / (a)) < 0.75)

TREE *local_initializer(SYMBOL *sym, TYPE *ty)
{
    LIST *elems = NULL;
    int nelems;

    ty = parse_initializer(ty, 0, 0, &elems);

    if (isarray(sym->type) && sym->type->size == 0)
        sym->type = ty;

    /* assign all elements of optimized structures */
    if (isstruct(ty) && optimized_struct_type(ty) != NULL)
        return initializer_tree(NULL, id_tree(sym), ty, 0, elems, FALSE);

    /* get number of variable elements */
    nelems = listelems(elems) - constant_elems(elems);

    /* emit constant initializer */
    if (nelems == 0 && !options.cbstring)
    {
        TYPE *ty = sym->type, *ty1 = ty;
        SYMBOL *sym2;

        while (isarray(ty1))
            ty1 = ty1->type;

        if (!isconst(ty) && (!isarray(ty) || !isconst(ty1)))
            ty = qual(CONST_, ty);

        sym2 = make_ident(STATIC, ty, GLOBAL);
        define_global(sym2, LIT);
        emit_constant_initializer(ty, 0, elems);

        if (isarray(sym->type) && sym->type->size == 0 && sym2->type->size > 0)
            sym->type = new_array(sym->type->type, sym2->type->size/sym2->type->type->size, 0);

        return assignment(sym, id_tree(sym2));
    }
    else /* emit variable initializer */
    {
        int n = assignment_elems(ty);

        if (options.cbstring ? listelems(elems) < n : sparse(n, nelems))
        {
            /* clear memory and assign variable elements */
            TREE *e = memset_tree(pointer(id_tree(sym)), ty->size);
            return new_tree(RIGHT, voidtype, e, initializer_tree(NULL, id_tree(sym), ty, 0, elems, TRUE));
        }
        else
        {
            /* assign all elements */
            return initializer_tree(NULL, id_tree(sym), ty, 0, elems, FALSE);
        }
    }
}

#undef sparse

/****************************************************************************
 *                                                                          *
 * Function: parse_initializer                                              *
 *                                                                          *
 * Purpose : Parse an initializer.                                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           01-01-08  Warnings added for char vs widechar mismatch.        *
 *           03-09-08  Support for compound literals added.                 *
 *           04-12-05  Rewritten to support designated initializers.        *
 *                                                                          *
 ****************************************************************************/

static TYPE *parse_initializer(TYPE *ty, int lev, int offset, LIST **elems)
{
    static char stop[] = { IF, CHAR_, STATIC, 0 };
    int n = 0;
    TREE *e;
    TYPE *aty = NULL;

    ty = unqual(ty);

    if (isscalar(ty))
    {
        if (tok == '{')
        {
            tok = gettok();
            e = expr1(0);
            initializer_end(lev, stop);
        }
        else
        {
            e = expr1(0);
        }

        e = pointer(e);

        if ((aty = check_assignment(ty, e)) != NULL)
            e = cast(e, aty);
        else
            apperror(RCERROR(ERROR_INVALID_INIT_TYPE), e->type, ty);

        *elems = add_elem(offset, e, *elems);

        n = ty->size;
    }
    else if (isstruct(ty) && ty->size == 0)  /* struct or union */
    {
        static char stop[] = { CHAR_, STATIC, 0 };
        apperror(RCERROR(ERROR_CANNOT_INIT_UNDEFINED), ty);
        skipto(';', stop);
        return ty;
    }
    else if (isunion(ty))
    {
        if (tok == '{')
        {
            tok = gettok();
            n = parse_struct_initializer(ty->u.sym->u.s.flist->type->size, ty, lev + 1, offset, elems, TRUE);
            initializer_end(lev, stop);
        }
        else
        {
            if (lev == 0)
                apperror(RCERROR(ERROR_MISSING_BRACKET_IN_INIT), ty);
            n = parse_struct_initializer(ty->u.sym->u.s.flist->type->size, ty, lev + 1, offset, elems, FALSE);
        }
    }
    else if (isstruct(ty))
    {
        if (tok == '{')
        {
            tok = gettok();
            n = parse_struct_initializer(0, ty, lev + 1, offset, elems, TRUE);
            follow('}', stop);
        }
        else if (lev > 0)
        {
            n = parse_struct_initializer(ty->size, ty, lev + 1, offset, elems, FALSE);
        }
        else
        {
            apperror(RCERROR(ERROR_MISSING_BRACKET_IN_INIT), ty);
            n = parse_struct_initializer(ty->u.sym->u.s.flist->type->size, ty, lev + 1, offset, elems, FALSE);
        }
    }
    else if (isarray(ty))
    {
        aty = unqual(ty->type);

        if (tok == SCONST && isint(aty))
        {
            TYPE *sty = ischar(aty) ? chartype : widechartype;

            if (unqual(toksym->type->type) != sty)
                apperror(RCWARNING1(ERROR_NON_PORTABLE_USAGE), toksym->type->type, sty);

            if (ty->size > 0 && ty->size == toksym->type->size - aty->size)
                toksym->type = new_array(aty, ty->size/aty->size, 0);
            n = toksym->type->size;

            string_to_array(n, toksym->u.c.v.p, aty, offset, elems);
            toksym->ref++;

            tok = gettok();
        }
        else if (tok == '{')
        {
            tok = gettok();
            if (tok == SCONST && isint(aty))
            {
                ty = parse_initializer(ty, lev + 1, offset, elems);
                initializer_end(lev, stop);
                return ty;
            }

            n = parse_array_initializer(0, aty, lev + 1, offset, elems, TRUE);
            follow('}', stop);
        }
        else if (lev > 0 && ty->size > 0)
        {
            n = parse_array_initializer(ty->size, aty, lev + 1, offset, elems, FALSE);
        }
        else
        {
            apperror(RCERROR(ERROR_MISSING_BRACKET_IN_INIT), ty);
            n = parse_array_initializer(aty->size, aty, lev + 1, offset, elems, FALSE);
        }
    }

    if (ty->size)
    {
        if (n > ty->size)
            apperror(RCERROR(ERROR_TOO_MANY_INITIALIZERS));
    }
    else if (isarray(ty) && ty->type->size > 0)
    {
        ty = new_array(ty->type, n/ty->type->size, 0);
    }
    else
    {
        ty->size = n;
    }

    return ty;
}

/****************************************************************************
 *                                                                          *
 * Function: initializer_end                                                *
 *                                                                          *
 * Purpose : Finish off an initialization at level lev;                     *
 *           accepts trailing comma.                                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void initializer_end(int lev, char stop[])
{
    if (lev == 0 && tok == ',')
        tok = gettok();

    follow('}', stop);
}

/****************************************************************************
 *                                                                          *
 * Function: parse_struct_initializer                                       *
 *                                                                          *
 * Purpose : Parse a structure initializer list.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-12-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int parse_struct_initializer(int len, TYPE *ty, int lev, int offset, LIST **elems, bool_t toplev)
{
    FIELD *field = ty->u.sym->u.s.flist;
    int n = 0;
    int align;

    do
    {
        /* designated initializer? */
        if (tok == '.')
        {
            tok = gettok();
            if (tok == ID)
            {
                /* will also calculate xoffset */
                field = struct_field_reference(tokstr, ty);
                if (!field)
                {
                    apperror(RCERROR(ERROR_UNKNOWN_FIELD), tokstr, ty);
                    return 0;
                }
                tok = gettok();

                if (tok == '=')
                    tok = gettok();

                n = field->xoffset;
            }
            else
            {
                apperror(RCERROR(ERROR_EXPECTING_FIELD_NAME));
                return 0;
            }
        }

        if (field->offset > n)
            n += field->offset - n;

        if (field->lsb)
        {
            FIELD *fieldT = field;

            while (fieldT->link && fieldT->link->offset == field->offset)
                fieldT = fieldT->link;

            /* parse initializer(s) for current bitfield */
            n += parse_bitfield_initializer(field, fieldT->link, offset + n, elems);

            field = fieldT;
        }
        else
        {
            /* parse initializer for current field */
            parse_initializer(field->type, lev, offset + n, elems);
            n += field->type->size;
        }

        if (field->link)
        {
            field = field->link;
            align = field->type->align;
        }
        else
        {
            align = ty->align;
        }

        if (align && n%align)
            n = roundup(n, align);

        /* designation in a subitem must break the recursion */
        if (tok == '.' && toplev) continue;

        /* time to quit? */
        if (len > 0 && n >= len || tok != ',')
            break;

        tok = gettok();
    } while (tok != '}' && tok != '[' && (tok != '.' || toplev));

    /* maximum size in bytes */
    return n;
}

/****************************************************************************
 *                                                                          *
 * Function: parse_bitfield_initializer                                     *
 *                                                                          *
 * Purpose : Parse a bitfield initializer.                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-12-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int parse_bitfield_initializer(FIELD *field, FIELD *field_next, int offset, LIST **elems)
{
    TYPE *ty = field->type;
    uint_t bits = 0;
    int n = 0;
    int k;

    do
    {
        TYPE *aty;
        TREE *e;

        need_const++;

        /* parse expression (must be constant for now) */
        e = expr1(0);

        if ((aty = check_assignment(inttype, e)) != NULL)
        {
            e = cast(e, aty);
        }
        else
        {
            apperror(RCERROR(ERROR_INVALID_INIT_TYPE), e->type, inttype);
            e = const_tree(0, inttype);
        }

        need_const--;

        if (generic(e->op) != CNST)
        {
            apperror(RCERROR(ERROR_INIT_MUST_BE_CONSTANT));
            e = const_tree(0, inttype);
        }

        /* initializer as integer constant */
        k = (int)e->u.v.i;

        if (fieldsize(field) < 8*field->type->size)
        {
            if (field->type == inttype &&
                (k < -(int)(fieldmask(field)>>1)-1 ||
                 k > +(int)(fieldmask(field)>>1)) ||
                field->type == unsignedtype && (k & ~fieldmask(field)) != 0)
            {
                apperror(RCWARNING1(ERROR_INIT_EXCEEDS_FIELD_WIDTH));
            }

            k &= fieldmask(field);
        }

        /* update initializer value */
        bits |= k << fieldright(field);

        if (IR->little_endian)
        {
            if (fieldsize(field) + fieldright(field) > n)
                n = fieldsize(field) + fieldright(field);
        }
        else
        {
            if (fieldsize(field) + fieldleft(field) > n)
                n = fieldsize(field) + fieldleft(field);
        }

        if (field->link == field_next)
            break;

        field = field->link;
    } while (tok == ',' && (tok = gettok()) != 0);

    n = bits2bytes(n);

    /* remember initializer value */
    *elems = add_elem(offset, const_tree(bits, ty), *elems);

    /* maximum size in bytes */
    return n;
}

/****************************************************************************
 *                                                                          *
 * Function: parse_array_initializer                                        *
 *                                                                          *
 * Purpose : Parse an array initializer list.                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-12-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int parse_array_initializer(int len, TYPE *ty, int lev, int offset, LIST **elems, bool_t toplev)
{
    int n = 0, m = 0;

    do
    {
        /* designated initializer? */
        if (tok == '[')
        {
            tok = gettok();
            n = intexpr(']', 0);
            if (n < 0)
            {
                apperror(RCERROR(ERROR_ILLEGAL_ARRAY_INDEX), n);
                n = 0;
            }
            else if (len > 0 && n * ty->size >= len)
            {
                apperror(RCERROR(ERROR_ILLEGAL_ARRAY_INDEX), n);
                n = 0;
            }

            if (tok == '=')
                tok = gettok();
        }

        /* parse initializer for current index */
        parse_initializer(ty, lev, offset + n * ty->size, elems);

        /* set next default index, save maximum size: arr[8] = [0]...[7] */
        if (++n > m) m = n;

        /* designation in a subitem must break the recursion */
        if (tok == '[' && toplev) continue;

        /* time to quit? */
        if (len > 0 && n * ty->size >= len || tok != ',')
            break;

        tok = gettok();
    } while (tok != '}' && tok != '.' && (tok != '[' || toplev));

    /* maximum size in bytes */
    return m * ty->size;
}

/****************************************************************************
 *                                                                          *
 * Function: string_to_array                                                *
 *                                                                          *
 * Purpose : Convert literal string to array initializer.                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-12-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void string_to_array(int n, void *p, TYPE *ty, int offset, LIST **elems)
{
    int i;

    if (ischar(ty))
    {
        uchar_t *s = p;

        for (i = 0; i < n; i++)
        {
            /* convert individual characters to array initializers */
            *elems = add_elem(offset + i, const_tree(*s++, ty), *elems);
        }
    }
    else
    {
        widechar_t *s = p;

        for (i = 0; i < n; i += ty->size)
        {
            /* convert individual wide characters to array initializers */
            *elems = add_elem(offset + i, const_tree(*s++, ty), *elems);
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: initializer_tree                                               *
 *                                                                          *
 * Purpose : Construct tree for a non-constant initializer.                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-12-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

#define is_zero(e)  ((e)->op == CNST+I && (e)->u.v.i == 0 || \
                     (e)->op == CNST+U && (e)->u.v.u == 0 || \
                     (e)->op == CNST+P && (e)->u.v.p == 0)

static TREE *initializer_tree(TREE *q, TREE *l, TYPE *ty, int offset, LIST *elems, bool_t memzero)
{
    TREE *e;

    ty = unqual(ty);

    if (isscalar(ty))
    {
        /* build tree to initialize current offset */
        e = get_elem(offset, ty, elems);
        /* avoid unneeded assignments */
        if (!memzero || !is_constant_tree(e) || options.cbstring && !is_zero(e))
            q = new_tree(RIGHT, voidtype, q, assignment_tree(ASGN, l, e));
    }
    else if (isunion(ty))
    {
        int n = 0;

        e = get_elem(offset, NULL, elems);
        if (e != NULL)
        {
            /* build tree to initialize current union */
            q = initializer_tree(q, retype(l, e->type), e->type, offset, elems, memzero);
            n = e->type->size;
        }

        if (!memzero)
        {
            TYPE *ty1;

            /* emit padding for union */
            for (; n < ty->size; n += ty1->size)
            {
                ty1 = (ty->size - n) >= inttype->size ? inttype : (ty->size - n) == shorttype->size ? shorttype : chartype;
                e = rvalue(simplify(ADD+P, ptr(ty1), addrof(l), const_tree(n, signedptrtype)));
                q = initializer_tree(q, e, ty1, offset + n, NULL, memzero);
            }
        }
    }
    else if (isstruct(ty))
    {
        FIELD *field;

        /* walk through all fields */
        for (field = ty->u.sym->u.s.flist; field != NULL; field = field->link)
        {
            if (field->lsb)
            {
                FIELD *fieldT = field;

                while (fieldT->link && fieldT->link->offset == field->offset)
                    fieldT = fieldT->link;

                /* build tree to initialize current bitfield */
                e = rvalue(simplify(ADD+P, ptr(field->type), addrof(l), const_tree(field->xoffset, signedptrtype)));
                q = initializer_tree(q, e, field->type, offset + field->xoffset, elems, memzero);

                field = fieldT;
            }
            else
            {
                /* build tree to initialize current field */
                e = pointer(field_tree(addrof(l), field->name));
                q = initializer_tree(q, e, field->type, offset + field->xoffset, elems, memzero);
            }
        }
    }
    else if (isarray(ty))
    {
        int i, m = ty->size / ty->type->size;

        for (i = 0; i < m; i++)
        {
            /* build tree to address current index */
            e = (*optree['+'])(ADD, pointer(l), const_tree(i, signedptrtype));
            if (isfunc(e->type->type) || isarray(e->type->type))
                e = retype(e, e->type->type);
            else
                e = rvalue(e);
            /* build tree to initialize current index */
            q = initializer_tree(q, e, ty->type, offset + i * ty->type->size, elems, memzero);
        }
    }
    else assert(0);

    return q;
}

#undef is_zero

/****************************************************************************
 *                                                                          *
 * Function: memset_tree                                                    *
 *                                                                          *
 * Purpose : Construct tree to clear n bytes of memory.                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-12-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

static TREE *memset_tree(TREE *e, int n)
{
    if (IR->wants_intrinsic)
    {
        /* call intrinsic memset() - if supported by backend */
        e = new_tree(mkop(INTRIN2S, voidptype), voidptype, e, const_tree(n, unsignedtype));
        e->u.sym = intconst(INTRIN_MEMSET);
        return e;
    }
    else
    {
        /* call standard memset() */
        static SYMBOL *func = 0;
        if (func == 0)
        {
            func = make_symbol(EXTERN, "memset", func_type(voidptype, voidptype, inttype, unsignedtype, NULL));
            func->defined = 0;
        }
        return vcall(func, NULL, e, const_tree(0, inttype), const_tree(n, unsignedtype), NULL);
    }
}

/****************************************************************************
 *                                                                          *
 * Function: emit_constant_initializer                                      *
 *                                                                          *
 * Purpose : Emit tree for a constant initializer.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-12-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int emit_constant_initializer(TYPE *ty, int offset, LIST *elems)
{
    int n = 0;

    ty = unqual(ty);

    if (isscalar(ty))
    {
        /* emit initializer for current offset */
        n = emit_constant_tree(get_elem(offset, ty, elems));
    }
    else if (isunion(ty))
    {
        TREE *e;

        /* emit initializer for current union */
        e = get_elem(offset, NULL, elems);
        if (e != NULL)
            n = emit_constant_tree(e);

        /* emit padding for union */
        while (n < ty->size)
            n += emit_constant_initializer(chartype, offset + n, NULL);
    }
    else if (isstruct(ty))
    {
        FIELD *field;
        int align;

        /* walk through all fields */
        for (field = ty->u.sym->u.s.flist; field != NULL; field = field->link)
        {
            if (field->offset > n)
            {
                (*IR->space)(field->offset - n);
                n += field->offset - n;
            }

            if (field->lsb)
            {
                FIELD *fieldT = field;

                while (fieldT->link && fieldT->link->offset == field->offset)
                    fieldT = fieldT->link;

                /* emit initializer for current bitfield */
                n += emit_constant_initializer(field->type, offset + n, elems);

                field = fieldT;
            }
            else
            {
                /* emit initializer for current field */
                n += emit_constant_initializer(field->type, offset + n, elems);
            }

            if (field->link)
                align = field->type->align;
            else
                align = ty->align;

            /* emit padding for current field or whole struct */
            if (align && n%align)
            {
                (*IR->space)(align - n%align);
                n = roundup(n, align);
            }
        }
    }
    else if (isarray(ty))
    {
        while (n < ty->size)
        {
            /* emit initializer for current index */
            n += emit_constant_initializer(ty->type, offset + n, elems);
        }
    }
    else assert(0);

    assert(n == ty->size);
    return n;
}

/****************************************************************************
 *                                                                          *
 * Function: emit_constant_tree                                             *
 *                                                                          *
 * Purpose : Emit constant tree e; return size in bits.                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-12-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int emit_constant_tree(TREE *e)
{
    for (;;)
    {
        switch (generic(e->op))
        {
            case ADDRG:
                (*IR->defaddress)(e->u.sym);
                return e->type->size;

            case CNST:
                if (e->op == CNST+P && isarray(e->type))
                {
                    e = static_constant(e);
                    continue;
                }
                (*IR->defconst)(e->type->op, e->type->size, e->u.v);
                return e->type->size;

            case RIGHT:
                if (e->kids[0] && e->kids[1])
                    (void)emit_constant_tree(e->kids[0]);  /* (cast)(compound literal) */
                e = e->kids[1] ? e->kids[1] : e->kids[0];
                continue;

            case CVP:
                if (isarith(e->type))
                    apperror(RCERROR(ERROR_ILLEGAL_CONST_CAST), e->kids[0]->type, e->type);
                /* fall through */
            case CVI:
            case CVU:
            case CVF:
            case CBOOL:
                e = e->kids[0];
                continue;

            case ASGN:
                if (e->op == ASGN+B &&
                    generic(e->kids[0]->op) == ADDRG &&
                    generic(e->kids[1]->op) == INDIR &&
                    generic(e->kids[1]->kids[0]->op) == ADDRG)
                {
                    e = e->kids[1]->kids[0];
                    e->u.sym->ref++;
                    (*IR->defaddress)(e->u.sym);
                    return voidptype->size;
                }

            default:
                apperror(RCERROR(ERROR_INIT_MUST_BE_CONSTANT));
                return emit_constant_tree(const_tree(0, inttype));
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: is_constant_tree                                               *
 *                                                                          *
 * Purpose : Check if the given tree is constant.                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-12-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

static bool_t is_constant_tree(TREE *e)
{
    for (;;)
    {
        switch (generic(e->op))
        {
            case CNST:
            case ADDRG:
                return TRUE;

            case RIGHT:
                if (e->kids[0] && e->kids[1] && !is_constant_tree(e->kids[0]))  /* (cast)(compound literal) */
                    return FALSE;
                e = e->kids[1] ? e->kids[1] : e->kids[0];
                continue;

            case CVP:
                if (isarith(e->type))
                    return FALSE;
                /* fall through */
            case CVI:
            case CVU:
            case CVF:
            case CBOOL:
                e = e->kids[0];
                continue;

            case ASGN:
                if (e->op == ASGN+B &&
                    generic(e->kids[0]->op) == ADDRG &&
                    generic(e->kids[1]->op) == INDIR &&
                    generic(e->kids[1]->kids[0]->op) == ADDRG)
                    return TRUE;
                /* fall through */
            default:
                return FALSE;
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: constant_elems                                                 *
 *                                                                          *
 * Purpose : Count number of specified constant initializers.               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-12-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int constant_elems(LIST *list)
{
    int n = 0;

    if (list != NULL)
    {
        LIST *lp = list;
        do
        {
            ELEM *p = lp->data;
            if (is_constant_tree(p->e)) n++;
        } while ((lp = lp->link) != list);
    }

    return n;
}

/****************************************************************************
 *                                                                          *
 * Function: assignment_elems                                               *
 *                                                                          *
 * Purpose : Count number of assignments required (estimate).               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-12-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int assignment_elems(TYPE *ty)
{
    int n = 0;

    ty = unqual(ty);

    if (isscalar(ty) || isunion(ty))
    {
        /* bump count */
        n++;
    }
    else if (isstruct(ty))
    {
        FIELD *field;

        /* walk through all fields */
        for (field = ty->u.sym->u.s.flist; field != NULL; field = field->link)
        {
            if (field->lsb)
            {
                FIELD *fieldT = field;

                while (fieldT->link && fieldT->link->offset == field->offset)
                    fieldT = fieldT->link;

                /* count assignments for current bitfield */
                n += assignment_elems(field->type);

                field = fieldT;
            }
            else
            {
                /* count assignments for current field */
                n += assignment_elems(field->type);
            }
        }
    }
    else if (isarray(ty))
    {
        int i, m = ty->size / ty->type->size;

        for (i = 0; i < m; i++)
        {
            /* count assignments for current index */
            n += assignment_elems(ty->type);
        }
    }

    return n;
}

/****************************************************************************
 *                                                                          *
 * Function: add_elem                                                       *
 *                                                                          *
 * Purpose : Add or update initializer tree by offset.                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-12-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

static LIST *add_elem(int offset, TREE *e, LIST *list)
{
    ELEM *p;

    p = find_elem(offset, list);
    if (p)
    {
        apperror(RCWARNING1(ERROR_DUPLICATE_INITIALIZERS), offset);
        p->e = e;
        return list;
    }
    else
    {
        p = memalloc(sizeof(*p), STMT);
        p->offset = offset;
        p->e = e;
        return listappend(p, list);
    }
}

/****************************************************************************
 *                                                                          *
 * Function: get_elem                                                       *
 *                                                                          *
 * Purpose : Return initializer tree by offset.                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-12-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

static TREE *get_elem(int offset, TYPE *ty, LIST *list)
{
    ELEM *p;

    assert(!ty || isscalar(ty));

    p = find_elem(offset, list);
    if (p)
        return p->e;
    else if (ty)
        return cnst_tree(isenum(ty) ? ty->type : ty, (intmax_t)0);
    else
        return NULL;
}

/****************************************************************************
 *                                                                          *
 * Function: find_elem                                                      *
 *                                                                          *
 * Purpose : Find initializer element by offset.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-12-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

static ELEM *find_elem(int offset, LIST *list)
{
    if (list != NULL)
    {
        LIST *lp = list;
        do
        {
            ELEM *p = lp->data;
            if (p->offset == offset) return p;
        } while ((lp = lp->link) != list);
    }

    return NULL;
}
