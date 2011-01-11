/****************************************************************************
 *                                                                          *
 * File    : types.c                                                        *
 *                                                                          *
 * Purpose : ISO C Compiler; Type handling.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           05-01-02  Added optimized_struct_type().                       *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop
#include <float.h>

#include "lcc.h"

typedef struct _ENTRY ENTRY;
typedef struct _ENTRY {
    TYPE type;
    ENTRY *link;
} ENTRY;

/* Locals */
static ENTRY *typetable[128];
static int maxlevel;
static SYMBOL *ptrsym;

/*
 * Basic types.
 */
TYPE *booltype;                 /* _Bool */
TYPE *chartype;                 /* char */
TYPE *doubletype;               /* double */
TYPE *floattype;                /* float */
TYPE *inttype;                  /* signed int */
TYPE *longdoubletype;           /* long double */
TYPE *longtype;                 /* long */
TYPE *longlongtype;             /* long long */
TYPE *shorttype;                /* signed short int */
TYPE *signedchartype;           /* signed char */
TYPE *unsignedchartype;         /* unsigned char */
TYPE *unsignedlongtype;         /* unsigned long int */
TYPE *unsignedlonglongtype;     /* unsigned long long int */
TYPE *unsignedshorttype;        /* unsigned short int */
TYPE *unsignedtype;             /* unsigned int */
TYPE *funcptype;                /* void (*)() */
TYPE *charptype;                /* char* */
TYPE *voidptype;                /* void* */
TYPE *voidtype;                 /* void */
TYPE *unsignedptrtype;          /* unsigned type to hold void* */
TYPE *signedptrtype;            /* signed type to hold void* */
TYPE *widechartype;             /* unsigned type that represents wchar_t */
#ifdef HAS_C99_COMPLEX
TYPE *complexdoubletype;        /* _Complex double */
TYPE *complexfloattype;         /* _Complex float */
TYPE *complexlongdoubletype;    /* _Complex long double */
#endif
#ifdef HAS_M64_TYPE
TYPE *m64type;                  /* __m64 MMX type */
#endif

/* Static function prototypes */
static FIELD *is_struct_field(const char *, FIELD *);
static bool_t has_empty_struct_field(TYPE *);
static TYPE *new_basic_type(int, char *, METRICS);
static TYPE *new_type(int, TYPE *, int, int, SYMBOL *);

/****************************************************************************
 *                                                                          *
 * Function: setup_basic_types                                              *
 *                                                                          *
 * Purpose : Initialize the basic types.                                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void setup_basic_types(void)
{
    METRICS voidmetric = { 0, 0, 0 };
#ifdef HAS_M64_TYPE
    SYMBOL *sym;
#endif

    assert(IR);

    /*
     * Must do this first...
     */
    ptrsym = install_symbol(string("T*"), &types, GLOBAL, PERM);
    ptrsym->addressed = IR->ptrmetric.outofline;
    ptrsym->u.limits.max.p = (void *)(ptrdiff_t)ones(8*IR->ptrmetric.size);
    ptrsym->u.limits.min.p = 0;

    /*
     * Scalar types.
     */
    booltype = new_basic_type(UNSIGNED, "_Bool", IR->boolmetric);
    chartype = new_basic_type(IR->unsigned_char ? UNSIGNED : INT_, "char", IR->charmetric);
    doubletype = new_basic_type(FLOAT_, "double", IR->doublemetric);
    floattype = new_basic_type(FLOAT_, "float", IR->floatmetric);
    inttype = new_basic_type(INT_, "int", IR->intmetric);
    longdoubletype = new_basic_type(FLOAT_, "long double", IR->longdoublemetric);
    longtype = new_basic_type(INT_, "long int", IR->longmetric);
    longlongtype = new_basic_type(INT_, "long long int", IR->longlongmetric);
    shorttype = new_basic_type(INT_, "short", IR->shortmetric);
    signedchartype = new_basic_type(INT_, "signed char", IR->charmetric);
    unsignedchartype = new_basic_type(UNSIGNED, "unsigned char", IR->charmetric);
    unsignedlongtype = new_basic_type(UNSIGNED, "unsigned long", IR->longmetric);
    unsignedlonglongtype = new_basic_type(UNSIGNED, "unsigned long long", IR->longlongmetric);
    unsignedshorttype = new_basic_type(UNSIGNED, "unsigned short", IR->shortmetric);
    unsignedtype = new_basic_type(UNSIGNED, "unsigned int", IR->intmetric);
    voidtype = new_basic_type(VOID_, "void", voidmetric);

    /*
     * Complex types.
     */
#ifdef HAS_C99_COMPLEX
    complexdoubletype = new_struct(STRUCT, "");
    new_struct_field("re", complexdoubletype, doubletype);
    new_struct_field("im", complexdoubletype, doubletype);
    complexdoubletype->size = 2 * doubletype.size;
    complexdoubletype->align = doubletype.align;

    complexfloattype = new_struct(STRUCT, "");
    new_struct_field("re", complexfloattype, floattype);
    new_struct_field("im", complexfloattype, floattype);
    complexfloattype->size = 2 * floattype.size;
    complexfloattype->align = floattype.align;

    complexlongdoubletype = new_struct(STRUCT, "");
    new_struct_field("re", complexlongdoubletype, longdoubletype);
    new_struct_field("im", complexlongdoubletype, longdoubletype);
    complexlongdoubletype->size = 2 * longdoubletype.size;
    complexlongdoubletype->align = longdoubletype.align;
#endif

    /*
     * Special MMX types.
     */
#ifdef HAS_M64_TYPE
    m64type = new_struct(UNION, string("__m64"));
    new_struct_field(string("_m64_u64"), m64type, unsignedlonglongtype);
    new_struct_field(string("_m64_f32"), m64type, new_array(floattype, 2, 0));
    new_struct_field(string("_m64_i8"),  m64type, new_array(signedchartype, 8, 0));
    new_struct_field(string("_m64_i16"), m64type, new_array(shorttype, 4, 0));
    new_struct_field(string("_m64_i32"), m64type, new_array(inttype, 2, 0));
    new_struct_field(string("_m64_i64"), m64type, longlongtype);
    new_struct_field(string("_m64_u8"),  m64type, new_array(unsignedchartype, 8, 0));
    new_struct_field(string("_m64_u16"), m64type, new_array(unsignedshorttype, 4, 0));
    new_struct_field(string("_m64_u32"), m64type, new_array(unsignedtype, 2, 0));
    m64type->size = m64type->align = 8;
    //
    sym = install_symbol(string("__m64"), &identifiers, GLOBAL, PERM);
    sym->type = m64type;
    sym->sclass = TYPEDEF;
#endif

    /*
     * Pointer types.
     */
    voidptype = ptr(voidtype);
    funcptype = ptr(func(voidtype, NULL, NULL, TRUE, 0));
    charptype = ptr(chartype);

    /*
     * Generic unsigned ptr.
     */
    if (unsignedptrtype == NULL && unsignedshorttype->size == voidptype->size && unsignedshorttype->align == voidptype->align) unsignedptrtype = unsignedshorttype;
    if (unsignedptrtype == NULL && unsignedtype->size == voidptype->size && unsignedtype->align == voidptype->align) unsignedptrtype = unsignedtype;
    if (unsignedptrtype == NULL && unsignedlongtype->size == voidptype->size && unsignedlongtype->align == voidptype->align) unsignedptrtype = unsignedlongtype;
    if (unsignedptrtype == NULL && unsignedlonglongtype->size == voidptype->size && unsignedlonglongtype->align == voidptype->align) unsignedptrtype = unsignedlonglongtype;
    if (unsignedptrtype == NULL) unsignedptrtype = new_type(UNSIGNED, NULL, voidptype->size, voidptype->align, voidptype->u.sym);

    /*
     * Generic signed ptr.
     */
    if (signedptrtype == NULL && shorttype->size == voidptype->size && shorttype->align == voidptype->align) signedptrtype = shorttype;
    if (signedptrtype == NULL && inttype->size == voidptype->size && inttype->align == voidptype->align) signedptrtype = inttype;
    if (signedptrtype == NULL && longtype->size == voidptype->size && longtype->align == voidptype->align) signedptrtype = longtype;
    if (signedptrtype == NULL && longlongtype->size == voidptype->size && longlongtype->align == voidptype->align) signedptrtype = longlongtype;
    if (signedptrtype == NULL) signedptrtype = new_type(INT_, NULL, voidptype->size, voidptype->align, voidptype->u.sym);

    widechartype = unsignedshorttype;

    /* check <= compares */
    assert(isscalar(booltype));
    assert(isarith(booltype));
}

/****************************************************************************
 *                                                                          *
 * Function: new_basic_type                                                 *
 *                                                                          *
 * Purpose : Initialize a new basic type node.                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static TYPE *new_basic_type(int op, char *name, METRICS m)
{
    SYMBOL *sym = install_symbol(string(name), &types, GLOBAL, PERM);
    TYPE *ty = new_type(op, 0, m.size, m.align, sym);

    assert(ty->align == 0 || ty->size % ty->align == 0);

    sym->type = ty;
    sym->addressed = m.outofline;

    switch (ty->op)
    {
        case INT_:
            sym->u.limits.max.i = ones(8*ty->size) >> 1;
            sym->u.limits.min.i = -sym->u.limits.max.i - 1;
            break;

        case UNSIGNED:
            sym->u.limits.max.u = ones(8*ty->size);
            sym->u.limits.min.u = 0;
            break;

        case FLOAT_:
            if (ty->size == sizeof(float))
                sym->u.limits.max.d =  FLT_MAX;
            else if (ty->size == sizeof(double))
                sym->u.limits.max.d =  DBL_MAX;
            else
                sym->u.limits.max.d = LDBL_MAX;
            sym->u.limits.min.d = -sym->u.limits.max.d;
            break;

        case VOID_:
            break;

        default:
            assert(0);
    }

    return ty;
}

/****************************************************************************
 *                                                                          *
 * Function: new_type                                                       *
 *                                                                          *
 * Purpose : Allocate a new type node.                                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static TYPE *new_type(int op, TYPE *ty, int size, int align, SYMBOL *sym)
{
    uint_t h = (op ^ ((uint_t)ty >> 3)) & (NELEMS(typetable)-1);
    ENTRY *tn;

    if (op != FUNCTION && (op != ARRAY || size > 0))
    {
        for (tn = typetable[h]; tn != NULL; tn = tn->link)
        {
            if (tn->type.op == op && tn->type.type == ty &&
                tn->type.size == size && tn->type.align == align &&
                tn->type.u.sym == sym)
            {
                return &tn->type;
            }
        }
    }

    tn = memalloc(sizeof(*tn), PERM);
    memset(tn, 0, sizeof(*tn));

    tn->type.op = op;
    tn->type.type = ty;
    tn->type.size = size;
    tn->type.align = align;
    tn->type.u.sym = sym;
    tn->link = typetable[h];
    typetable[h] = tn;

    return &tn->type;
}

/****************************************************************************
 *                                                                          *
 * Function: remove_types_from_scope                                        *
 *                                                                          *
 * Purpose : Remove local types.  Called from leave_scope().                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void remove_types_from_scope(int lev)
{
    if (maxlevel >= lev)
    {
        int i;

        maxlevel = 0;

        for (i = 0; i < NELEMS(typetable); i++)
        {
            ENTRY **tq = &typetable[i], *tn;

            while ((tn = *tq) != NULL)
            {
                if (tn->type.op == FUNCTION)
                {
                    tq = &tn->link;
                }
                else if (tn->type.u.sym && tn->type.u.sym->scope >= lev)
                {
                    *tq = tn->link;
                }
                else
                {
                    if (tn->type.u.sym && tn->type.u.sym->scope > maxlevel)
                        maxlevel = tn->type.u.sym->scope;

                    tq = &tn->link;
                }
            }
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: new_array                                                      *
 *                                                                          *
 * Purpose : Construct the type 'array 0..n-1 of ty'.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           03-08-14  Support for variable-length arrays added.            *
 *                                                                          *
 ****************************************************************************/

TYPE *new_array(TYPE *ty, int nelems, int align)
{
    assert(ty);

    if (isfunc(ty))
    {
        apperror(RCERROR(ERROR_ILLEGAL_TYPE_ARRAY), ty);
        return new_array(inttype, nelems, 0);
    }

    if (isvla(ty))  /* VLA size is calculated at runtime */
        return new_type(ARRAY, ty, 0, (align) ? align : ty->align, NULL);

    if (isarray(ty) && ty->size == 0)
        apperror(RCERROR(ERROR_MISSING_ARRAY_SIZE));

    if (ty->size == 0)
    {
        if (unqual(ty) == voidtype)
            apperror(RCERROR(ERROR_ILLEGAL_TYPE_ARRAY), ty);
        else
            apperror(RCWARNING2(ERROR_UNDEFINED_ARRAY_DECL), ty);
    }
    else if (nelems > INT_MAX/ty->size)
    {
        apperror(RCERROR(ERROR_ARRSIZE_EXCEEDS_X_BYTES), ty, INT_MAX);
        nelems = 1;
    }

    return new_type(ARRAY, ty, nelems*ty->size, (align) ? align : ty->align, NULL);
}

/****************************************************************************
 *                                                                          *
 * Function: new_struct                                                     *
 *                                                                          *
 * Purpose : Install a new structure/union/enum depending on op.            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

TYPE *new_struct(int op, char *tag)
{
    SYMBOL *sym;

    assert(tag);

    if (*tag == '\0')
    {
        tag = stringd(make_label(1));
    }
    else if ((sym = lookup_symbol(tag, types)) != NULL &&
        (sym->scope == scope || sym->scope == PARAM && scope == PARAM+1))
    {
        if (sym->type->op == op && !sym->defined)
            return sym->type;

        apperror(RCERROR(ERROR_REDEFINITION_SEE_DEF), sym->name, &sym->src);
    }

    sym = install_symbol(tag, &types, scope, PERM);
    sym->type = new_type(op, NULL, 0, 0, sym);

    if (sym->scope > maxlevel)
        maxlevel = sym->scope;

    sym->src = src;
    return sym->type;
}

/****************************************************************************
 *                                                                          *
 * Function: new_struct_field                                               *
 *                                                                          *
 * Purpose : Install a new field in ty with type fty.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           00-12-19  Handling of anonymous struct/unions added.           *
 *           01-07-24  Better error checking on flexible array members.     *
 *           03-08-14  Support for variable-length arrays added.            *
 *                                                                          *
 ****************************************************************************/

FIELD *new_struct_field(char *name, TYPE *ty, TYPE *fty)
{
    FIELD **pp = &ty->u.sym->u.s.flist;
    FIELD *field;
    TYPE *ty1;

    /* VLA's are not allowed in structures */
    for (ty1 = fty; isptr(ty1); ty1 = ty1->type)
        ;
    if (isvla(ty1))
        apperror(RCERROR(ERROR_ILLEGAL_VLA_USAGE));

    if (!name && isstruct(fty))
    {
        /* verify fields in a anonymous struct/union */
        for (field = unqual(fty)->u.sym->u.s.flist; field != NULL; field = field->link)
        {
            if (is_struct_field(field->name, *pp))
                apperror(RCERROR(ERROR_DUPLICATE_FIELD_NAME), name, ty);
        }
    }
    else
    {
        if (name == NULL)  /* anonymous bit field */
            name = stringd(make_label(1));

        if (is_struct_field(name, *pp))
            apperror(RCERROR(ERROR_DUPLICATE_FIELD_NAME), name, ty);
    }

    if (!isunion(ty) && has_empty_struct_field(ty))
        apperror(RCERROR(ERROR_ILLEGAL_FLEXIBLE_FIELD), ty);

    for (field = *pp; field != NULL; pp = &field->link, field = *pp)
        ;

    field = memalloc(sizeof(*field), PERM);
    memset(field, 0, sizeof(*field));

    *pp = field;
    field->name = name;
    field->type = fty;

#ifdef XREF
    if (options.xreflevel > 1)
    {
        if (ty->u.sym->u.s.ftab == NULL)
            ty->u.sym->u.s.ftab = new_symbol_table(NULL, scope);

        install_symbol(name, &ty->u.sym->u.s.ftab, 0, PERM)->src = src;
    }
#endif

    return field;
}

/****************************************************************************
 *                                                                          *
 * Function: struct_field_reference                                         *
 *                                                                          *
 * Purpose : Find field name of type ty, return entry.                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

FIELD *struct_field_reference(const char *name, TYPE *ty)
{
    FIELD *field = is_struct_field(name, unqual(ty)->u.sym->u.s.flist);

#ifdef XREF
    if (field && options.xreflevel > 1)
    {
        SYMBOL *sym;

        sym = lookup_symbol(name, unqual(ty)->u.sym->u.s.ftab);
        assert(sym);

        use_symbol(sym, src);
    }
#endif

    return field;
}

/****************************************************************************
 *                                                                          *
 * Function: is_struct_field                                                *
 *                                                                          *
 * Purpose : If name is a field in flist, return ptr to the field struct.   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           00-12-19  Handling of anonymous struct/unions added.           *
 *           01-01-02  New FIELD member added: xoffset.                     *
 *                                                                          *
 ****************************************************************************/

static FIELD *is_struct_field(const char *name, FIELD *flist)
{
    for (; flist != NULL; flist = flist->link)
    {
        if (!flist->name && isstruct(flist->type))
        {
            FIELD *field;

            /* search for field in a anonymous struct/union */
            field = is_struct_field(name, unqual(flist->type)->u.sym->u.s.flist);
            if (field)
            {
                field->xoffset += flist->offset;
                return field;
            }
        }
        else if (flist->name == name)
        {
            flist->xoffset = flist->offset;
            break;
        }
    }

    return flist;
}

/****************************************************************************
 *                                                                          *
 * Function: has_empty_struct_field                                         *
 *                                                                          *
 * Purpose : Check if flist contains a zero-sized field.                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-07-24  Created                                              *
 *                                                                          *
 ****************************************************************************/

static bool_t has_empty_struct_field(TYPE *ty)
{
    FIELD *field;

    for (field = unqual(ty)->u.sym->u.s.flist; field != NULL; field = field->link)
    {
        if (isstruct(field->type) && has_empty_struct_field(field->type))
            return TRUE;
        else if (field->type->size == 0)
            return TRUE;
    }

    return FALSE;
}

/****************************************************************************
 *                                                                          *
 * Function: optimized_struct_type                                          *
 *                                                                          *
 * Purpose : Return unsigned type, for small structure optimization.        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           05-01-02  Created                                              *
 *                                                                          *
 ****************************************************************************/

TYPE *optimized_struct_type(TYPE *ty)
{
    /* any interest in small structure optimizations? */
    if (IR->wants_optb && !IR->wants_callb && isstruct(ty))
    {
        int size = unqual(ty)->size;

        assert(unsignedchartype->size == 1 && unsignedchartype->op == U);
        assert(unsignedshorttype->size == 2 && unsignedshorttype->op == U);
        assert(unsignedtype->size == 4 && unsignedtype->op == U);
        assert(unsignedlonglongtype->size == 8 && unsignedlonglongtype->op == U);

        if (size == unsignedchartype->size) return unsignedchartype;
        if (size == unsignedshorttype->size) return unsignedshorttype;
        if (size == unsignedtype->size) return unsignedtype;
        if (size == unsignedlonglongtype->size) return unsignedlonglongtype;
    }

    return NULL;
}

/****************************************************************************
 *                                                                          *
 * Function: composite_type                                                 *
 *                                                                          *
 * Purpose : Return the composite type of ty1 and ty2, or NULL,             *
 *           if ty1 and ty2 are incompatible.                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           03-08-14  Support for variable-length arrays added.            *
 *           03-10-28  Support for optional arguments added.                *
 *                                                                          *
 ****************************************************************************/

TYPE *composite_type(TYPE *ty1, TYPE *ty2)
{
    if (ty1 == ty2)
        return ty1;

    assert(ty1->op == ty2->op);

    switch (ty1->op)
    {
        case POINTER:
            return ptr(composite_type(ty1->type, ty2->type));

        case CONST_+RESTRICT_:
            return qual(CONST_, qual(RESTRICT_, composite_type(ty1->type, ty2->type)));

        case CONST_+VOLATILE_+RESTRICT_:
            return qual(CONST_, qual(VOLATILE_, qual(RESTRICT_, composite_type(ty1->type, ty2->type))));

        case VOLATILE_+RESTRICT_:
            return qual(VOLATILE_, qual(RESTRICT_, composite_type(ty1->type, ty2->type)));

        case CONST_+VOLATILE_:
            return qual(CONST_, qual(VOLATILE_, composite_type(ty1->type, ty2->type)));

        case CONST_:
        case VOLATILE_:
        case RESTRICT_:
            return qual(ty1->op, composite_type(ty1->type, ty2->type));

        case ARRAY:
        {
            TYPE *ty = composite_type(ty1->type, ty2->type);

            /*
             * A composite type can be constructed from two types that are compatible; it is a
             * type that is compatible with both of the two types and satisfies the following
             * conditions:
             *
             * — If one type is an array of known constant size, the composite type is an array
             *   of that size; otherwise, if one type is a variable length array, the composite
             *   type is that type.
             *
             * — If only one type is a function type with a parameter type list (a function
             *   prototype), the composite type is a function prototype with the parameter type
             *   list.
             *
             * — If both types are function types with parameter type lists, the type of each
             *   parameter in the composite parameter type list is the composite type of the
             *   corresponding parameters.
             *
             * These rules apply recursively to the types from which the two types are derived.
             */
            if (ty1->size && ty1->type->size)
                return new_array(ty, ty1->size/ty1->type->size, ty1->align);

            if (ty2->size && ty2->type->size)
                return new_array(ty, ty2->size/ty2->type->size, ty2->align);

            if (ty1->u.arr.e != 0)  /* VLA */
            {
                ty = new_array(ty, 0, 0);
                ty->u.arr.e = ty1->u.arr.e;
                ty->u.arr.sym = ty1->u.arr.sym;
                return ty;
            }

            if (ty2->u.arr.e != 0)  /* VLA */
            {
                ty = new_array(ty, 0, 0);
                ty->u.arr.e = ty2->u.arr.e;
                ty->u.arr.sym = ty2->u.arr.sym;
                return ty;
            }
/*
            old style code:

            if (ty1->size && (ty1->type->size && ty2->size == 0 || ty1->size == ty2->size))
                return new_array(ty, ty1->size/ty1->type->size, ty1->align);

            if (ty2->size && ty2->type->size && ty1->size == 0)
                return new_array(ty, ty2->size/ty2->type->size, ty2->align);
*/
            return new_array(ty, 0, 0);
        }

        case FUNCTION:
        {
            TYPE **p1 = ty1->u.fcn.prototype, **p2 = ty2->u.fcn.prototype;
            TREE **e1 = ty1->u.fcn.optparam, **e2 = ty2->u.fcn.optparam;
            TYPE *ty = composite_type(ty1->type, ty2->type);
            LIST *tlist = NULL, *elist = NULL;

            if (p1 == NULL && p2 == NULL)
                return func(ty, NULL, NULL, TRUE, CDECL_);

            if (p1 != NULL && p2 == NULL)
                return func(ty, p1, e1, ty1->u.fcn.oldstyle, ty1->u.fcn.calltype);

            if (p2 != NULL && p1 == NULL)
                return func(ty, p2, e2, ty2->u.fcn.oldstyle, ty2->u.fcn.calltype);

            for ( ; *p1 && *p2; p1++, p2++)
            {
                TYPE *ty = composite_type(unqual(*p1), unqual(*p2));

                if (isconst(*p1) || isconst(*p2))
                    ty = qual(CONST_, ty);
                if (isvolatile(*p1) || isvolatile(*p2))
                    ty = qual(VOLATILE_, ty);
                if (isrestrict(*p1) || isrestrict(*p2))
                    ty = qual(RESTRICT_, ty);

                tlist = listappend(ty, tlist);

                if (e1 != NULL && e2 != NULL)
                {
                    if (*e1 && *e2)
                        apperror(RCERROR(ERROR_REDEFINITION_OF_OPT_PARAM));
                    elist = listappend(*e1 ? *e1 : *e2, elist);
                    e1++, e2++;  /* OK - don't touch! */
                }
            }

            if (elist) e1 = listvector(&elist, PERM);

            assert(*p1 == NULL && *p2 == NULL);
            return func(ty, listvector(&tlist, PERM), e1 ? e1 : e2, FALSE, ty1->u.fcn.calltype);
        }
    }

    assert(0);
    return NULL;
}

/****************************************************************************
 *                                                                          *
 * Function: qual                                                           *
 *                                                                          *
 * Purpose : Construct the type 'op ty' where op is CONST_ or VOLATILE_.    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           01-09-17  Changed type qualifier error to warning, for C99.    *
 *           03-08-14  Support for variable-length arrays added.            *
 *                                                                          *
 ****************************************************************************/

TYPE *qual(int op, TYPE *ty)
{
    if (isarray(ty))
    {
        TYPE *nty = new_type(ARRAY, qual(op, ty->type), ty->size, ty->align, NULL);
        nty->u.arr.e = ty->u.arr.e;
        nty->u.arr.sym = ty->u.arr.sym;
        ty = nty;
    }
    else if (isfunc(ty))
        apperror(RCWARNING1(ERROR_QUAL_FUNC_TYPE_IGNORED));
    else if (isconst(ty) && op == CONST_ || isvolatile(ty) && op == VOLATILE_ || isrestrict(ty) && op == RESTRICT_)
        apperror(RCWARNING2(ERROR_QUAL_TYPE_IGNORED), op);
    else
    {
        if (isqual(ty))
        {
            op += ty->op;
            ty = ty->type;
        }

        ty = new_type(op, ty, ty->size, ty->align, NULL);
    }

    return ty;
}

/****************************************************************************
 *                                                                          *
 * Function: array_to_ptr                                                   *
 *                                                                          *
 * Purpose : Convert ty from 'array of ty' to 'pointer to ty'.              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

TYPE *array_to_ptr(TYPE *ty)
{
    if (isarray(ty))
        return ptr(ty->type);

    apperror(RCERROR(ERROR_TYPE_ERROR_NO_ARRAY));

    return ptr(ty);
}

/****************************************************************************
 *                                                                          *
 * Function: ptr                                                            *
 *                                                                          *
 * Purpose : Convert ty to 'pointer to ty'.  See also isptr().              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

TYPE *ptr(TYPE *ty)
{
    assert(ptrsym);
    return new_type(POINTER, ty, IR->ptrmetric.size, IR->ptrmetric.align, ptrsym);
}

/****************************************************************************
 *                                                                          *
 * Function: dereference_type                                               *
 *                                                                          *
 * Purpose : Dereference ty, type *ty.                                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

TYPE *dereference_type(TYPE *ty)
{
    if (isptr(ty))
        ty = ty->type;
    else
        apperror(RCERROR(ERROR_TYPE_ERROR_NO_POINTER));

    return isenum(ty) ? unqual(ty)->type : ty;
}

/****************************************************************************
 *                                                                          *
 * Function: func                                                           *
 *                                                                          *
 * Purpose : Construct the type 'function (void) returning ty'.             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           03-10-28  Support for optional arguments added.                *
 *                                                                          *
 ****************************************************************************/

TYPE *func(TYPE *ty, TYPE **prototype, TREE **optparam, int oldstyle, int calltype)
{
    assert(options.microsoft || calltype == CDECL_ || calltype == 0);

    if (ty && (isarray(ty) || isfunc(ty)))
        apperror(RCERROR(ERROR_ILLEGAL_RETURN_TYPE), ty);

    ty = new_type(FUNCTION, ty, 0, 0, NULL);
    ty->u.fcn.prototype = prototype;
    ty->u.fcn.optparam = optparam;
    ty->u.fcn.oldstyle = oldstyle;
    ty->u.fcn.calltype = calltype;

    return ty;
}

/****************************************************************************
 *                                                                          *
 * Function: func_return                                                    *
 *                                                                          *
 * Purpose : For 'function returning ty', return ty.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

TYPE *func_return(TYPE *ty)
{
    if (isfunc(ty))
        return ty->type;

    apperror(RCERROR(ERROR_TYPE_ERROR_NO_FUNCTION));

    return inttype;
}

/****************************************************************************
 *                                                                          *
 * Function: func_type                                                      *
 *                                                                          *
 * Purpose : Return a function type for rty function (ty,...)               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-12-30  Created                                              *
 *           03-09-21  Changed to LCC42 style.                              *
 *                                                                          *
 ****************************************************************************/

TYPE *func_type(TYPE *rty, ...)
{
    LIST *list = NULL;
    TYPE *ty = NULL;
    va_list ap;

    va_start(ap, rty);
    ty = va_arg(ap, TYPE *);
    for (; ty != NULL; ty = va_arg(ap, TYPE *))
        list = listappend(ty, list);
    va_end(ap);

    return func(rty, listvector(&list, PERM), NULL, FALSE, CDECL_);
}

/****************************************************************************
 *                                                                          *
 * Function: has_varargs                                                    *
 *                                                                          *
 * Purpose : True if function can take variable number of arguments.        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

bool_t has_varargs(TYPE *ty)
{
    if (isfunc(ty) && ty->u.fcn.prototype)
    {
        int i;

        for (i = 0; ty->u.fcn.prototype[i]; i++)
            ;

        return i > 1 && ty->u.fcn.prototype[i-1] == voidtype;
    }

    return FALSE;
}

/****************************************************************************
 *                                                                          *
 * Function: has_prototype                                                  *
 *                                                                          *
 * Purpose : True if ty has no function types or they all have prototypes.  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

bool_t has_prototype(TYPE *ty)
{
    if (ty == 0)
        return TRUE;

    switch (ty->op)
    {
        case CONST_:
        case VOLATILE_:
        case RESTRICT_:
        case VOLATILE_+RESTRICT_:
        case CONST_+RESTRICT_:
        case CONST_+VOLATILE_:
        case CONST_+VOLATILE_+RESTRICT_:
        case POINTER:
        case ARRAY:
            return has_prototype(ty->type);

        case FUNCTION:
            return has_prototype(ty->type) && ty->u.fcn.prototype;

        case STRUCT:
        case UNION:
        case VOID_:
        case FLOAT_:
        case ENUM:
        case INT_:
        case UNSIGNED:
            return TRUE;
    }

    assert(0);
    return FALSE;
}

/****************************************************************************
 *                                                                          *
 * Function: is_same_type                                                   *
 *                                                                          *
 * Purpose : Check if ty1 is same type as ty2. Return ret if ty1==ty2,      *
 *           but one is incomplete.                                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           03-08-14  Support for variable-length arrays added.            *
 *                                                                          *
 ****************************************************************************/

bool_t is_same_type(TYPE *ty1, TYPE *ty2, bool_t ret)
{
    if (ty1 == ty2)
        return TRUE;

    if (ty1->op != ty2->op)
        return FALSE;

    switch (ty1->op)
    {
        case ENUM:
        case UNION:
        case STRUCT:
        case UNSIGNED:
        case INT_:
        case FLOAT_:
            return FALSE;

        case POINTER:
            return is_same_type(ty1->type, ty2->type, TRUE);

        case CONST_:
        case VOLATILE_:
        case RESTRICT_:
        case VOLATILE_+RESTRICT_:
        case CONST_+RESTRICT_:
        case CONST_+VOLATILE_:
        case CONST_+VOLATILE_+RESTRICT_:
            return is_same_type(ty1->type, ty2->type, TRUE);

        case ARRAY:
            if (is_same_type(ty1->type, ty2->type, TRUE))
            {
                if (ty1->size == ty2->size)
                    return TRUE;

                if (ty1->u.arr.e != 0 || ty2->u.arr.e != 0)  /* VLA */
                    return TRUE;

                if (ty1->size == 0 || ty2->size == 0)
                    return ret;  /* incomplete */
            }
            return FALSE;

        case FUNCTION:
            if (is_same_type(ty1->type, ty2->type, TRUE))
            {
                TYPE **p1 = ty1->u.fcn.prototype;
                TYPE **p2 = ty2->u.fcn.prototype;

                if (ty1->u.fcn.calltype != ty2->u.fcn.calltype)
                    return FALSE;

                if (p1 == p2)
                    return TRUE;

                if (p1 && p2)
                {
                    for ( ; *p1 && *p2; p1++, p2++)
                    {
                        if (!is_same_type(unqual(*p1), unqual(*p2), TRUE))
                            return FALSE;
                    }

                    if (*p1 == NULL && *p2 == NULL)
                        return TRUE;
                }
                else
                {
                    if (has_varargs(p1 ? ty1 : ty2))
                        return FALSE;

                    if (p1 == NULL)
                        p1 = p2;

                    for ( ; *p1; p1++)
                    {
                        TYPE *ty = unqual(*p1);

                        if (promote_type(ty) != (isenum(ty) ? ty->type : ty))
                            return FALSE;
                    }
                    return TRUE;
                }
            }
            return FALSE;

    }

    assert(0);
    return FALSE;
}

/****************************************************************************
 *                                                                          *
 * Function: promote_type                                                   *
 *                                                                          *
 * Purpose : The usual integral promotions.                                 *
 *                                                                          *
 * Comment : If an int can represent all values of the original type, the   *
 *           value is converted to an int; otherwise, it is converted to    *
 *           an unsigned int. These are called the integer promotions.      *
 *           All other types are unchanged by the integer promotions.       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

TYPE *promote_type(TYPE *ty)
{
    ty = unqual(ty);

    switch (ty->op)
    {
        case ENUM:
            return inttype;

        case INT_:
            if (ty->size < inttype->size)
                return inttype;
            break;

        case UNSIGNED:
            if (ty->size < inttype->size)
                return inttype;
            if (ty->size < unsignedtype->size)
                return unsignedtype;
            break;

        case FLOAT_:
            if (ty->size < doubletype->size)
                return doubletype;
    }

    return ty;
}

/****************************************************************************
 *                                                                          *
 * Function: signedint_type                                                 *
 *                                                                          *
 * Purpose : Map type into signed integer type.                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

TYPE *signedint_type(TYPE *ty)
{
    if (ty->op == INT_)
        return ty;

    assert(ty->op == UNSIGNED);

    if (ty->size == inttype->size) return inttype;
    if (ty->size == longtype->size) return longtype;
    if (ty->size == longlongtype->size) return longlongtype;

    assert(0);
    return NULL;
}

/****************************************************************************
 *                                                                          *
 * Function: ttob                                                           *
 *                                                                          *
 * Purpose : Map type into size specification for a DAG operator.           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

int ttob(TYPE *ty)
{
    switch (ty->op)
    {
        case CONST_:
        case VOLATILE_:
        case RESTRICT_:
        case VOLATILE_+RESTRICT_:
        case CONST_+RESTRICT_:
        case CONST_+VOLATILE_:
        case CONST_+VOLATILE_+RESTRICT_:
            return ttob(ty->type);

        case VOID_:
        case INT_:
        case UNSIGNED:
        case FLOAT_:
            return ty->op + sizeop(ty->size);

        case POINTER:
            return POINTER + sizeop(voidptype->size);

        case FUNCTION:
            return POINTER + sizeop(funcptype->size);

        case ARRAY:
        case STRUCT:
        case UNION:
            return STRUCT;

        case ENUM:
            return INT_ + sizeop(inttype->size);
    }

    assert(0);
    return INT_;
}

/****************************************************************************
 *                                                                          *
 * Function: btot                                                           *
 *                                                                          *
 * Purpose : Map DAG operator into size specification.                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

TYPE *btot(int op, int size)
{
    switch (optype(op))
    {
        case F:
            if (size == floattype->size) return floattype;
            if (size == doubletype->size) return doubletype;
            if (size == longdoubletype->size) return longdoubletype;
            assert(0); return 0;

        case I:
            if (chartype->op == INT_) if (size == chartype->size) return chartype;
            if (size == signedchartype->size) return signedchartype;
            if (size == shorttype->size) return shorttype;
            if (size == inttype->size) return inttype;
            if (size == longtype->size) return longtype;
            if (size == longlongtype->size) return longlongtype;
            assert(0); return 0;

        case U:
            if (chartype->op == UNSIGNED) if (size == chartype->size) return chartype;
            if (size == unsignedchartype->size) return unsignedchartype;
            if (size == unsignedshorttype->size) return unsignedshorttype;
            if (size == unsignedtype->size) return unsignedtype;
            if (size == unsignedlongtype->size) return unsignedlongtype;
            if (size == unsignedlonglongtype->size) return unsignedlonglongtype;
            if (size == booltype->size) return booltype;
            assert(0); return 0;

        case P:
            if (size == voidptype->size) return voidptype;
            if (size == funcptype->size) return funcptype;
            assert(0); return 0;
    }

    assert(0);
    return 0;
}

/****************************************************************************
 *                                                                          *
 * Function: outtype                                                        *
 *                                                                          *
 * Purpose : Output type ty.  Only used by vfprint().                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           03-08-14  Support for variable-length arrays added.            *
 *           03-10-12  Added argument bp, for DLL mode.                     *
 *           04-10-05  Emit note for incomplete struct/union/enum.          *
 *                                                                          *
 ****************************************************************************/

void outtype(TYPE *ty, FILE *f, char *bp)
{
    switch (ty->op)
    {
        case CONST_:
        case VOLATILE_:
        case RESTRICT_:
        case VOLATILE_+RESTRICT_:
        case CONST_+RESTRICT_:
        case CONST_+VOLATILE_:
        case CONST_+VOLATILE_+RESTRICT_:
            fprint(f, bp, "%k %t", ty->op, ty->type);
            break;

        case STRUCT:
        case UNION:
        case ENUM:
            assert(ty->u.sym);
            assert(ty->u.sym->name);
            if (ty->size == 0)
                fprint(f, bp, "[incomplete] ");
            if (*ty->u.sym->name >= '1' && *ty->u.sym->name <= '9')
            {
                SYMBOL *sym;

                if ((sym = find_type_symbol(ty)) != NULL)
                    fprint(f, bp, sym->name);
                else
                    fprint(f, bp, "%k", ty->op);
            }
            else
            {
                fprint(f, bp, "%k %s", ty->op, ty->u.sym->name);
            }
            break;

        case VOID_:
        case FLOAT_:
        case INT_:
        case UNSIGNED:
            fprint(f, bp, ty->u.sym->name);
            break;

        case POINTER:
            fprint(f, bp, "%t *", ty->type);
            break;

        case FUNCTION:
            fprint(f, bp, "%t %k function", ty->type, (ty->u.fcn.calltype) ? ty->u.fcn.calltype : ELLIPSIS);
            if (ty->u.fcn.prototype && ty->u.fcn.prototype[0])
            {
                int i;
                fprint(f, bp, "(%t", ty->u.fcn.prototype[0]);
                for (i = 1; ty->u.fcn.prototype[i]; i++)
                {
                    if (ty->u.fcn.prototype[i] == voidtype)
                        fprint(f, bp, ", ...");
                    else
                        fprint(f, bp, ", %t", ty->u.fcn.prototype[i]);
                }
                fprint(f, bp, ")");
            }
            else if (ty->u.fcn.prototype && ty->u.fcn.prototype[0] == 0)
            {
                fprint(f, bp, "(void)");
            }
            break;

        case ARRAY:
            if (ty->size > 0 && ty->type && ty->type->size > 0)
            {
                fprint(f, bp, "%t [%d", ty->type, ty->size/ty->type->size);
                while (ty->type && isarray(ty->type) && ty->type->type->size > 0)
                {
                    ty = ty->type;
                    fprint(f, bp, ",%d", ty->size/ty->type->size);
                }
                fprint(f, bp, "]");
            }
            else if (ty->u.arr.e != 0 && ty->type)  /* VLA */
            {
                fprint(f, bp, "%t [*]", ty->type);
            }
            else if (ty->type)
            {
                fprint(f, bp, "%t []", ty->type);
            }
            else
            {
                fprint(f, bp, "array []");
            }
            break;

        default:
            assert(0);
    }
}

/****************************************************************************
 *                                                                          *
 * Function: print_prototype                                                *
 *                                                                          *
 * Purpose : Output a prototype declaration for function sym.               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void print_prototype(SYMBOL *sym, SYMBOL *callee[])
{
    if (sym->type->u.fcn.prototype)
    {
        if (sym->attr.inlined) fprint(stdout, NULL, "%k ", INLINE);
        print_declaration(sym, sym->type);
    }
    else
    {
        LIST *list = NULL;

        if (callee[0] == 0)
        {
            list = listappend(voidtype, list);
        }
        else
        {
            int i;

            for (i = 0; callee[i]; i++)
                list = listappend(callee[i]->type, list);
        }

        print_declaration(sym, func(func_return(sym->type), listvector(&list, PERM), NULL, FALSE, 0));
    }
}

/****************************************************************************
 *                                                                          *
 * Function: print_declaration                                              *
 *                                                                          *
 * Purpose : Output a C declaration for symbol sym of type ty.              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void print_declaration(SYMBOL *sym, TYPE *ty)
{
    switch (sym->sclass)
    {
        case AUTO:
            fprint(stdout, NULL, "%s;\n", typestring(ty, sym->name));
            break;

        case STATIC:
        case EXTERN:
            fprint(stdout, NULL, "%k %s;\n", sym->sclass, typestring(ty, sym->name));
            break;

        case TYPEDEF:
        case ENUM:
            break;

        default:
            assert(0);
    }
}

/****************************************************************************
 *                                                                          *
 * Function: typestring                                                     *
 *                                                                          *
 * Purpose : Return ty as C declaration for str, which may be "".           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           03-08-14  Support for variable-length arrays added.            *
 *                                                                          *
 ****************************************************************************/

char *typestring(TYPE *ty, char *str)
{
    if (!str) str = "";  /* 04-12-07: just in case */

    for (; ty != NULL; ty = ty->type)
    {
        SYMBOL *sym;

        switch (ty->op)
        {
            int calltype;

            case CONST_:
            case VOLATILE_:
            case RESTRICT_:
            case VOLATILE_+RESTRICT_:
            case CONST_+RESTRICT_:
            case CONST_+VOLATILE_:
            case CONST_+VOLATILE_+RESTRICT_:
                if (isptr(ty->type))
                    str = stringf("%k %s", ty->op, str);
                else
                    return stringf("%k %s", ty->op, typestring(ty->type, str));
                break;

            case STRUCT:
            case UNION:
            case ENUM:
                assert(ty->u.sym);

                if ((sym = find_type_symbol(ty)) != NULL)  /* typedef? */
                    return (*str) ? stringf("%s %s", sym->name, str) : sym->name;

                if (*ty->u.sym->name >= '1' && *ty->u.sym->name <= '9')
                    apperror(RCWARNING2(ERROR_UNNAMED_IN_PROTOTYPE), ty->op);

                if (*str)
                    return stringf("%k %s %s", ty->op, ty->u.sym->name, str);
                else
                    return stringf("%k %s", ty->op, ty->u.sym->name);

            case VOID_:
            case FLOAT_:
            case INT_:
            case UNSIGNED:
                return (*str) ? stringf("%s %s", ty->u.sym->name, str) : ty->u.sym->name;

            case POINTER:
                if (!ischar(ty->type) && (sym = find_type_symbol(ty)) != NULL)  /* typedef? */
                    return (*str) ? stringf("%s %s", sym->name, str) : sym->name;

                str = stringf(isarray(ty->type) || isfunc(ty->type) ? "(*%s)" : "*%s", str);
                break;

            case FUNCTION:
                if ((sym = find_type_symbol(ty)) != NULL)  /* typedef? */
                    return (*str) ? stringf("%s %s", sym->name, str) : sym->name;

                calltype = (ty->u.fcn.calltype) ? ty->u.fcn.calltype : ELLIPSIS;

                if (ty->u.fcn.prototype == 0)
                {
                    str = stringf("%k %s()", calltype, str);
                }
                else if (ty->u.fcn.prototype[0])
                {
                    int i;

                    str = stringf("%k %s(%s", calltype, str, typestring(ty->u.fcn.prototype[0], ""));

                    for (i = 1; ty->u.fcn.prototype[i]; i++)
                    {
                        if (ty->u.fcn.prototype[i] == voidtype)
                            str = stringf("%s, ...", str);
                        else
                            str = stringf("%s, %s", str, typestring(ty->u.fcn.prototype[i], ""));
                    }
                    str = stringf("%s)", str);
                }
                else
                {
                    str = stringf("%k %s(void)", calltype, str);
                }
                break;

            case ARRAY:
                if ((sym = find_type_symbol(ty)) != NULL)  /* typedef? */
                    return (*str) ? stringf("%s %s", sym->name, str) : sym->name;

                if (ty->u.arr.e != 0)  /* VLA */
                    str = stringf("%s[*]", str);
                else if (ty->type && ty->type->size > 0)
                    str = stringf("%s[%d]", str, ty->size/ty->type->size);
                else
                    str = stringf("%s[]", str);
                break;

            default:
                assert(0);
        }
    }

    assert(0);
    return NULL;
}
