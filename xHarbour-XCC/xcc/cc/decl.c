/****************************************************************************
 *                                                                          *
 * File    : decl.c                                                         *
 *                                                                          *
 * Purpose : ISO C Compiler; Declaration parsing.                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "lcc.h"

typedef struct _FCNENTRY {
    SYMBOL *func;
    SYMBOL **caller;
    SYMBOL **callee;
    CODE *codelist;
    TREE *args;         /* temp copy */
    LIST *refsyms;
    bool_t touched;
} FCNENTRY;

typedef struct _SYMREF {
    SYMBOL *sym;
    float refinc;
} SYMREF;

#define add(x,n)  ((x > inttype->u.sym->u.limits.max.i - (n)) ? (overflow=1, x) : x + (n))
#define chkoverflow(x,n)  ((void)add(x,n))

SYMBOL *funcsym;    /* current function */
SYMBOL *retv;       /* return value location for structs */
SEHINFO sehinfo;    /* SEH intrinsic info */
uint_t funca;       /* FUNC or PERM (inline) arena */
bool_t optparam;    /* parsing optional parameter expression? */
int funcvla;        /* number of seen VLA declarations */

static int regcount;

static LIST *autos = NULL;
static LIST *registers = NULL;
static LIST *functions = NULL;
static LIST *refsyms;

static ATTR noattr = {0};

/* Static function prototypes */
static void declarator(SYMBOL *(*)(int, ATTR, char *, TYPE *, COORDINATE *));
static TYPE *parse_declaration(TYPE *, char **, SYMBOL ***, bool_t);
static TYPE *parse_declaration1(char **, SYMBOL ***, bool_t, int *);
static TYPE *typenode(int, TYPE *);
static SYMBOL *declare_as_global(int, ATTR, char *, TYPE *, COORDINATE *);
static SYMBOL *declare_as_local(int, ATTR, char *, TYPE *, COORDINATE *);
static SYMBOL *declare_as_param(int, ATTR, char *, TYPE *, COORDINATE *);
static void init_global(SYMBOL *, bool_t);
static void parse_function_definition(int, ATTR, char *, TYPE *, SYMBOL *[], COORDINATE);
static SYMBOL **parse_function_parameters(TYPE *);
static void exit_function_parameters(SYMBOL *[]);
static void fixup_old_style_parameter(SYMBOL *, void *);
static TYPE *parse_enum_declaration(void);
static TYPE *parse_struct_declaration(int);
static void parse_struct_fields(TYPE *);
static TYPE *parse_type(int *, ATTR *);
static void parse_spec_declaration(ATTR *, int *);
static void finalize_extern(SYMBOL *, void *);
static void finalize_global(SYMBOL *, void *);
static void finalize_const(SYMBOL *, void *);
static void check_labels(SYMBOL *, void *);
static void check_references(SYMBOL *, void *);
static void check_inline(SYMBOL *, void *);
static TREE *tree_from_dag(NODE *, FCNENTRY *);
static void do_referenced_inline_functions(void);
static void touch_function(NODE *, bool_t *);

/****************************************************************************
 *                                                                          *
 * Function: parse_program                                                  *
 *                                                                          *
 * Purpose : Parse an entire C program, i.e. declarator*                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-07-15  Added call to intrinsic_init().                      *
 *                                                                          *
 ****************************************************************************/

void parse_program(void)
{
    int n;

    sehinfo.codename = string("_exception_code");
    sehinfo.infoname = string("_exception_info");

    intrinsic_init();

    scope = GLOBAL;

    for (n = 0; tok != EOI; n++)
    {
        funca = FUNC;
        if (kind[tok] == CHAR_ || kind[tok] == STATIC || tok == ID || tok == '*' || tok == '(')
        {
            declarator(declare_as_global);
            memfree(STMT);

#ifdef XREF
            if (options.xreflevel == 0)
                memfree(FUNC);
#else
            memfree(FUNC);
#endif
        }
        else
        {
            if (tok == ';')
                apperror(RCWARNING2(ERROR_EMPTY_DECLARATION));
            else
                apperror(RCERROR(ERROR_UNRECOGNIZED_DECL));

            tok = gettok();
        }
    }

    do_referenced_inline_functions();

    if (n == 0)
        apperror(RCWARNING1(ERROR_EMPTY_INPUT_FILE));

    /* don't report error with file/lineno */
    src.file = NULL;
    src.y = 0;
}

/****************************************************************************
 *                                                                          *
 * Function: declarator                                                     *
 *                                                                          *
 * Purpose : type [ decl ( , decl )* ] ;                                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-07-02  Support for fastcall keyword added.                  *
 *           04-09-08  Bugfix: typedef redeclaration check was all wrong.   *
 *                                                                          *
 ****************************************************************************/

static void declarator(SYMBOL *(*decl)(int, ATTR, char *, TYPE *, COORDINATE *))
{
    TYPE *ty;
    TYPE *ty1;
    ATTR attr = noattr;
    int sclass = 0;

    ty = parse_type(&sclass, &attr);

    if (tok == ID || tok == '*' || tok == '(' || tok == '[' || tok == CDECL_ || tok == STDCALL_ || tok == FASTCALL_)
    {
        COORDINATE pos;
        char *id;

        pos = src; id = NULL;

        if (scope == GLOBAL)
        {
            SYMBOL **params = NULL;

            ty1 = parse_declaration(ty, &id, &params, FALSE);

            if (params && id && isfunc(ty1) && (tok == '{' || istypename(tok, toksym) || (kind[tok] == STATIC && tok != TYPEDEF)))
            {
                if (sclass == TYPEDEF)
                {
                    apperror(RCERROR(ERROR_INVALID_USE_OF_TYPEDEF));
                    sclass = EXTERN;
                }

                if (ty1->u.fcn.oldstyle)
                    leave_scope();

                parse_function_definition(sclass, attr, id, ty1, params, pos);
                return;
            }
            else if (params)
            {
                exit_function_parameters(params);
            }
        }
        else
        {
            ty1 = parse_declaration(ty, &id, NULL, FALSE);
        }

        for (;;)
        {
            if (!has_prototype(ty1))
                apperror(RCWARNING2(ERROR_MISSING_PROTOTYPE));

            if (id == NULL)
            {
                apperror(RCERROR(ERROR_MISSING_IDENT));
            }
            else if (sclass == TYPEDEF)
            {
                SYMBOL *sym = lookup_symbol(id, identifiers);

                if (sym && sym->scope == scope)
                {
                    if (options.microsoft && is_same_type(sym->type, ty1, TRUE))  /* bugfix 04-09-08 */
                        apperror(RCWARNING2(ERROR_REDECLARATION), id);
                    else
                        apperror(RCERROR(ERROR_REDECLARATION), id);
                }

                sym = install_symbol(id, &identifiers, scope, scope < LOCAL ? PERM : funca);
                sym->type = ty1;
                sym->sclass = TYPEDEF;
                sym->src = pos;
            }
            else
            {
                (void)(*decl)(sclass, attr, id, ty1, &pos);
            }

            if (tok != ',')
                break;

            tok = gettok();

            pos = src; id = NULL;
            ty1 = parse_declaration(ty, &id, NULL, FALSE);
        }
    }
    else if (ty == NULL || !(isenum(ty) || isstruct(ty) &&
        (*unqual(ty)->u.sym->name < '1' ||
         *unqual(ty)->u.sym->name > '9')))
    {
        apperror(RCERROR(ERROR_EMPTY_DECLARATION));
    }

    /**/
    {
        static char stop[] = { CHAR_, STATIC, ID, 0 };
        follow(';', stop);
    }
}

/****************************************************************************
 *                                                                          *
 * Function: parse_declaration                                              *
 *                                                                          *
 * Purpose : type [ decl ( , decl )* ] ;                                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-07-02  Support for fastcall keyword added.                  *
 *                                                                          *
 ****************************************************************************/

static TYPE *parse_declaration(TYPE *basety, char **id, SYMBOL ***params, bool_t abstract)
{
    int calltype = 0;
    TYPE *ty;

    for (ty = parse_declaration1(id, params, abstract, &calltype);
         ty != NULL;
         ty = ty->type)
    {
        switch (ty->op)
        {
            case POINTER:
                basety = ptr(basety);
                break;

            case FUNCTION:
                if (calltype == 0)
                {
                    /* no explicit calling convention, use default */
                    calltype = (has_varargs(ty) || ty->u.fcn.oldstyle) ? CDECL_ : options.calltype;
                }
                if (calltype == STDCALL_ || calltype == FASTCALL_)
                {
                    if (!options.microsoft || has_varargs(ty) || (id && *id && strcmp(*id, "main") == 0))
                    {
                        apperror(RCWARNING1(ERROR_FUNCTION_MUST_BE_CDECL), *id, calltype);
                        calltype = CDECL_;
                    }
                }
                basety = func(basety, ty->u.fcn.prototype, ty->u.fcn.optparam, ty->u.fcn.oldstyle, calltype);
                calltype = 0;
                break;

            case ARRAY:
                basety = new_array(basety, ty->size, 0);
                basety->u.arr.e = ty->u.arr.e;  /* no u.arr.sym yet */
                break;

            case CONST_:
            case VOLATILE_:
            case RESTRICT_:
                basety = qual(ty->op, basety);
                break;

            default:
                assert(0);
                break;
        }
    }

    if (basety->size > 65535)  /* old limit 32767 */
        apperror(RCWARNING2(ERROR_MORE_THAN_X_BYTES), 65535, basety);

    if (calltype != 0)
        apperror(RCWARNING1(ERROR_ILLEGAL_USE_OF), calltype);

    return basety;
}

/****************************************************************************
 *                                                                          *
 * Function: parse_declaration1                                             *
 *                                                                          *
 * Purpose : ( id | * ( const | volatile | restrict )* |                    *
 *              '(' parse_declaration1 ')' ) ( (...) | [...] )*             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           03-08-14  Support for variable-length arrays added.            *
 *           04-07-02  Support for fastcall keyword added.                  *
 *           04-07-10  Bugfix: accept redundant calling convention specs.   *
 *           04-08-11  Accept arr[0] in Microsoft mode.                     *
 *           04-09-08  Bugfix: don't accept VLA with global scope.          *
 *           04-11-27  Bump count of seen VLA's.                            *
 *                                                                          *
 ****************************************************************************/

static TYPE *parse_declaration1(char **id, SYMBOL ***params, bool_t abstract, int *calltype)
{
    TYPE *ty = NULL;

    while (tok == CDECL_ || tok == STDCALL_ || tok == FASTCALL_)  /* if -> while 04-07-10 */
    {
        if (*calltype && *calltype != tok)
            apperror(RCWARNING1(ERROR_ILLEGAL_USE_OF), tok);
        else
            *calltype = tok;
        tok = gettok();
    }

    switch (tok)
    {
        case ID:
            if (id)
                *id = tokstr;
            else
                apperror(RCERROR(ERROR_EXTRANEOUS_IDENT), tokstr);
            tok = gettok();
            break;

        case '*':
            tok = gettok();
            if (tok == CONST_ || tok == VOLATILE_ || tok == RESTRICT_)
            {
                TYPE *ty1;

                ty1 = ty = typenode(tok, NULL);

                while ((tok = gettok()) == CONST_ || tok == VOLATILE_ || tok == RESTRICT_)
                    ty1 = typenode(tok, ty1);

                ty->type = parse_declaration1(id, params, abstract, calltype);
                ty = ty1;
            }
            else
            {
                ty = parse_declaration1(id, params, abstract, calltype);
            }

            ty = typenode(POINTER, ty);
            break;

        case '(':
            tok = gettok();
            if (abstract && (tok == REGISTER || istypename(tok, toksym) || tok == ')'))
            {
                SYMBOL **args;

                ty = typenode(FUNCTION, ty);

                enter_scope();
                if (scope > PARAM)
                    enter_scope();

                args = parse_function_parameters(ty);
                exit_function_parameters(args);
            }
            else
            {
                ty = parse_declaration1(id, params, abstract, calltype);
                expect(')');

                if (abstract && ty == NULL && (id == NULL || *id == NULL))
                    return typenode(FUNCTION, NULL);
            }
            break;

        case '[':
            break;

        default:
            return ty;
    }

    while (tok == '(' || tok == '[')
    {
        switch (tok)
        {
            case '(':
                tok = gettok();
                /**/
                {
                    SYMBOL **args;

                    ty = typenode(FUNCTION, ty);

                    enter_scope();
                    if (scope > PARAM)
                        enter_scope();

                    args = parse_function_parameters(ty);

                    if (params && *params == NULL)
                        *params = args;
                    else
                        exit_function_parameters(args);
                }
                break;

            case '[':
                tok = gettok();
                /**/
                {
                    TREE *e = 0;
                    int n = 0;

                    /* accept C99 idempotent type qualifiers (static added 01-09-30) */
                    while (abstract && id && (tok == CONST_ || tok == VOLATILE_ || tok == RESTRICT_ || tok == STATIC))
                    {
                        if (tok != STATIC)
                            ty = typenode(tok, ty);
                        tok = gettok();
                    }

                    /* accept C99 prototype VLA declaration */
                    if (abstract && tok == '*')
                    {
                        tok = gettok();
                        expect(']');
                        e = const_tree(1, inttype);  /* any expression so that e != 0 */
                    }
                    /* accept array size or C99 VLA size */
                    else if (kind[tok] == IF || kind[tok] == ID)
                    {
                        if (scope == GLOBAL)  /* 04-09-08 */
                        {
                            /* No VLA at global scope, *must* evaluate as constant expression */
                            n = intexpr(']', 1);
                            if (n == 0 && options.microsoft)  /* accept arr[0] */
                                ;
                            else if (n <= 0)
                            {
                                apperror(RCERROR(ERROR_ILLEGAL_ARRAY_SIZE), n);
                                n = 1;
                            }
                        }
                        else
                        {
                            e = expr(']');
                            if (e->op == CNST+I || e->op == CNST+U)
                            {
                                need_const++;
                                n = (int)cast(e, inttype)->u.v.i, e = 0;
                                need_const--;

                                if (n == 0 && options.microsoft)  /* accept arr[0] */
                                    ;
                                else if (n <= 0)
                                {
                                    apperror(RCERROR(ERROR_ILLEGAL_ARRAY_SIZE), n);
                                    n = 1;
                                }
                            }
                            else
                            {
                                /* remember VLA size */
                                e = tree_to_arena(cast(e, inttype), funca);
                            }
                        }
                    }
                    else
                    {
                        expect(']');
                    }

                    if (e)  /* VLA */
                    {
                        ty = typenode(ARRAY, ty);
                        ty->u.arr.e = e;  /* no u.arr.sym yet */
                        funcvla++;
                    }
                    else if (abstract && id && n == 0)  /* n == 0 added 00-12-06 */
                    {
                        ty = typenode(POINTER, ty);
                    }
                    else
                    {
                        ty = typenode(ARRAY, ty);
                        ty->size = n;
                    }
                }
                break;

            default:
                assert(0);
        }
    }

    return ty;
}

/****************************************************************************
 *                                                                          *
 * Function: typenode                                                       *
 *                                                                          *
 * Purpose : Allocate a temporary type node (only used by the parser).      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static TYPE *typenode(int op, TYPE *type)
{
    TYPE *ty;

    ty = memalloc(sizeof(*ty), STMT);
    memset(ty, 0, sizeof(*ty));

    ty->op = op;
    ty->type = type;

    return ty;
}

/****************************************************************************
 *                                                                          *
 * Function: declare_as_global                                              *
 *                                                                          *
 * Purpose : Called from declarator to declare a global.                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           03-01-13  Bugfix: attribute flags were sometimes lost.         *
 *           03-08-14  Support for variable-length arrays added.            *
 *           04-12-05  initializer() changed to global_initializer().       *
 *                                                                          *
 ****************************************************************************/

static SYMBOL *declare_as_global(int sclass, ATTR attr, char *id, TYPE *ty, COORDINATE *pos)
{
    SYMBOL *sym;

    if (attr.dllimport && sclass == 0)
        sclass = EXTERN;

    if (sclass == 0)
    {
        sclass = AUTO;
    }
    else if (sclass != EXTERN && sclass != STATIC)
    {
        apperror(RCERROR(ERROR_INVALID_STORAGE_CLASS), sclass, id, ty);
        sclass = AUTO;
    }

    sym = lookup_symbol(id, identifiers);
    if (sym != NULL && sym->scope == GLOBAL)
    {
        if (sym->sclass != TYPEDEF && is_same_type(ty, sym->type, TRUE))
            ty = composite_type(ty, sym->type);
        else
            apperror(RCERROR(ERROR_REDECLARATION_SEE_DECLTY), sym->name, &sym->src, ty, sym->type);

        if (!isfunc(ty) && sym->defined && tok == '=')
            apperror(RCERROR(ERROR_REDEFINITION_SEE_DEF), sym->name, &sym->src);

        if (sym->sclass == EXTERN && sclass == STATIC ||
            sym->sclass == STATIC && sclass == AUTO ||
            sym->sclass == AUTO   && sclass == STATIC)
            apperror(RCWARNING1(ERROR_INCONSISTENT_LINKAGE), sym->name, &sym->src);
    }

    if (attr.inlined && !isfunc(ty))
        apperror(RCERROR(ERROR_ILLEGAL_INLINE_USAGE));
    if (attr.naked && !isfunc(ty))
        apperror(RCERROR(ERROR_ILLEGAL_NAKED_USAGE));
    if (attr.dllimport && sclass == STATIC)
        apperror(RCERROR(ERROR_STATIC_IMPORT_EXPORT));

    if (sym == NULL || sym->scope != GLOBAL)
    {
        SYMBOL *sym2;

        sym2 = lookup_symbol(id, externals);
        if (sym2 != NULL)
        {
            if (sclass == STATIC || !is_same_type(ty, sym2->type, TRUE))
            {
                if (isfunc(ty))
                    apperror(RCERROR(ERROR_REDECLARATION_SEE_DECLTY), id, &sym2->src, ty, sym2->type);
                else
                    apperror(RCWARNING1(ERROR_DECLARATION_MISMATCH), id, &sym2->src);
            }

            sym = relocate_symbol(id, externals, globals);
            sym->sclass = sclass;
        }
        else
        {
            sym = install_symbol(id, &globals, GLOBAL, PERM);
            sym->sclass = sclass;

            /* set possible __stdcall or __fastcall */
            if (isfunc(ty)) sym->type = ty;

            sym->attr = attr;
            (*IR->defsymbol)(sym);
        }

        if (sym->sclass != STATIC)
        {
            static int nglobals;
            nglobals++;

            if (nglobals == 4096)  /* old limit 512 */
                apperror(RCWARNING2(ERROR_MORE_THAN_X_EXTERN_IDENT), 4095);
        }
    }
    else if (sym->sclass == EXTERN && !sym->attr.inlined)
    {
        sym->sclass = sclass;
    }
    else if (sclass == EXTERN && sym->attr.inlined)
    {
        sym->sclass = sclass;
    }

    sym->type = ty;
    sym->src = *pos;

    /* global VLA's are not allowed */
    while (isptr(ty))
        ty = ty->type;
    if (isvla(ty))
        apperror(RCERROR(ERROR_ILLEGAL_VLA_USAGE));

    /* update attributes */
    if (attr.inlined) sym->attr.inlined = TRUE;
    if (attr.naked) sym->attr.naked = TRUE;
    if (attr.dllimport) sym->attr.dllimport = TRUE;
    if (attr.dllexport) sym->attr.dllexport = TRUE;
    if (attr.noreturn) sym->attr.noreturn = TRUE;

    /* try to handle C99 inline semantics */
    if (isfunc(sym->type) && sym->attr.inlined && (sclass == EXTERN || !attr.inlined))
        sym->ref++;  /* needs external linkage */

    if (tok == '=' && isfunc(sym->type))
    {
        apperror(RCERROR(ERROR_ILLEGAL_INIT_FOR), sym->name);
        tok = gettok();
        global_initializer(sym, sym->type);
    }
    else if (tok == '=')
    {
        init_global(sym, FALSE);

        if (options.dbglevel > 1 && IR->dbgsym)
        {
            (*IR->dbgsym)(sym);
            set_segment(sym->u.seg);
        }
    }
    else if (sym->sclass == STATIC && !isfunc(sym->type) && !isvla(sym->type) && sym->type->size == 0)
        apperror(RCERROR(ERROR_UNDEFINED_SIZE_FOR), sym->name, sym->type);

    return sym;
}

/****************************************************************************
 *                                                                          *
 * Function: declare_as_local                                               *
 *                                                                          *
 * Purpose : Called from declarator to declare a local.                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           03-08-14  Support for variable-length arrays added.            *
 *           04-12-05  Accept non-constant initializers.                    *
 *                                                                          *
 ****************************************************************************/

static SYMBOL *declare_as_local(int sclass, ATTR attr, char *id, TYPE *ty, COORDINATE *pos)
{
    SYMBOL *sym;
    SYMBOL *sym2;

    if (sclass == 0)
    {
        sclass = isfunc(ty) ? EXTERN : AUTO;
    }
    else if (isfunc(ty) && sclass != EXTERN)
    {
        apperror(RCERROR(ERROR_INVALID_STORAGE_CLASS), sclass, id, ty);
        sclass = EXTERN;
    }
    else if (sclass == REGISTER && (isvolatile(ty) || isstruct(ty) || isarray(ty)))
    {
        apperror(RCWARNING2(ERROR_REGISTER_DECL_IGNORED), id);
        sclass = AUTO;
    }

    if (attr.inlined && !isfunc(ty))
        apperror(RCERROR(ERROR_ILLEGAL_INLINE_USAGE));
    if (attr.naked && !isfunc(ty))
        apperror(RCERROR(ERROR_ILLEGAL_NAKED_USAGE));
    if (attr.dllimport)
        apperror(RCERROR(ERROR_ILLEGAL_DLLIMPORT_USAGE));
    if (attr.dllexport)
        apperror(RCERROR(ERROR_ILLEGAL_DLLEXPORT_USAGE));

    sym2 = lookup_symbol(id, identifiers);
    if (sym2 && sym2->scope >= scope ||
        sym2 && sym2->scope == PARAM && scope == LOCAL)
    {
        if (sclass == EXTERN && sym2->sclass == EXTERN && is_same_type(sym2->type, ty, TRUE))
            ty = composite_type(ty, sym2->type);
        else
            apperror(RCERROR(ERROR_REDECLARATION_SEE_DECLTY), sym2->name, &sym2->src, ty, sym2->type);
    }

    assert(scope >= LOCAL);
    sym = install_symbol(id, &identifiers, scope, (sclass == STATIC || sclass == EXTERN) ? PERM : funca);
    sym->type = ty;
    sym->sclass = sclass;
    sym->src = *pos;

    /* external VLA's are not allowed */
    // ? static pointer to VLA *is* allowed
    while (isptr(ty))
        ty = ty->type;
    if (isvla(ty) && (sclass == EXTERN || sclass == STATIC))
    {
        apperror(RCERROR(ERROR_INVALID_STORAGE_CLASS), sclass, sym->name, sym->type);
        sym->type->u.arr.e = 0, sym->type->size = 1;  /* no u.arr.sym yet */
    }

    switch (sclass)
    {
        case EXTERN:
            sym2 = lookup_symbol(id, globals);
            if (sym2 == NULL || sym2->sclass == TYPEDEF || sym2->sclass == ENUM)
            {
                sym2 = lookup_symbol(id, externals);
                if (sym2 == NULL)
                {
                    sym2 = install_symbol(sym->name, &externals, GLOBAL, PERM);
                    sym2->type = sym->type;
                    sym2->sclass = EXTERN;
                    sym2->src = src;
                    (*IR->defsymbol)(sym2);
                }
            }

            if (!is_same_type(sym->type, sym2->type, TRUE))
                apperror(RCWARNING1(ERROR_DECLARATION_MISMATCH), sym2->name, &sym2->src);

            sym->u.alias = sym2;
            break;

        case STATIC:
            (*IR->defsymbol)(sym);
            init_global(sym, FALSE);
            if (!sym->defined)
            {
                if (sym->type->size > 0)
                {
                    define_global(sym, BSS);
                    (*IR->space)(sym->type->size);
                }
                else
                {
                    apperror(RCERROR(ERROR_UNDEFINED_SIZE_FOR), sym->name, sym->type);
                }
            }
            sym->defined = TRUE;
            break;

        case REGISTER:
            registers = listappend(sym, registers);
            regcount++;
            sym->defined = TRUE;
            break;

        case AUTO:
            autos = listappend(sym, autos);
            sym->defined = TRUE;
            if (isarray(sym->type))
                sym->addressed = TRUE;
            break;

        default:
            assert(0);
    }

    if (isvla(ty))  /* bugfix 04-03-26: use ty, not sym->type */
    {
        static SYMBOL *vlafunc;

        if (vlafunc == 0)
        {
            /* use _alloca() to implement VLA's */
            vlafunc = make_symbol(EXTERN, "_alloca", func_type(voidptype, unsignedtype, NULL));
            vlafunc->defined = 0;
        }

        /* emit code to allocate space for the variable-length array */
        new_execution_point(NULL);
        new_forest(assignment(sym, vcall(vlafunc, NULL, vlasize_tree(ty, 1), NULL)), 0, 0);
    }

    if (tok == '=')
    {
        TREE *e;

        if (sclass == EXTERN)
            apperror(RCERROR(ERROR_ILLEGAL_INIT_OF_EXTERN), id);
        if (isvla(sym->type))
            apperror(RCERROR(ERROR_ILLEGAL_INIT_OF_VLA), id);

        tok = gettok();

        new_execution_point(NULL);

        if (isscalar(sym->type) || isstruct(sym->type) && tok != '{')
        {
            if (tok == '{')
            {
                tok = gettok();
                e = expr1(0);
                expect('}');
            }
            else
            {
                e = expr1(0);
            }

            new_forest(root(assignment(sym, e)), 0, 0);
            sym->ref = 1;
        }
        else
        {
            /* rewritten to accept non-constant initializers (04-12-05) */
            new_forest(root(local_initializer(sym, sym->type)), 0, 0);
            sym->ref = 1;
        }
    }

    if (!isfunc(sym->type) && !isvla(sym->type) && sym->defined && sym->type->size <= 0)
        apperror(RCERROR(ERROR_UNDEFINED_SIZE_FOR), id, sym->type);

    return sym;
}

/****************************************************************************
 *                                                                          *
 * Function: declare_as_param                                               *
 *                                                                          *
 * Purpose : Called from declarator to declare a parameter.                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           03-08-14  Support for variable-length arrays added.            *
 *           03-10-28  Support for optional arguments added.                *
 *           04-10-06  Bugfix: optional argument can't be in SYMBOL union!  *
 *                                                                          *
 ****************************************************************************/

static SYMBOL *declare_as_param(int sclass, ATTR attr, char *id, TYPE *ty, COORDINATE *pos)
{
    SYMBOL *sym;

    if (isfunc(ty))
        ty = ptr(ty);
    else if (isarray(ty))
        ty = array_to_ptr(ty);

    if (sclass == 0)
    {
        sclass = AUTO;
    }
    else if (sclass != REGISTER)
    {
        apperror(RCERROR(ERROR_INVALID_STORAGE_CLASS),
            sclass, stringf(id ? "%s" : "parameter", id), ty);
        sclass = AUTO;
    }
    else if (isvolatile(ty) || isstruct(ty))
    {
        apperror(RCWARNING2(ERROR_REGISTER_DECL_IGNORED), stringf(id ? "%s" : "parameter", id));
        sclass = AUTO;
    }

    if (attr.inlined && !isfunc(ty))
        apperror(RCERROR(ERROR_ILLEGAL_INLINE_USAGE));
    if (attr.naked && !isfunc(ty))
        apperror(RCERROR(ERROR_ILLEGAL_NAKED_USAGE));
    if (attr.dllimport)
        apperror(RCWARNING1(ERROR_ILLEGAL_DLLIMPORT_USAGE));
    if (attr.dllexport)
        apperror(RCERROR(ERROR_ILLEGAL_DLLEXPORT_USAGE));

    sym = lookup_symbol(id, identifiers);

    if (sym && sym->scope == scope)
        apperror(RCERROR(ERROR_DUPLICATE_DECLARATION), id, &sym->src);
    else
        sym = install_symbol(id, &identifiers, scope, funca);

    sym->sclass = sclass;
    sym->src = *pos;
    sym->type = ty;
    sym->defined = TRUE;
    sym->e = NULL;

    /* arrays decay to pointers, even VLA's */
    if (isptr(sym->type) && isvla(sym->type->type))
    {
        /* assign size to temporary, but don't emit code */
        (void)vlasize_tree(sym->type->type, 1);
    }

    if (tok == '=')
    {
        if (options.extensions)
        {
            /* accept optional argument value */
            tok = gettok();
            optparam++;
            sym->e = expr_in_arena(expr1, 0, PERM);
            optparam--;
        }
        else
        {
            apperror(RCERROR(ERROR_ILLEGAL_INIT_OF_PARAM), id);
            tok = gettok();
            (void)expr1(0);
        }
    }

    return sym;
}

/****************************************************************************
 *                                                                          *
 * Function: init_global                                                    *
 *                                                                          *
 * Purpose : Initialize a global or static sym.                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-12-05  initializer() changed to global_initializer().       *
 *                                                                          *
 ****************************************************************************/

static void init_global(SYMBOL *sym, bool_t force)
{
    TYPE *ty;

    if (tok == '=' || force)
    {
        if (tok == '=' && sym->attr.dllimport)
            apperror(RCERROR(ERROR_ILLEGAL_DLLIMPORT_USAGE));

        if (sym->sclass == STATIC)
        {
            for (ty = sym->type; isarray(ty); ty = ty->type)
                ;

            define_global(sym, isconst(ty) ? LIT : DATA);
        }
        else
        {
            /* 01-08-20: works better for "const GUID xxx = {...} */
            /* define_global(sym, isconst(sym->type) ? LIT : DATA); */
            define_global(sym, DATA);
        }

        if (tok == '=')
            tok = gettok();

        ty = global_initializer(sym, sym->type);

        if (isarray(sym->type) && sym->type->size == 0)
            sym->type = ty;

        if (sym->sclass == EXTERN)
            sym->sclass = AUTO;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: parse_compound_statement                                       *
 *                                                                          *
 * Purpose : { [ (decl | statement)* ] }                                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           03-10-20  No warning when calling a 'noreturn' function.       *
 *                                                                          *
 ****************************************************************************/

void parse_compound_statement(int loop, SWTCH *swp, SEH *seh, int lev)
{
    CODE *cp;
    int nregs;

    new_forest(NULL, 0, 0);
    cp = new_code(CODE_BLOCKBEG);

    enter_scope();
    assert(scope >= LOCAL);

#ifdef PROF
    if (scope == LOCAL && events.entry)
        apply(events.entry, funcsym, NULL);
#endif

    new_execution_point(NULL);

    expect('{');
    autos = registers = NULL;

    if (scope == LOCAL && IR->wants_callb && isstruct(func_return(funcsym->type)))
    {
        retv = make_ident(AUTO, ptr(unqual(func_return(funcsym->type))), scope);
        retv->defined = TRUE;
        retv->ref = 1;
        registers = listappend(retv, registers);
    }

    for (;;)
    {
        /* C99 supports mixed declarations and code (like C++) */
        if (kind[tok] == CHAR_ || kind[tok] == STATIC || istypename(tok, toksym) && getchr() != ':')
        {
            declarator(declare_as_local);
        }
        else if (kind[tok] == IF || kind[tok] == ID)
        {
            LIST *a = autos, *r = registers;
            parse_statement(loop, swp, seh, lev);
            autos = a; registers = r;
        }
        else
            break;  /* } for example */
    }

    new_forest(NULL, 0, 0);

    /**/
    {
        SYMBOL **a = listvector(&autos, STMT);
        int i;

        nregs = listelems(registers);
        for (i = 0; a[i]; i++)
            registers = listappend(a[i], registers);
        cp->u.block.locals = listvector(&registers, funca);
    }

#ifdef PROF
    if (events.blockentry)
        apply(events.blockentry, cp->u.block.locals, NULL);
#endif

    for_each_symbol(identifiers, scope, check_references, NULL);

    /**/
    {
        SYMBOL *sym;
        int i, j;

        /* sort locals - shouldn't affect register assignment, but *might* affect caching */
        for (i = nregs; (sym = cp->u.block.locals[i]) != NULL; i++)
        {
            for (j = i; j > nregs && cp->u.block.locals[j-1]->ref < sym->ref; j--)
                cp->u.block.locals[j] = cp->u.block.locals[j-1];
            cp->u.block.locals[j] = sym;
        }
    }

    if (scope == LOCAL)
    {
        CODE *cp;

        for (cp = codelist; cp->kind < CODE_LABEL; cp = cp->prev)
            ;

        if (cp->kind != CODE_JUMP && cp->kind != CODE_ASM)
        {
            int warn = 0;

            if (cp->kind == CODE_GEN)
            {
                NODE *p;

                /* check if calling a __declspec(noreturn) function */
                for (p = cp->u.forest; p != NULL; p = p->link)
                {
                    if (generic(p->op) == CALL &&
                        p->kids[0] &&
                        p->kids[0]->syms[0] &&
                        p->kids[0]->syms[0]->attr.noreturn)
                    {
                        warn++;
                        break;
                    }
                }
            }

            if (func_return(funcsym->type) != voidtype)
            {
                /*
                 * C99 says "If the } that terminates a function is reached,
                 * and the value of the function call is used by the caller,
                 * the behavior is undefined".
                 */
                if (warn++ == 0)
                    apperror(RCWARNING1(ERROR_MISSING_RETURN_VALUE));
                return_value(cnst_tree(inttype, (intmax_t)0));
            }
            else
            {
                return_value(NULL);
            }
        }
    }

#ifdef PROF
    if (events.blockexit)
        apply(events.blockexit, cp->u.block.locals, NULL);
#endif

    if (sehinfo.codesym)
    {
        seh->sym = sehinfo.codesym;
        sehinfo.codesym = NULL;
    }

    if (seh && seh->type == SEH_TRY && seh->level == lev)
        new_label(seh->label);

    cp->u.block.level = scope;
    cp->u.block.identifiers = identifiers;
    cp->u.block.types = types;
    cp->u.block.seh.type = (seh && seh->level == lev) ? seh->type : SEH_NOTHING;
    cp->u.block.seh.data = (seh && seh->level == lev) ? seh : NULL;
    new_code(CODE_BLOCKEND)->u.begin = cp;

    if (reachable_code(CODE_GEN))
        new_execution_point(NULL);

    if (scope > LOCAL)
    {
        leave_scope();
        expect('}');
    }
}

/****************************************************************************
 *                                                                          *
 * Function: parse_for_declaration                                          *
 *                                                                          *
 * Purpose : Parse a for statement declaration.                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

CODE *parse_for_declaration(void)
{
    CODE *cp;

    new_forest(NULL, 0, 0);
    cp = new_code(CODE_BLOCKBEG);

    enter_scope();
    assert(scope > LOCAL);

    autos = registers = NULL;
    declarator(declare_as_local);

    /**/
    {
        SYMBOL **a = listvector(&autos, STMT);
        int i;

        for (i = 0; a[i]; i++)
            registers = listappend(a[i], registers);
        cp->u.block.locals = listvector(&registers, funca);
    }

    return cp;
}

/****************************************************************************
 *                                                                          *
 * Function: close_for_scope                                                *
 *                                                                          *
 * Purpose : End scope on a for statement declaration.                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void close_for_scope(CODE *cp)
{
    new_forest(NULL, 0, 0);

    for_each_symbol(identifiers, scope, check_references, NULL);

    /**/
    {
        SYMBOL *sym;
        int i, j;

        /* sort locals - shouldn't affect register assignment, but *might* affect caching */
        for (i = 0; (sym = cp->u.block.locals[i]) != NULL; i++)
        {
            /* really should use nregs, as above */
            for (j = i; j > 0 && cp->u.block.locals[j-1]->ref < sym->ref; j--)
                cp->u.block.locals[j] = cp->u.block.locals[j-1];
            cp->u.block.locals[j] = sym;
        }
    }

    cp->u.block.level = scope;
    cp->u.block.identifiers = identifiers;
    cp->u.block.types = types;
    cp->u.block.seh.type = SEH_NOTHING;
    cp->u.block.seh.data = NULL;
    new_code(CODE_BLOCKEND)->u.begin = cp;

    new_execution_point(NULL);

    if (scope > LOCAL)
        leave_scope();
}

/****************************************************************************
 *                                                                          *
 * Function: parse_function_definition                                      *
 *                                                                          *
 * Purpose : ... ( ... ) decl* compound                                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-01-21  Bugfix: must set sehinfo.infosym to NULL on exit.    *
 *           04-07-07  Added check for cbstring option (drivers).           *
 *           04-09-05  Bugfix: ignore inline specifier from prototype.      *
 *           04-11-27  Clear count of seen VLA's.                           *
 *           05-01-02  Added support for small structure optimization.      *
 *                                                                          *
 ****************************************************************************/

static void parse_function_definition(int sclass, ATTR attr, char *id, TYPE *ty, SYMBOL *params[], COORDINATE pt)
{
    SYMBOL **callee, **caller, *sym;
    TYPE *rty = func_return(ty);
    int i, n;

    if (attr.dllimport)
        apperror(RCERROR(ERROR_ILLEGAL_DLLIMPORT_USAGE));
    if (attr.dllexport && sclass == STATIC)
        apperror(RCERROR(ERROR_STATIC_IMPORT_EXPORT));

    /* check the return type */
    if (isstruct(rty) && rty->size == 0)
        apperror(RCERROR(ERROR_ILLEGAL_USE_OF_TYPE), rty);

    for (n = 0; params[n]; n++)
        ;
    if (n > 0 && params[n-1]->name == NULL)
        params[--n] = NULL;
    if (n > 127)  /* old limit 31 */
        apperror(RCWARNING2(ERROR_MORE_THAN_X_FUNC_PARAMS), 127, id);

    if (ty->u.fcn.oldstyle)
    {
        apperror(RCWARNING2(ERROR_OLD_STYLE_FUNC_DEF), id);

        caller = params;
        callee = memarray(n+1, sizeof(*callee), funca);
        memcpy(callee, caller, (n+1) * sizeof(*callee));

        enter_scope();
        assert(scope == PARAM);

        while (kind[tok] == STATIC || istypename(tok, toksym))
            declarator(declare_as_param);

        for_each_symbol(identifiers, PARAM, fixup_old_style_parameter, callee);

        for (i = 0; (sym = callee[i]) != NULL; i++)
        {
            if (!sym->defined)
                callee[i] = declare_as_param(0, noattr, sym->name, inttype, &sym->src);

            *caller[i] = *sym;
            caller[i]->sclass = AUTO;
            caller[i]->type = promote_type(sym->type);
        }

        sym = lookup_symbol(id, identifiers);
        if (sym && sym->scope == GLOBAL && isfunc(sym->type) && sym->type->u.fcn.prototype)
        {
            TYPE **prototype = sym->type->u.fcn.prototype;

            for (i = 0; caller[i] && prototype[i]; i++)
            {
                TYPE *ty = unqual(prototype[i]);

                if (is_same_type(isenum(ty) ? ty->type : ty, unqual(caller[i]->type), TRUE) == 0)
                    break;
                else if (isenum(ty) && !isenum(unqual(caller[i]->type)))
                    apperror(RCWARNING1(ERROR_NON_PORTABLE_USAGE), prototype[i], caller[i]->type);
            }

            if (prototype[i] || caller[i])
                apperror(RCERROR(ERROR_CONFLICTING_FUNC_PARAM), id);
        }
        else
        {
            TYPE **prototype = memarray(n+1, sizeof(*prototype), PERM);

            /* accept main() without warning */
            if (n != 0 || strcmp(id, "main") != 0)
                apperror(RCWARNING1(ERROR_MISSING_PROTOTYPE_FOR), id);

            for (i = 0; i < n; i++)
                prototype[i] = caller[i]->type;
            prototype[i] = NULL;

            ty = func(rty, prototype, NULL, TRUE, CDECL_);
        }
    }
    else  /* new style def */
    {
        callee = params;
        caller = memarray(n+1, sizeof(*caller), funca);

        for (i = 0; (sym = callee[i]) != NULL && sym->name; i++)
        {
            caller[i] = memalloc(sizeof(*caller[i]), funca);
            *caller[i] = *sym;

            if (isint(sym->type))
                caller[i]->type = promote_type(sym->type);

            caller[i]->sclass = AUTO;

            if ('1' <= *sym->name && *sym->name <= '9')
                apperror(RCERROR(ERROR_MISSING_PARAM_NAME), i+1, id);
        }
        caller[i] = NULL;
    }

    for (i = 0; (sym = callee[i]) != NULL; i++)
    {
        if (sym->type->size == 0)
        {
            apperror(RCERROR(ERROR_UNDEFINED_SIZE_FOR_PARAM), sym->name, sym->type);
            caller[i]->type = sym->type = inttype;
        }
    }

    /* check that the entry point has a correct signature */
    if (sclass != STATIC && strcmp(id, "main") == 0)
    {
        if (ty->u.fcn.oldstyle)
        {
            apperror(RCWARNING2(ERROR_NON_ISO_FUNC_DEFINITION), id);
        }
        else if (!(rty == inttype && (n == 0 && callee[0] == NULL ||
            n == 2 && callee[0]->type == inttype && isptr(callee[1]->type) &&
            callee[1]->type->type == charptype && !has_varargs(ty))))
        {
            apperror(RCWARNING2(ERROR_NON_ISO_DEFINITION), typestring(ty, id));
        }
        attr.inlined = FALSE;
    }

    sym = lookup_symbol(id, identifiers);
    if (sym && isfunc(sym->type) && sym->defined)
        apperror(RCERROR(ERROR_REDEFINITION_SEE_DEF), sym->name, &sym->src);

    funcsym = declare_as_global(sclass, attr, id, ty, &pt);
    funcsym->u.fcn.label = make_label(1);
    funcsym->u.fcn.callee = callee;
    funcsym->u.fcn.pt = src;
    funcsym->defined = TRUE;
    funcvla = 0;
    /*
     * because of *severe* memory allocation problems, we can only handle the
     * inline specifier on the function definition, *not* the prototype!
     */
    /* if (attr.inlined) funcsym->attr.inlined = TRUE; */
    funcsym->attr.inlined = attr.inlined;

#ifdef XREF
    if (options.xreflevel > 0)
        use_symbol(funcsym, funcsym->src);
#endif

    if (options.printproto)
        print_prototype(funcsym, funcsym->u.fcn.callee);

#ifdef PROF
    if (ncalled >= 0)
        ncalled = findfunc(funcsym->name, pt.file);
#endif

    labels   = new_symbol_table(NULL, LABELS);
    stmtlabs = new_symbol_table(NULL, LABELS);
    refinc = 1.0;
    regcount = 0;
    codelist = &codehead;
    codelist->next = NULL;

    if (!IR->wants_callb && isstruct(rty) && !optimized_struct_type(rty))
        retv = make_ident(AUTO, ptr(unqual(rty)), PARAM);

    parse_compound_statement(0, NULL, 0, 0);

    new_label(funcsym->u.fcn.label);

#ifdef PROF
    if (events.exit)
        apply(events.exit, funcsym, NULL);
#endif

    new_forest(NULL, 0, 0);

    leave_scope();
    assert(scope == PARAM);

    for_each_symbol(identifiers, scope, check_references, NULL);

    if (!IR->wants_callb && isstruct(rty) && !optimized_struct_type(rty))
    {
        /* insert hidden return value location for structs */
        SYMBOL **a;

        a = memarray(n+2, sizeof(*a), funca);
        a[0] = retv;
        memcpy(&a[1], callee, (n+1) * sizeof(*callee));
        callee = a;

        a = memarray(n+2, sizeof(*a), funca);
        a[0] = memalloc(sizeof(*a[0]), funca);
        *a[0] = *retv;
        memcpy(&a[1], caller, (n+1) * sizeof(*callee));
        caller = a;
    }

    if (!IR->wants_argb)
    {
        /* annotate structure arguments */
        for (i = 0; caller[i]; i++)
        {
            if (isstruct(caller[i]->type))
            {
                caller[i]->type = ptr(caller[i]->type);
                callee[i]->type = ptr(callee[i]->type);
                caller[i]->structarg = callee[i]->structarg = TRUE;
            }
        }
    }

    if (options.dbglevel > 1)  /* turn off register allocations */
        for (i = 0; callee[i]; i++) callee[i]->sclass = AUTO;

    if (funcsym->attr.inlined)
    {
        FCNENTRY *fcn;

        fcn = memalloc(sizeof(*fcn), PERM);
        memset(fcn, 0, sizeof(*fcn));

        fcn->func = funcsym;
        fcn->caller = caller;
        fcn->callee = callee;
        fcn->codelist = codehead.next;
        fcn->refsyms = refsyms; refsyms = NULL;

        functions = listappend(fcn, functions);
    }
    else
    {
        if (funcsym->sclass != STATIC)
            (*IR->export)(funcsym);

        if (options.dbglevel > 1 && IR->dbgsym)
        {
            set_segment(TEXT);
            (*IR->dbgsym)(funcsym);
        }
        set_segment(TEXT);

        (*IR->function)(funcsym, caller, callee, funcsym->u.fcn.ncalls);

        if (options.dbglevel > 1 && IR->dbgfend)
            (*IR->dbgfend)(funcsym, lineno);

        if (options.cbstring)  /* emit strings to the current code section */
            for_each_symbol(constants, CONSTANTS, finalize_const, NULL);
    }

#ifdef XREF
    if (options.xreflevel > 0 && IR->dbgfend)
        (*IR->dbgfend)(funcsym, lineno);
#endif

    for_each_symbol(stmtlabs, LABELS, check_labels, NULL);

    leave_scope();
    expect('}');

    labels = stmtlabs = NULL;
    retv  = NULL;
    funcsym = NULL;
    sehinfo.infosym = NULL;  /* Bugfix 04-01-21 */
}

/****************************************************************************
 *                                                                          *
 * Function: parse_function_parameters                                      *
 *                                                                          *
 * Purpose : [ id ( , id )* | type decl ( , type decl )* ]                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           03-10-28  Support for optional arguments added.                *
 *           04-10-06  Bugfix: optional argument can't be in SYMBOL union!  *
 *                                                                          *
 ****************************************************************************/

static SYMBOL **parse_function_parameters(TYPE *fty)
{
    LIST *list = NULL;
    SYMBOL **params;

    if (kind[tok] == STATIC || istypename(tok, toksym))
    {
        /* parse new-style parameter list */
        TYPE *ty1 = NULL;
        int n = 0;

        for (;;)
        {
            int sclass = 0;
            char *id = NULL;
            TYPE *ty;

            if (ty1 && tok == ELLIPSIS)
            {
                static SYMBOL sentinel;

                if (sentinel.type == NULL)
                {
                    sentinel.type = voidtype;
                    sentinel.defined = TRUE;
                }

                if (ty1 == voidtype)
                    apperror(RCERROR(ERROR_ILLEGAL_FORMAL_PARAM_TYPE));

                list = listappend(&sentinel, list);
                tok = gettok();
                break;
            }

            if (!istypename(tok, toksym) && tok != REGISTER)
                apperror(RCERROR(ERROR_MISSING_PARAM_TYPE), tokstr);

            n++;

            ty = parse_declaration(parse_type(&sclass, NULL), &id, NULL, TRUE);

            if (ty == voidtype && (ty1 || id) || ty1 == voidtype)
                apperror(RCERROR(ERROR_ILLEGAL_FORMAL_PARAM_TYPE));

            if (id == NULL)
                id = stringd(n);

            if (ty != voidtype)
                list = listappend(declare_as_param(sclass, noattr, id, ty, &src), list);

            if (!has_prototype(ty))
                apperror(RCWARNING2(ERROR_MISSING_PROTOTYPE));

            if (ty1 == NULL)
                ty1 = ty;

            if (tok != ',')
                break;

            tok = gettok();
        }

        if (options.extensions)
        {
            bool_t hasopt = FALSE;

            fty->u.fcn.prototype = memarray(listelems(list)+1, sizeof(TYPE *), PERM);
            fty->u.fcn.optparam = memarray(listelems(list)+1, sizeof(TREE *), PERM);
            params = listvector(&list, funca);
            for (n = 0; params[n]; n++)
            {
                fty->u.fcn.prototype[n] = params[n]->type;
                fty->u.fcn.optparam[n] = params[n]->e;
                if (params[n]->e != NULL)
                    hasopt = TRUE;
                else if (hasopt)
                    apperror(RCERROR(ERROR_MISSING_OPT_PARAM), params[n]->name);
            }
            fty->u.fcn.prototype[n] = NULL;
            fty->u.fcn.optparam[n] = NULL;
            fty->u.fcn.oldstyle = FALSE;
        }
        else
        {
            fty->u.fcn.prototype = memarray(listelems(list)+1, sizeof(TYPE *), PERM);
            fty->u.fcn.optparam = NULL;
            params = listvector(&list, funca);
            for (n = 0; params[n]; n++)
                fty->u.fcn.prototype[n] = params[n]->type;
            fty->u.fcn.prototype[n] = NULL;
            fty->u.fcn.oldstyle = FALSE;
        }
    }
    else
    {
        /* parse old-style parameter list */
        if (tok == ID)
        {
            for (;;)
            {
                SYMBOL *sym;

                if (tok != ID)
                {
                    apperror(RCERROR(ERROR_EXPECTING_IDENT));
                    break;
                }

                sym = declare_as_param(0, noattr, tokstr, inttype, &src);
                sym->defined = FALSE;
                list = listappend(sym, list);

                tok = gettok();
                if (tok != ',')
                    break;

                tok = gettok();
            }
        }
        params = listvector(&list, funca);
        fty->u.fcn.prototype = NULL;
        fty->u.fcn.optparam = NULL;
        fty->u.fcn.oldstyle = TRUE;
    }

    if (tok != ')')
    {
        static char stop[] = { CHAR_, STATIC, IF, ')', 0 };
        expect(')');
        skipto('{', stop);
    }

    if (tok == ')')
        tok = gettok();

    return params;
}

/****************************************************************************
 *                                                                          *
 * Function: exit_function_parameters                                       *
 *                                                                          *
 * Purpose : Close a scope in a parameter list.                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void exit_function_parameters(SYMBOL *params[])
{
    assert(params);

    if (params[0] && !params[0]->defined)
        apperror(RCERROR(ERROR_OLD_STYLE_PARAM_LIST));

    if (scope > PARAM)
        leave_scope();

    leave_scope();
}

/****************************************************************************
 *                                                                          *
 * Function: fixup_old_style_parameter                                      *
 *                                                                          *
 * Purpose : Check that sym is an old-style parameter, and patch callee.    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void fixup_old_style_parameter(SYMBOL *sym, void *cl)
{
    SYMBOL **callee = cl;
    int i;

    for (i = 0; callee[i]; i++)
    {
        if (sym->name == callee[i]->name)
        {
            callee[i] = sym;
            return;
        }
    }

    apperror(RCERROR(ERROR_DECLARED_PARAM_MISSING), sym->name);
}

/****************************************************************************
 *                                                                          *
 * Function: parse_enum_declaration                                         *
 *                                                                          *
 * Purpose : enum [ id ] [ { id [ = cexpr ] ( , id [ = cexpr ] )* } ]       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static TYPE *parse_enum_declaration(void)
{
    char *tag;
    TYPE *ty;
    SYMBOL *sym;
    COORDINATE pos;

    tok = gettok();
    pos = src;

    if (tok == ID)
    {
        tag = tokstr;
        tok = gettok();
    }
    else
    {
        tag = "";
    }

    if (tok == '{')
    {
        static char stop[] = { IF, 0 };
        int n = 0;
        long k = -1;
        LIST *idlist = 0;

        ty = new_struct(ENUM, tag);

        tok = gettok();

        if (tok != ID)
            apperror(RCERROR(ERROR_EXPECTING_ENUM_IDENT));

        while (tok == ID)
        {
            char *id = tokstr;
            COORDINATE s;

            if (toksym && toksym->scope == scope)
                apperror(RCERROR(ERROR_REDECLARATION_SEE_DECL), tokstr, &toksym->src);

            s = src;
            tok = gettok();
            if (tok == '=')
            {
                tok = gettok();
                k = intexpr(0, 0);
            }
            else
            {
                if (k == inttype->u.sym->u.limits.max.i)
                    apperror(RCERROR(ERROR_OVERFLOW_IN_ENUM_CONST), id);
                k++;
            }

            sym = install_symbol(id, &identifiers, scope, (scope < LOCAL) ? PERM : funca);
            sym->src = s;
            sym->type = ty;
            sym->sclass = ENUM;
            sym->u.value = k;
            idlist = listappend(sym, idlist);
            n++;

            if (n == 1024)  /* old limit 128 */
                apperror(RCWARNING2(ERROR_MORE_THAN_X_ENUM_CONST), 1023, ty);

            if (tok != ',')
                break;

            tok = gettok();

            /* C99 accepts trailing comma */
        }

        follow('}', stop);

        ty->type = inttype;
        ty->size = ty->type->size;
        ty->align = ty->type->align;
        ty->u.sym->u.idlist = listvector(&idlist, PERM);
        ty->u.sym->defined = TRUE;
    }
    else if ((sym = lookup_symbol(tag, types)) != NULL && sym->type->op == ENUM)
    {
        ty = sym->type;

        if (tok == ';')
            apperror(RCERROR(ERROR_EMPTY_DECLARATION));
    }
    else
    {
        apperror(RCERROR(ERROR_UNKNOWN_ENUMERATION), tag);
        ty = new_struct(ENUM, tag);
        ty->type = inttype;
    }

#ifdef XREF
    if (options.xreflevel > 1 && *tag)
        use_symbol(sym, pos);
#endif

    return ty;
}

/****************************************************************************
 *                                                                          *
 * Function: parse_struct_declaration                                       *
 *                                                                          *
 * Purpose : ( struct | union )  ( [ id ] { ( field; )+ } | id )            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-04-21  Added support for __declspec(align(n)).              *
 *                                                                          *
 ****************************************************************************/

static TYPE *parse_struct_declaration(int op)
{
    int align = -1;
    char *tag;
    TYPE *ty;
    SYMBOL *sym;
    COORDINATE pos;

    tok = gettok();
    pos = src;

    /* parse __declspec(align(n)) */
    if (tok == DECLSPEC)
        parse_spec_declaration(NULL, &align);

    if (tok == ID)
    {
        tag = tokstr;
        tok = gettok();
    }
    else
    {
        tag = "";
    }

    if (tok == '{')
    {
        static char stop[] = { IF, ',', 0 };

        ty = new_struct(op, tag);
        ty->u.sym->src = pos;
        ty->u.sym->defined = TRUE;

        tok = gettok();

        if (istypename(tok, toksym))
            parse_struct_fields(ty);
        else
            apperror(RCERROR(ERROR_INVALID_FIELD_DECL), op);

        /* enforce any user alignment */
        if (align != -1)
            ty->align = align;

        follow('}', stop);
    }
    else if (*tag && (sym = lookup_symbol(tag, types)) != NULL && sym->type->op == op)
    {
        ty = sym->type;

        if (tok == ';' && sym->scope < scope)
            ty = new_struct(op, tag);
    }
    else
    {
        if (*tag == 0)
            apperror(RCERROR(ERROR_MISSING_TAG), op);

        ty = new_struct(op, tag);
    }

#ifdef XREF
    if (options.xreflevel > 1 && *tag)
        use_symbol(ty->u.sym, pos);
#endif

    return ty;
}

/****************************************************************************
 *                                                                          *
 * Function: parse_struct_fields                                            *
 *                                                                          *
 * Purpose : ( type decl ( , decl )* ; )*                                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           00-12-19  Reverted back to (almost) original code and moved    *
 *                     much of the handling of anonymous structs/unions     *
 *                     to types.c - too many bugs in the old approach!      *
 *           01-01-03  Bugfix: better alignment for bitfields in MS mode.   *
 *           01-07-24  Simplified and improved handling of anonymous        *
 *                     members (MS mode) and flexible members in unions.    *
 *           02-06-04  Added support for enum bitfields in MS mode.         *
 *           04-11-05  Bugfix: bad handling of types smaller than int size. *
 *                                                                          *
 ****************************************************************************/

static void parse_struct_fields(TYPE *ty)
{
    int n = 0;

    while (istypename(tok, toksym))
    {
        static char stop[] = { IF, CHAR_, '}', 0 };
        TYPE *ty1 = parse_type(NULL, NULL);

        for (;;)
        {
            char *id = NULL;
            TYPE *fty = parse_declaration(ty1, &id, NULL, FALSE);
            FIELD *field;

            field = new_struct_field(id, ty, fty);

            if (!has_prototype(field->type))
                apperror(RCWARNING2(ERROR_MISSING_PROTOTYPE));

            if (tok == ':')
            {
                if (unqual(field->type) != inttype &&
                    unqual(field->type) != unsignedtype &&
                    unqual(field->type) != booltype)
                {
                    if (options.microsoft)
                    {
                        if (unqual(field->type) != chartype &&
                            unqual(field->type) != unsignedchartype &&
                            unqual(field->type) != shorttype &&
                            unqual(field->type) != unsignedshorttype &&
                            unqual(field->type) != longtype &&
                            unqual(field->type) != unsignedlongtype &&
                            !isenum(unqual(field->type)))
                        {
                            apperror(RCERROR(ERROR_ILLEGAL_BIT_FIELD_TYPE), field->type);
                            field->type = inttype;
                        }
                        else
                        {
                            apperror(RCWARNING2(ERROR_NON_INT_BIT_FIELD_TYPE), field->type);
                        }
                    }
                    else
                    {
                        apperror(RCERROR(ERROR_ILLEGAL_BIT_FIELD_TYPE), field->type);
                        field->type = inttype;
                    }
                }

                tok = gettok();
                field->bitsize = intexpr(0, 0);

                if (field->bitsize > 8*field->type/*inttype*/->size || field->bitsize < 0)  /* 04-11-05 */
                {
                    apperror(RCERROR(ERROR_ILLEGAL_BIT_FIELD_SIZE), field->bitsize);
                    field->bitsize = 8*field->type/*inttype*/->size;  /* 04-11-05 */
                }
                else if (field->bitsize == 0 && id)
                {
                    apperror(RCWARNING2(ERROR_ZERO_WIDTH_FIELD_IGNORED), id, field->type);
                    field->name = stringd(make_label(1));
                }

                field->lsb = 1;
            }
            else
            {
                if (id == NULL)
                {
                    if (options.microsoft && isstruct(fty))
                        apperror(RCWARNING2(ERROR_ANONYMOUS_FIELD_NAME));
                    else
                        apperror(RCERROR(ERROR_EXPECTING_FIELD_NAME));
                }
                else if (isfunc(field->type))
                {
                    apperror(RCERROR(ERROR_ILLEGAL_FIELD_TYPE), field->type);
                }
            }

            if (isconst(field->type))
                ty->u.sym->u.s.cfields = TRUE;
            if (isvolatile(field->type))
                ty->u.sym->u.s.vfields = TRUE;

            n++;

            if (n == 1024)  /* old limit 128 */
                apperror(RCWARNING2(ERROR_MORE_THAN_X_FIELDS), 1023, ty);

            if (tok != ',')
                break;

            tok = gettok();
        }

        follow(';', stop);
    }

    /**/
    {
        FIELD *field, **flist = &ty->u.sym->u.s.flist;
        int bits = 0, offs = 0, overflow = 0;

        ty->align = 1;  /* see dag.c; ty->align = IR->structmetric.align; */

        for (field = *flist; field != NULL; field = field->link)
        {
            int align = (field->type->align) ? field->type->align : 1;

            if (options.structalign && options.structalign < align)
                align = options.structalign;

            if (field->lsb && !options.microsoft)
                align = unsignedtype->align;

            if (ty->op == UNION)
                offs = bits = 0;
            else if (field->bitsize == 0 || bits == 0 || bits-1 + field->bitsize > 8*field->type/*unsignedtype*/->size)  /* 04-11-05 */
            {
                offs = add(offs, bits2bytes(bits-1));
                bits = 0;
                chkoverflow(offs, align-1);
                offs = roundup(offs, align);
            }

            if (align > ty->align)
                ty->align = align;

            field->offset = offs;

            if (field->lsb)
            {
                if (bits == 0)
                    bits = 1;

                if (IR->little_endian)
                    field->lsb = bits;
                else
                    field->lsb = 8*field->type/*unsignedtype*/->size - bits + 1 - field->bitsize + 1;  /* 04-11-05 */

                bits += field->bitsize;
            }
            else
            {
                offs = add(offs, field->type->size);
            }

            if (offs + bits2bytes(bits-1) > ty->size)
                ty->size = offs + bits2bytes(bits-1);

            /* omit anonymous (padding) bit-fields from the list */
            if (!field->name || !('1' <= *field->name && *field->name <= '9'))
            {
                *flist = field;
                flist = &field->link;
            }
        }

        *flist = NULL;

        chkoverflow(ty->size, ty->align-1);
        ty->size = roundup(ty->size, ty->align);
        if (overflow)
        {
            apperror(RCERROR(ERROR_SIZE_EXCEEDS_X_BYTES), ty, inttype->u.sym->u.limits.max.i);
            ty->size = (int)(inttype->u.sym->u.limits.max.i & (~(ty->align-1)));
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: parse_type                                                     *
 *                                                                          *
 * Purpose : Parse basic storage class and type specification.              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           01-09-17  Changed type qualifier error to warning, for C99.    *
 *                                                                          *
 ****************************************************************************/

static TYPE *parse_type(int *sclass, ATTR *attrp)
{
    int cls = 0;    /* storage class */
    int vol = 0;    /* volatile */
    int cons = 0;   /* const */
    int rest = 0;   /* restrict */
    int sign = 0;   /* signed/unsigned */
    int size = 0;
    int type = 0;
#ifdef HAS_C99_COMPLEX
    int cmpl = 0;   /* complex? */
#endif
    TYPE *ty = NULL;

    if (sclass == NULL)
        cls = AUTO;

    for (;;)
    {
        int *p, tt = tok;

        switch (tok)
        {
            case AUTO:
            case REGISTER:
                if (scope <= GLOBAL && cls == 0)
                    apperror(RCERROR(ERROR_ILLEGAL_USE_OF), tok);
                p = &cls;
                tok = gettok();
                break;

            case DECLSPEC:
                parse_spec_declaration(attrp, NULL);
                continue;

            case INLINE:
                if (attrp != NULL)
                    attrp->inlined = TRUE, funca = PERM;  /* types are in PERM, this *shouldn't* be too late */
                tok = gettok();
                continue;

            case STATIC:
            case EXTERN:
            case TYPEDEF:
                p = &cls;
                tok = gettok();
                break;

            case CONST_:
                p = &cons;
                tok = gettok();
                break;

            case VOLATILE_:
                p = &vol;
                tok = gettok();
                break;

            case RESTRICT_:
                p = &rest;
                tok = gettok();
                break;

            case SIGNED:
            case UNSIGNED:
                p = &sign;
                tok = gettok();
                break;

            case LONG_:
                if (size == LONG_)
                {
                    size = 0;
                    tt = LONG_+LONG_;
                }
                p = &size;
                tok = gettok();
                break;

            case SHORT_:
                p = &size;
                tok = gettok();
                break;

            case BOOL_:
            case VOID_:
            case CHAR_:
            case INT_:
            case FLOAT_:
            case DOUBLE_:
                p = &type;
                ty = toksym->type;
                tok = gettok();
                break;

#ifdef HAS_C99_COMPLEX
            case COMPLEX_:
                p = &cmpl;
                tok = gettok();
                break;
#endif

            case ENUM:
                p = &type;
                ty = parse_enum_declaration();
                break;

            case STRUCT:
            case UNION:
                p = &type;
                ty = parse_struct_declaration(tok);
                break;

            case ID:
                if (istypename(tok, toksym) && type == 0 && sign == 0 && size == 0)
                {
#ifdef XREF
                    if (options.xreflevel > 1)
                        use_symbol(toksym, src);
#endif
                    ty = toksym->type;
                    if (isqual(ty) && ty->size != ty->type->size)
                    {
                        ty = unqual(ty);

                        if (isconst(toksym->type))
                            ty = qual(CONST_, ty);
                        if (isvolatile(toksym->type))
                            ty = qual(VOLATILE_, ty);
                        if (isrestrict(toksym->type))
                            ty = qual(RESTRICT_, ty);

                        toksym->type = ty;
                    }
                    p = &type;
                    tok = gettok();
                    break;
                }
                /* fall through */
            default:
                p = NULL;
                break;
        }

        if (p == NULL)
            break;

        /* accept redundant type qualifiers per C99 */
        if (*p == tt && (tt == CONST_ || tt == VOLATILE_ || tt == RESTRICT_))
            apperror(RCWARNING2(ERROR_QUAL_TYPE_IGNORED), tt);
        else if (*p)
            apperror(RCERROR(ERROR_ILLEGAL_USE_OF), tt);
        else
            *p = tt;
    }

    if (sclass)
        *sclass = cls;

    if (type == 0)
    {
        /*
         * C99 no longer has a rule for implicit declaration of variables.
         * We issue a strong warning, but continue in order to support old code.
         */
        if (size == 0 && sign == 0)
            apperror(RCWARNING1(ERROR_MISSING_TYPE_SPEC));
        type = INT_;
        ty = inttype;
    }

    if (size == SHORT_ && type != INT_ ||
        size == LONG_+LONG_ && type != INT_ ||
        size == LONG_ && type != INT_ && type != DOUBLE_ ||
        sign && type != INT_ && type != CHAR_)
        apperror(RCERROR(ERROR_INVALID_TYPE_SPEC));

    if (type == BOOL_)
        ty = booltype;
    else if (type == CHAR_ && sign)
        ty = (sign == UNSIGNED) ? unsignedchartype : signedchartype;
    else if (size == SHORT_)
        ty = (sign == UNSIGNED) ? unsignedshorttype : shorttype;
    else if (size == LONG_ && type == DOUBLE_)
        ty = longdoubletype;
    else if (size == LONG_+LONG_)
        ty = (sign == UNSIGNED) ? unsignedlonglongtype : longlongtype;
    else if (size == LONG_)
        ty = (sign == UNSIGNED) ? unsignedlongtype : longtype;
    else if (sign == UNSIGNED && type == INT_)
        ty = unsignedtype;

#ifdef HAS_C99_COMPLEX
    if (cmpl == COMPLEX_)
    {
        if (ty == floattype)
            ty = complexfloattype;
        else if (ty == doubletype)
            ty = complexdoubletype;
        else if (ty == longdoubletype)
            ty = complexlongdoubletype;
        else
            apperror(RCERROR(ERROR_ILLEGAL_USE_OF), COMPLEX_);
    }
#endif

    if (cons == CONST_)
        ty = qual(CONST_, ty);
    if (vol == VOLATILE_)
        ty = qual(VOLATILE_, ty);
    if (rest == RESTRICT_)
    {
        if (isptr(ty))
            ty = qual(RESTRICT_, ty);
        else
            apperror(RCERROR(ERROR_ILLEGAL_USE_OF), RESTRICT_);
    }

    return ty;
}

/****************************************************************************
 *                                                                          *
 * Function: parse_spec_declaration                                         *
 *                                                                          *
 * Purpose : Parse __declspec() parameters.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-04-21  Added support for __declspec(align(n)).              *
 *           04-11-05  Bugfix: ignore __declspec(dllimport) for ARM target. *
 *           04-12-12  Warn about unknown extended attributes in MS mode.   *
 *                                                                          *
 ****************************************************************************/

static void parse_spec_declaration(ATTR *attrp, int *alignp)
{
    if (!attrp && !alignp)
        apperror(RCWARNING1(ERROR_ILLEGAL_USE_OF), DECLSPEC);

    tok = gettok();

    expect('(');
    while (tok == ID)
    {
        if (strcmp(tokstr, "dllimport") == 0)
        {
            extern INTERFACE armIR;  /* ARM target */
            if (IR == &armIR)
                ;  /* silently ignore */
            else if (!options.microsoft && !options.crtdll)
                apperror(RCWARNING1(ERROR_ILLEGAL_EXTENDED_ATTRIB), tokstr);
            else if (attrp)
                attrp->dllimport = TRUE;
        }
        else if (strcmp(tokstr, "dllexport") == 0)
        {
            if (!options.microsoft && !options.crtdll)
                apperror(RCWARNING1(ERROR_ILLEGAL_EXTENDED_ATTRIB), tokstr);
            else if (attrp)
                attrp->dllexport = TRUE;
        }
        else if (strcmp(tokstr, "naked") == 0)
        {
            if (attrp)
                attrp->naked = TRUE;
        }
        else if (strcmp(tokstr, "noreturn") == 0)
        {
            if (attrp)
                attrp->noreturn = TRUE;
        }
        else if (strcmp(tokstr, "align") == 0)
        {
            tok = gettok();
            expect('(');
            if (tok == ICONST)
            {
                int align = (toksym->type->op == INT_) ? (int)toksym->u.c.v.i : (int)toksym->u.c.v.u;

                if (align == 0 || (align & (align-1)) != 0)
                    apperror(RCERROR(ERROR_UNKNOWN_EXTENDED_ATTRIB), "align(?)");
                else if (alignp)
                    *alignp = align;
            }
            expect(ICONST);
        }
        else
        {
            if (options.microsoft)
                apperror(RCWARNING1(ERROR_UNKNOWN_EXTENDED_ATTRIB), tokstr);
            else
                apperror(RCERROR(ERROR_UNKNOWN_EXTENDED_ATTRIB), tokstr);
        }

        tok = gettok();
    }
    expect(')');
}

/****************************************************************************
 *                                                                          *
 * Function: parse_typename                                                 *
 *                                                                          *
 * Purpose : Parse a type name (used in cast and operand to sizeof).        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

TYPE *parse_typename(void)
{
    TYPE *ty = parse_type(NULL, NULL);

    if (tok == '*' || tok == '(' || tok == '[')
    {
        ty = parse_declaration(ty, NULL, NULL, TRUE);

        if (!has_prototype(ty))
            apperror(RCWARNING2(ERROR_MISSING_PROTOTYPE));
    }

    return ty;
}

/****************************************************************************
 *                                                                          *
 * Function: finalize                                                       *
 *                                                                          *
 * Purpose : Perform some final steps.                                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void finalize(void)
{
    for_each_symbol(externals, GLOBAL, finalize_extern, NULL);
    for_each_symbol(identifiers, GLOBAL, finalize_global, NULL);
    for_each_symbol(identifiers, GLOBAL, check_references, NULL);
    for_each_symbol(constants, CONSTANTS, finalize_const, NULL);
}

/****************************************************************************
 *                                                                          *
 * Function: finalize_extern                                                *
 *                                                                          *
 * Purpose : Define a external symbol in the back-end.                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void finalize_extern(SYMBOL *sym, void *cl)
{
    (*IR->import)(sym);
}

/****************************************************************************
 *                                                                          *
 * Function: finalize_global                                                *
 *                                                                          *
 * Purpose : Define a global symbol in the back-end.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void finalize_global(SYMBOL *sym, void *cl)
{
    if (!sym->defined && (sym->sclass == EXTERN || isfunc(sym->type) && sym->sclass == AUTO))
    {
        (*IR->import)(sym);

        if (options.dbglevel > 1 && IR->dbgsym)
            (*IR->dbgsym)(sym);
    }
    else if (!sym->defined && !isfunc(sym->type) && (sym->sclass == AUTO || sym->sclass == STATIC))
    {
        if (isarray(sym->type) &&
            sym->type->size == 0 && sym->type->type->size > 0)
            sym->type = new_array(sym->type->type, 1, 0);

        if (sym->type->size > 0)
        {
            define_global(sym, BSS);
            (*IR->space)(sym->type->size);

            if (options.dbglevel > 1 && IR->dbgsym)
                (*IR->dbgsym)(sym);
        }
        else
        {
            apperror(RCERROR(ERROR_UNDEFINED_SIZE_FOR), sym->name, sym->type);
        }

        sym->defined = TRUE;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: finalize_const                                                 *
 *                                                                          *
 * Purpose : Define a constant symbol in the back-end.                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-07-07  Added check for cbstring option (drivers).           *
 *                                                                          *
 ****************************************************************************/

static void finalize_const(SYMBOL *sym, void *cl)
{
    if (sym->u.c.loc && sym->ref)
    {
        assert(sym->u.c.loc->u.seg == 0);
        define_global(sym->u.c.loc, options.cbstring ? TEXT : LIT);

        if (isarray(sym->type) && sym->type->type == widechartype)
        {
            widechar_t *s = sym->u.c.v.p;
            int n = sym->type->size / widechartype->size;

            while (n-- > 0)
            {
                VALUE v;
                v.u = *s++;
                (*IR->defconst)(widechartype->op, widechartype->size, v);
            }
        }
        else if (isarray(sym->type))
        {
            (*IR->defstring)(sym->type->size, sym->u.c.v.p);
        }
        else
        {
            (*IR->defconst)(sym->type->op, sym->type->size, sym->u.c.v);
        }

        sym->u.c.loc = NULL;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: check_labels                                                   *
 *                                                                          *
 * Purpose : Check for undefined labels; called at end of function defs.    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void check_labels(SYMBOL *sym, void *cl)
{
    if (!sym->defined)
        apperror(RCERROR(ERROR_UNDEFINED_LABEL), sym->name);

    sym->defined = TRUE;
}

/****************************************************************************
 *                                                                          *
 * Function: check_references                                               *
 *                                                                          *
 * Purpose : Check for unreferenced variables; called at end of blocks.     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           03-09-23  Removed warning for unreferenced inline function.    *
 *           04-12-21  Force REGISTER as much as possible.                  *
 *                                                                          *
 ****************************************************************************/

static void check_references(SYMBOL *sym, void *cl)
{
    if (sym->scope >= PARAM && (isvolatile(sym->type) || isfunc(sym->type)))
        sym->addressed = TRUE;

    if (sym->defined && sym->ref == 0)
    {
        if (sym->attr.inlined)
            ;
        else if (sym->sclass == STATIC)
            apperror(RCWARNING1(ERROR_UNREFERENCED_STATIC), sym->name);
        else if (sym->scope == PARAM)
            apperror(RCWARNING2(ERROR_UNREFERENCED_PARAM), sym->name);
        else if (sym->scope >= LOCAL && sym->sclass != EXTERN)
            apperror(RCWARNING1(ERROR_UNREFERENCED_LOCAL), sym->name);

        if (sym->sclass == REGISTER)
            sym->sclass = AUTO;
    }

    /* force REGISTER as much as possible - let the backend sort it out */
    if (sym->sclass == AUTO && sym->scope >= PARAM && !sym->addressed && isscalar(sym->type) && sym->ref >= 2.0)
        sym->sclass = REGISTER;

    /*
    if (sym->sclass == AUTO && (sym->scope == PARAM && regcount == 0 || sym->scope >= LOCAL) &&
        !sym->addressed && isscalar(sym->type) && sym->ref >= 2.0)
        sym->sclass = REGISTER;
    */
    if (scope == GLOBAL && sym->sclass == STATIC && !sym->defined && isfunc(sym->type) && sym->ref)
        apperror(RCERROR(ERROR_UNDEFINED_STATIC), sym->name);

    assert(!(scope == GLOBAL && sym->sclass == STATIC && !sym->defined && !isfunc(sym->type)));
}

/****************************************************************************
 *                                                                          *
 * Function: inline_function                                                *
 *                                                                          *
 * Purpose : See if the specified function is marked inline. If so, try     *
 *           to build a tree from the function with given arguments.        *
 *                                                                          *
 * Comment : In short, the rules in C99 as currently understood:            *
 *                                                                          *
 *           All declarations specify inline but not extern -- pure inline  *
 *           function (no external symbol defined or code generated); calls *
 *           might be inlined using the supplied definition or might be     *
 *           actual calls to an external function.                          *
 *                                                                          *
 *           Some declarations specify inline and either at least one       *
 *           declaration specifies extern or at least one declaration does  *
 *           not specify inline -- normal function definition (external     *
 *           symbol is defined and code is generated) with strong           *
 *           suggestion to inline calls; again, calls might be inlined or   *
 *           might be actual calls to an external function.                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           03-08-25  Created                                              *
 *                                                                          *
 ****************************************************************************/

TREE *inline_function(SYMBOL *sym, TYPE *rty, TREE *args)
{
    if (functions)
    {
        LIST *lp = functions;
        do
        {
            FCNENTRY *fcn;
            lp = lp->link;

            fcn = (FCNENTRY *)lp->data;
            if (fcn->func == sym)
            {
                TREE *e = NULL;
                CODE *cp;

                assert(fcn->func->type->type == rty);

                fcn->args = args;
                for (cp = fcn->codelist; cp != NULL; cp = cp->next)
                {
                    switch (cp->kind)
                    {
                        case CODE_GEN:
                        {
                            NODE *p;

                            for (p = cp->u.forest; p != NULL; p = p->link)
                            {
                                TREE *q;

                                /* try to build a tree - might fail */
                                explicit_cast++;
                                q = tree_from_dag(p, fcn);
                                explicit_cast--;
                                if (!q) return NULL;

                                /* put "parenthesis" around the expression */
                                e = new_tree(RIGHT, q->type, e, q);
                            }
                            break;
                        }

                        case CODE_BLOCKBEG:
                        {
                            bool_t syms;

                            /* locals and statics are not acceptable */
                            if (*cp->u.block.locals != NULL)
                                return NULL;
                            else if (syms = 0, for_each_symbol(cp->u.block.identifiers,
                                cp->u.block.level, check_inline, &syms), syms != 0)
                                return NULL;
                            break;
                        }

                        case CODE_LABEL:
                            /* only the return label is acceptable */
                            if (cp->next != NULL)
                                return NULL;
                            break;

                        case CODE_BLOCKEND:
                        case CODE_DEFPOINT:
                            break;

                        default:
#ifdef PODEBUG
                            printf("unacceptable code list (%d) entry for inline function %s\n", cp->kind, fcn->func->name);
#endif
                            /* unacceptable codelist entry (for example jump) */
                            return NULL;
                    }
                }

                /* return tree on success */
                return e;
            }
        } while (lp != functions);
    }

    return NULL;
}

/****************************************************************************
 *                                                                          *
 * Function: check_inline                                                   *
 *                                                                          *
 * Purpose : Check for inline function symbols (static).                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           03-08-25  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void check_inline(SYMBOL *sym, void *cl)
{
    *(bool_t *)cl = 1;
}

/****************************************************************************
 *                                                                          *
 * Function: inline_symbol_ref                                              *
 *                                                                          *
 * Purpose : Keep track of reference increments for inline symbols,         *
 *           so we can undo it if the function isn't used.                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-11-25  Created                                              *
 *                                                                          *
 ****************************************************************************/

void inline_symbol_ref(SYMBOL *sym)
{
    SYMREF *symref;

    if (refsyms != NULL)
    {
        LIST *lp = refsyms;
        do
        {
            symref = lp->data;
            if (symref->sym == sym)
            {
                /* bump increment for existing symbol */
                symref->refinc += refinc;
                return;
            }
        } while ((lp = lp->link) != refsyms);
    }

    /* add new symbol with increment */
    symref = memalloc(sizeof(*symref), PERM);
    symref->sym = sym;
    symref->refinc = refinc;
    refsyms = listappend(symref, refsyms);
}

/****************************************************************************
 *                                                                          *
 * Function: tree_from_dag                                                  *
 *                                                                          *
 * Purpose : Try to build a tree from the inlined functions dag.            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           03-08-25  Created                                              *
 *           04-08-25  Added CEQ, CGE, CGT, CLE, CLT, CNE.                  *
 *           04-12-21  Added INTRIN1, INTRIN2, INTRIN1S, INTRIN2S.          *
 *           04-12-28  Bugfix: don't accept ADDRF without INDIR.            *
 *                                                                          *
 ****************************************************************************/

static TREE *tree_from_dag(NODE *p, FCNENTRY *fcn)
{
    TREE *l, *r;
    int op;

    if (p == NULL)
        return NULL;

    if ((generic(p->op) == ASGN &&
        generic(p->kids[0]->op) == ADDRL &&
        p->kids[0]->syms[0]->temporary &&
        p->kids[0]->syms[0]->u.t.cse == p->kids[1]))
    {
        return tree_from_dag(p->kids[1], fcn);
    }

    switch (generic(p->op))
    {
        case RET:
            return tree_from_dag(p->kids[0], fcn);

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
            l = tree_from_dag(p->kids[0], fcn);
            if (l == NULL) return NULL;
            r = tree_from_dag(p->kids[1], fcn);
            if (r == NULL) return NULL;
            return simplify(specific(p->op), btot(p->op, opsize(p->op)), l, r);

        case BCOM:
        case NEG:
        case CVU:
        case CVI:
        case CVP:
        case CVF:
        case CBOOL:
            l = tree_from_dag(p->kids[0], fcn);
            if (l == NULL) return NULL;
            return simplify(specific(p->op), btot(p->op, opsize(p->op)), l, NULL);

        case CEQ:
        case CNE:
        case CGT:
        case CLT:
        case CGE:
        case CLE:
            switch (generic(p->op))
            {
                case CEQ: op = EQ; break;
                case CNE: op = NE; break;
                case CGT: op = GT; break;
                case CLT: op = LT; break;
                case CGE: op = GE; break;
                case CLE: op = LE; break;
            }
            l = tree_from_dag(p->kids[0], fcn);
            if (l == NULL) return NULL;
            r = tree_from_dag(p->kids[1], fcn);
            if (r == NULL) return NULL;
            return value(simplify(op, btot(p->op, opsize(p->op)), l, r));

        case INDIR:
            if (generic(p->kids[0]->op) == ADDRF)
            {
                int i;

                /* find argument and use the correct tree from args */
                for (i = 0; fcn->callee[i] != NULL; i++)
                {
                    if (fcn->callee[i] == p->kids[0]->syms[0])
                    {
                        /* found the argument - find the correct tree in args */
                        TYPE *ty = fcn->callee[i]->type;

                        /* we get a RIGHT tree if there is a function call among the arguments */
                        for (r = fcn->args, l = (r->op == RIGHT ? r->kids[1] : r); fcn->callee[++i] != NULL; l = l->kids[1])
                            assert(generic(l->op) == ARG);

                        assert(generic(l->op) == ARG);
                        return cast(l->kids[0], ty);
                    }
                }
                break;
            }
            else if (generic(p->kids[0]->op) == ADDRL)
            {
                if (p->kids[0]->syms[0]->u.t.cse)
                    return tree_from_dag(p->kids[0]->syms[0]->u.t.cse, fcn);
                break;
            }
            else  /* ADDRG */
            {
                l = tree_from_dag(p->kids[0], fcn);
                if (l == NULL) return NULL;
                return new_tree(specific(p->op), btot(p->op, opsize(p->op)), l, NULL);
            }

        /* case ADDRF: */
        case ADDRG:
            l = new_tree(specific(p->op), btot(p->op, opsize(p->op)), NULL, NULL);
            l->u.sym = p->syms[0];
            return l;

        case CNST:
            l = new_tree(specific(p->op), btot(p->op, opsize(p->op)), NULL, NULL);
            l->u.v = p->syms[0]->u.c.v;
            return l;

        case ASGN:
            l = tree_from_dag(p->kids[0], fcn);
            if (l == NULL) return NULL;
            r = tree_from_dag(p->kids[1], fcn);
            if (r == NULL) return NULL;
            return new_tree(specific(p->op), btot(p->op, opsize(p->op)), l, r);

        case INTRIN1:
        case INTRIN1S:
            l = tree_from_dag(p->kids[0], fcn);
            if (l == NULL) return NULL;
            l = new_tree(specific(p->op), btot(p->op, opsize(p->op)), l, NULL);
            l->u.sym = p->syms[0];  /* pass on function ID */
            return l;

        case INTRIN2:
        case INTRIN2S:
            l = tree_from_dag(p->kids[0], fcn);
            if (l == NULL) return NULL;
            r = tree_from_dag(p->kids[1], fcn);
            if (r == NULL) return NULL;
            l = new_tree(specific(p->op), btot(p->op, opsize(p->op)), l, r);
            l->u.sym = p->syms[0];  /* pass on function ID */
            return l;

        default:
#ifdef PODEBUG
            printf("unrecognized inline op = %s\n", opname(p->op));
#endif
            break;
    }

    return NULL;
}

/****************************************************************************
 *                                                                          *
 * Function: do_referenced_inline_functions                                 *
 *                                                                          *
 * Purpose : Emit all referenced inline functions.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           01-01-10  Bugfix: wipe out bogus source coordinates.           *
 *           04-07-10  Bugfix: must set TEXT segment before emitting code.  *
 *           04-11-25  Bugfix: adjust references in unreferenced functions. *
 *           04-12-11  Bugfix: emit debugging information.                  *
 *           04-12-28  Bugfix: must adjust references in own loop.          *
 *                                                                          *
 ****************************************************************************/

static void do_referenced_inline_functions(void)
{
    /*
     * Inline functions that are called from ordinary functions have a
     * non-zero reference count. Make sure that all inline functions,
     * that are called from these inline functions, also gets a non-zero
     * reference count, i.e. gets emitted.
     */
    if (functions)
    {
        bool_t again;
        do
        {
            LIST *lp = functions;
            again = FALSE;
            do
            {
                FCNENTRY *fcn;
                lp = lp->link;

                fcn = (FCNENTRY *)lp->data;
                if (!fcn->touched && fcn->func->ref != 0)
                {
                    CODE *cp;

                    fcn->touched = TRUE;
                    for (cp = fcn->codelist; cp != NULL; cp = cp->next)
                    {
                        switch (cp->kind)
                        {
                            case CODE_GEN:
                            {
                                NODE *p;

                                for (p = cp->u.forest; p != NULL; p = p->link)
                                {
                                    if (generic(p->op) == CALL)
                                        touch_function(p, &again);
                                    else if (generic(p->op) == ASGN && generic(p->kids[1]->op) == CALL)
                                        touch_function(p->kids[1], &again);
                                }
                                break;
                            }

                            case CODE_ASM:
                                if (cp->u.asm.sym && cp->u.asm.sym->ref == 0)
                                {
                                    cp->u.asm.sym->ref++;
                                    again = TRUE;
                                }
                                break;

                            default:
                                break;
                        }
                    }
                }
            } while (lp != functions);
        } while (again);
    }

    /*
     * Adjust symbol references in unreferenced inline functions.
     * Note! Must be done after touching, but before emitting, functions.
     */
    if (functions)  /* bugfix 04-12-28 */
    {
        LIST *lp = functions;
        do
        {
            FCNENTRY *fcn;
            lp = lp->link;

            fcn = (FCNENTRY *)lp->data;
            if (fcn->func->ref == 0 && fcn->refsyms != NULL)
            {
                LIST *lp = fcn->refsyms;
                do
                {
                    SYMREF *symref = lp->data;
                    if (symref->sym->ref != 0)
                        symref->sym->ref -= symref->refinc;
#ifdef PODEBUG
printf("symbol %s adjusted to %f by %s\n", symref->sym->name, symref->sym->ref, fcn->func->name);
#endif
                } while ((lp = lp->link) != fcn->refsyms);
            }
        } while (lp != functions);
    }

    /*
     * Emit all referenced inline functions.
     */
    if (functions)
    {
        LIST *lp = functions;
        do
        {
            FCNENTRY *fcn;
            lp = lp->link;

            fcn = (FCNENTRY *)lp->data;
#ifdef PODEBUG
printf("inline function %s, ref = %g\n", fcn->func->name, fcn->func->ref);
#endif
            if (fcn->func->ref != 0)
            {
                CODE *cp;

                /* wipe out bogus source coordinates */
                for (cp = fcn->codelist; cp != NULL; cp = cp->next)
                {
                    switch (cp->kind)
                    {
                        case CODE_DEFPOINT:
                            cp->u.point.src.file = NULL;
                            break;

                        case CODE_ASM:
                            cp->u.asm.src.file = NULL;
                            break;
                    }
                }

                if (fcn->func->sclass == EXTERN)
                    (*IR->export)(fcn->func);

                /* emit to default section - for now */
                *options.codeseg = *options.dataseg = *options.litseg = '\0';

                if (options.dbglevel > 1 && IR->dbgsym)  /* bugfix 04-12-11 */
                {
                    set_segment(TEXT);  /* bugfix 04-07-10 */
                    (*IR->dbgsym)(fcn->func);
                }
                set_segment(TEXT);  /* bugfix 04-07-10 */

                /* finally, emit function code */
                codelist = &codehead;
                codelist->next = fcn->codelist;
                (*IR->function)(fcn->func, fcn->caller, fcn->callee, fcn->func->u.fcn.ncalls);
                codelist->next = NULL;

                if (options.dbglevel > 1 && IR->dbgfend)  /* bugfix 04-12-11 */
                    (*IR->dbgfend)(fcn->func, 0);
            }
        } while (lp != functions);
    }
}

/****************************************************************************
 *                                                                          *
 * Function: touch_function                                                 *
 *                                                                          *
 * Purpose : Bump reference count for a function invoked from here.         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void touch_function(NODE *p, bool_t *again)
{
    if (generic(p->kids[0]->op) == INDIR)
        p = p->kids[0];

    if (generic(p->kids[0]->op) == ADDRG && p->kids[0]->syms[0]->ref == 0)
    {
        p->kids[0]->syms[0]->ref++;
        *again = TRUE;
    }
}
