/****************************************************************************
 *                                                                          *
 * File    : input.c                                                        *
 *                                                                          *
 * Purpose : ISO C Compiler; Input processing.                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           01-03-21  Added pragma_comment().                              *
 *           03-08-01  Added pragma_startup(), pragma_exit().               *
 *           04-03-13  Added pragma_segment().                              *
 *           04-07-10  Added pragma_warn().                                 *
 *           04-07-16  Added pragma_intrinsic().                            *
 *           04-12-10  Added pragma_codepage().                             *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "lcc.h"

/* Globals */
char *file;         /* current input file name */
char *firstfile;    /* first input file */
uchar_t *cp;        /* current input character */
uchar_t *limit;     /* points to last character + 1 */
char *line;         /* current line */
int lineno;         /* line number of current line */

/* Locals */
static size_t bsize;    /* number of chars in last read */
static uchar_t buffer[MAXLINE+1 + BUFSIZE+1];  /* input buffer */

/* Static function prototypes */
static void resynch(void);
static void pragma(void);
static void pragma_stdc(void);
static void pragma_ref(void);
static void pragma_pack(void);
static void pragma_optimize(void);
static void pragma_comment(void);
static void pragma_message(void);
static void pragma_startup(void);
static void pragma_exit(void);
static void pragma_segment(char *, size_t);
static void pragma_warn(void);
static void pragma_intrinsic(bool_t);
static void pragma_codepage(void);
static int getrowtok(void);
static bool_t is_initexit_function(TYPE *);

/****************************************************************************
 *                                                                          *
 * Function: cc_input_init                                                  *
 *                                                                          *
 * Purpose : Initialize input processing.                                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void cc_input_init(void)
{
    limit = cp = &buffer[MAXLINE+1];
    lineno = 0;
    file = NULL;
    bsize = -1;

    cc_fillbuf();
    if (cp >= limit)
        cp = limit;

    cc_nextline();
}

/****************************************************************************
 *                                                                          *
 * Function: cc_nextline                                                    *
 *                                                                          *
 * Purpose : Prepare to read next line.                                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void cc_nextline(void)
{
    do
    {
        if (cp >= limit)
        {
            cc_fillbuf();
            if (cp >= limit)
                cp = limit;
            if (cp == limit)
                return;
        }
        else
        {
            lineno++;

            for (line = (char *)cp; *cp == ' ' || *cp == '\t'; cp++)
                ;

            if (*cp == '#')  /* #pragma and #line */
            {
                resynch();
                cc_nextline();
            }
        }
    } while (*cp == '\n' && cp == limit);
}

/****************************************************************************
 *                                                                          *
 * Function: cc_fillbuf                                                     *
 *                                                                          *
 * Purpose : Fill the input buffer, moving tail portion if necessary.       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void cc_fillbuf(void)
{
    if (bsize == 0)
        return;

    if (cp >= limit)
    {
        cp = &buffer[MAXLINE+1];
    }
    else
    {
        size_t sz = limit - cp;
        uchar_t *s = &buffer[MAXLINE+1] - sz;

        assert(s >= buffer);
        line = (char *)s - ((char *)cp - line);

        while (cp < limit)
            *s++ = *cp++;

        cp = &buffer[MAXLINE+1] - sz;
    }

    bsize = pp_read(&buffer[MAXLINE+1], BUFSIZE);

    limit = &buffer[MAXLINE+1+bsize];
    *limit = '\n';
}

/****************************************************************************
 *                                                                          *
 * Function: resynch                                                        *
 *                                                                          *
 * Purpose : Set line number/file name in # n [ "file" ] and #pragma ...    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void resynch(void)
{
    for (cp++; *cp == ' ' || *cp == '\t'; cp++)
        ;

    if (limit - cp < MAXLINE)
        cc_fillbuf();

    if (strncmp((char *)cp, "pragma", 6) == 0)
    {
        cp += 6;
        pragma();
    }
    else if (*cp >= '0' && *cp <= '9')
    {
line:
        for (lineno = 0; *cp >= '0' && *cp <= '9'; )
            lineno = 10 * lineno + *cp++ - '0';
        lineno--;

        while (*cp == ' ' || *cp == '\t')
            cp++;

        if (*cp == '"')
        {
            file = (char *)++cp;

            while (*cp && *cp != '"' && *cp != '\n')
                cp++;

            file = stringn(file, (char *)cp - file);

            if (*cp == '\n')
                apperror(RCWARNING1(ERROR_MISSING_QUOTE_IN_PP_LINE));  /* src is updated in gettok()! */

            if (firstfile == NULL)
                firstfile = file;
        }
    }
    else if (strncmp((char *)cp, "line", 4) == 0)
    {
        for (cp += 4; *cp == ' ' || *cp == '\t'; cp++)
            ;

        if (*cp >= '0' && *cp <= '9')
            goto line;

        apperror(RCWARNING2(ERROR_UNRECOGNIZED_CTRL_LINE));  /* src is updated in gettok()! */
    }
    else if (*cp != '\n')
        apperror(RCWARNING2(ERROR_UNRECOGNIZED_CTRL_LINE));  /* src is updated in gettok()! */

    while (*cp)
    {
        if (*cp++ == '\n')
        {
            if (cp == limit + 1)
            {
                cc_nextline();
                if (cp == limit) break;
            }
            else
                break;
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: pragma                                                         *
 *                                                                          *
 * Purpose : Handle #pragma.                                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           01-03-21  Added 'comment'.                                     *
 *           01-07-30  Only check 'once' and 'warning' when /Ze (cosmetic). *
 *           03-08-01  Added 'startup' and 'exit'.                          *
 *           04-03-13  Added 'code_seg', 'data_seg' and 'const_seg'.        *
 *           04-07-10  Added 'warn'.                                        *
 *           04-07-16  Added 'intrinsic' and 'function'.                    *
 *           04-08-06  Added 'nodefaultlib'.                                *
 *           04-12-10  Added 'code_page'.                                   *
 *                                                                          *
 ****************************************************************************/

static void pragma(void)
{
    if ((tok = gettok()) == ID)
    {
        if (strcmp(tokstr, "STDC") == 0)
            pragma_stdc();
        else if (strcmp(tokstr, "ref") == 0)
            pragma_ref();
        else if (strcmp(tokstr, "pack") == 0)
            pragma_pack();
        else if (strcmp(tokstr, "optimize") == 0)
            pragma_optimize();
        else if (strcmp(tokstr, "comment") == 0)
            pragma_comment();
        else if (strcmp(tokstr, "message") == 0)
            pragma_message();
        else if (strcmp(tokstr, "startup") == 0)
            pragma_startup();
        else if (strcmp(tokstr, "exit") == 0)
            pragma_exit();
        else if (strcmp(tokstr, "code_seg") == 0)
            pragma_segment(options.codeseg, NELEMS(options.codeseg));
        else if (strcmp(tokstr, "data_seg") == 0)
            pragma_segment(options.dataseg, NELEMS(options.dataseg));
        else if (strcmp(tokstr, "const_seg") == 0)
            pragma_segment(options.litseg, NELEMS(options.litseg));
        else if (strcmp(tokstr, "warn") == 0)
            pragma_warn();
        else if (strcmp(tokstr, "intrinsic") == 0)
            pragma_intrinsic(TRUE);
        else if (strcmp(tokstr, "function") == 0)
            pragma_intrinsic(FALSE);
        else if (strcmp(tokstr, "nodefaultlib") == 0)
            options.defaultlib = FALSE;
        else if (strcmp(tokstr, "code_page") == 0)
            pragma_codepage();
        else if (options.microsoft && (strcmp(tokstr, "once") == 0 || strcmp(tokstr, "warning") == 0))
            ; /* accept but ignore */
        else
            apperror(RCWARNING2(ERROR_UNRECOGNIZED_PRAGMA), tokstr);
    }
}

/****************************************************************************
 *                                                                          *
 * Function: pragma_stdc                                                    *
 *                                                                          *
 * Purpose : Handle #pragma STDC FP_CONTRACT {ON|OFF|DEFAULT} or            *
 *                  #pragma STDC FENV_ACCESS {ON|OFF|DEFAULT} or            *
 *                  #pragma STDC CX_LIMITED_RANGE {ON|OFF|DEFAULT}          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void pragma_stdc(void)
{
    while ((tok = getrowtok()) == ID)
        ;
}

/****************************************************************************
 *                                                                          *
 * Function: pragma_ref                                                     *
 *                                                                          *
 * Purpose : Handle #pragma ref id [id...].                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void pragma_ref(void)
{
    while ((tok = getrowtok()) == ID && toksym != NULL)
    {
        toksym->ref++;

#ifdef XREF
        if (options.xreflevel > 1)
            use_symbol(toksym, src);
#endif
    }
}

/****************************************************************************
 *                                                                          *
 * Function: pragma_pack                                                    *
 *                                                                          *
 * Purpose : Handle #pragma pack( [[{push|pop},] [id,]] [n] )               *
 *           <n> must be 1, 2, 4, 8 or 16.                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void pragma_pack(void)
{
    if ((tok = getrowtok()) != '(')
    {
        apperror(RCWARNING1(ERROR_EXPECTED_TOKEN_X), '(');
    }
    else if ((tok = getrowtok()) == ')')
    {
        options.structalign = 0;
        tok = getrowtok();
    }
    else
    {
        if (tok == ID)
        {
            static int stack[100], si = 0;

            if (strcmp(tokstr, "push") == 0)
            {
                if (si < NELEMS(stack))
                    stack[si++] = options.structalign;
            }
            else if (strcmp(tokstr, "pop") == 0)
            {
                if (si > 0)
                    options.structalign = stack[--si];
            }

            if ((tok = getrowtok()) == ',')
                 tok = getrowtok();
        }

        if (tok == ID)
        {
            if ((tok = getrowtok()) == ',')
                 tok = getrowtok();
        }

        if (tok == ICONST)
        {
            int align = (toksym->type->op == INT_) ? (int)toksym->u.c.v.i : (int)toksym->u.c.v.u;

            switch (align)
            {
                case 1:
                case 2:
                case 4:
                case 8:
                case 16:
                    options.structalign = align;
                    break;

                default:
                    apperror(RCWARNING1(ERROR_INVALID_PACK_PRAGMA));
                    break;
            }

            tok = getrowtok();
        }

        if (tok != ')')
            apperror(RCWARNING1(ERROR_EXPECTED_TOKEN_X), ')');
        else
            tok = getrowtok();
    }
}

/****************************************************************************
 *                                                                          *
 * Function: pragma_optimize                                                *
 *                                                                          *
 * Purpose : Handle #pragma optimize( [{time|size|none}] )                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void pragma_optimize(void)
{
    if ((tok = getrowtok()) != '(')
    {
        apperror(RCWARNING1(ERROR_EXPECTED_TOKEN_X), '(');
    }
    else if ((tok = getrowtok()) == ')')
    {
        options.pragmaopt = options.optimize;  /* default */
        tok = getrowtok();
    }
    else
    {
        if (tok == ID)
        {
            if (strcmp(tokstr, "time") == 0)
                options.pragmaopt = MAXSPEED;
            else if (strcmp(tokstr, "size") == 0)
                options.pragmaopt = MINSPACE;
            else if (strcmp(tokstr, "none") == 0)
                options.pragmaopt = DONTOPT;

            tok = getrowtok();
        }

        if (tok != ')')
            apperror(RCWARNING1(ERROR_EXPECTED_TOKEN_X), ')');
        else
            tok = getrowtok();
    }
}

/****************************************************************************
 *                                                                          *
 * Function: pragma_comment                                                 *
 *                                                                          *
 * Purpose : Handle #pragma comment( type [,string] )                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-03-21  Created                                              *
 *           04-03-15  Added warning if not 'lib' or 'linker' argument.     *
 *                                                                          *
 ****************************************************************************/

static void pragma_comment(void)
{
    if ((tok = getrowtok()) != '(')
    {
        apperror(RCWARNING1(ERROR_EXPECTED_TOKEN_X), '(');
    }
    else
    {
        if ((tok = getrowtok()) == ID)
        {
            if (strcmp(tokstr, "lib") == 0)
            {
                if ((tok = getrowtok()) == ',')
                {
                    if ((tok = getrowtok()) == SCONST)
                    {
                        set_segment(DRECTVE);
                        (*IR->defstring)(13, " -defaultlib:");
                        (*IR->defstring)(strlen(toksym->u.c.v.p) /* stop at first '\0' */, toksym->u.c.v.p);

                        tok = getrowtok();
                    }
                }
            }
            else if (strcmp(tokstr, "linker") == 0)
            {
                if ((tok = getrowtok()) == ',')
                {
                    if ((tok = getrowtok()) == SCONST)
                    {
                        set_segment(DRECTVE);
                        (*IR->defstring)(1, " ");
                        (*IR->defstring)(strlen(toksym->u.c.v.p) /* stop at first '\0' */, toksym->u.c.v.p);

                        tok = getrowtok();
                    }
                }
            }
            else
            {
                apperror(RCWARNING1(ERROR_INVALID_COMMENT_PRAGMA));
            }
        }

        if (tok != ')')
            apperror(RCWARNING1(ERROR_EXPECTED_TOKEN_X), ')');
        else
            tok = getrowtok();
    }
}

/****************************************************************************
 *                                                                          *
 * Function: pragma_message                                                 *
 *                                                                          *
 * Purpose : Handle #pragma message( [string] )                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-05-30  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void pragma_message(void)
{
    if ((tok = getrowtok()) != '(')
    {
        apperror(RCWARNING1(ERROR_EXPECTED_TOKEN_X), '(');
    }
    else
    {
        if ((tok = getrowtok()) == SCONST)
        {
            printf("%s\n", toksym->u.c.v.p);
            tok = getrowtok();
        }

        if (tok != ')')
            apperror(RCWARNING1(ERROR_EXPECTED_TOKEN_X), ')');
        else
            tok = getrowtok();
    }
}

/****************************************************************************
 *                                                                          *
 * Function: pragma_startup                                                 *
 *                                                                          *
 * Purpose : Handle #pragma startup id.                                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           03-08-01  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void pragma_startup(void)
{
    if ((tok = getrowtok()) == ID && toksym != NULL && is_initexit_function(toksym->type))
    {
        toksym->ref++;
        toksym->init = TRUE;

#ifdef XREF
        if (options.xreflevel > 0)
            use_symbol(toksym, src);
#endif
    }
    else
    {
        apperror(RCWARNING1(ERROR_INVALID_INITEXIT_PRAGMA));
    }
}

/****************************************************************************
 *                                                                          *
 * Function: pragma_exit                                                    *
 *                                                                          *
 * Purpose : Handle #pragma exit id.                                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           03-08-01  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void pragma_exit(void)
{
    if ((tok = getrowtok()) == ID && toksym != NULL && is_initexit_function(toksym->type))
    {
        toksym->ref++;
        toksym->exit = TRUE;

#ifdef XREF
        if (options.xreflevel > 0)
            use_symbol(toksym, src);
#endif
    }
    else
    {
        apperror(RCWARNING1(ERROR_INVALID_INITEXIT_PRAGMA));
    }
}

/****************************************************************************
 *                                                                          *
 * Function: pragma_segment                                                 *
 *                                                                          *
 * Purpose : Handle #pragma {code_seg|data_seg|const_seg}( ["name"] ).      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-03-13  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void pragma_segment(char *buf, size_t maxbuf)
{
    if ((tok = getrowtok()) != '(')
    {
        apperror(RCWARNING1(ERROR_EXPECTED_TOKEN_X), '(');
    }
    else if ((tok = getrowtok()) == ')')
    {
        set_segment(0);
        *buf = '\0';  /* default */
        tok = getrowtok();
    }
    else
    {
        if (tok == SCONST)
        {
            set_segment(0);
            strncpy(buf, toksym->u.c.v.p, maxbuf - 1);
            buf[maxbuf - 1] = '\0';
            tok = getrowtok();
        }

        if (tok != ')')
            apperror(RCWARNING1(ERROR_EXPECTED_TOKEN_X), ')');
        else
            tok = getrowtok();
    }
}

/****************************************************************************
 *                                                                          *
 * Function: pragma_warn                                                    *
 *                                                                          *
 * Purpose : Handle #pragma warn( {push|pop|{enable|disable}:num ...} ).    *
 *                                                                          *
 * Comment : We choose "warn" to avoid conflict with Microsoft "warning".   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-07-10  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void pragma_warn(void)
{
    bool_t enable = -1;

    if ((tok = getrowtok()) != '(')
    {
        apperror(RCWARNING1(ERROR_EXPECTED_TOKEN_X), '(');
    }
    else
    {
        if ((tok = getrowtok()) == ID)
        {
            static void *stack[100];
            static int si = 0;

            if (strcmp(tokstr, "push") == 0)
            {
                if (si < NELEMS(stack))
                    stack[si++] = copy_warnings();
            }
            else if (strcmp(tokstr, "pop") == 0)
            {
                if (si > 0)
                    restore_warnings(stack[--si]);
            }
            else if (strcmp(tokstr, "enable") == 0)
                enable = TRUE;
            else if (strcmp(tokstr, "disable") == 0)
                enable = FALSE;
            else
                apperror(RCWARNING1(ERROR_INVALID_WARN_PRAGMA));

            tok = getrowtok();
        }

        if (enable != -1)
        {
            if (tok == ':')
                tok = getrowtok();

            while (tok == ICONST)
            {
                enable_or_disable_warning(enable, (toksym->type->op == INT_) ?
                    (WINERR)toksym->u.c.v.i : (WINERR)toksym->u.c.v.u);
                tok = getrowtok();
            }
        }

        if (tok != ')')
            apperror(RCWARNING1(ERROR_EXPECTED_TOKEN_X), ')');
        else
            tok = getrowtok();
    }
}

/****************************************************************************
 *                                                                          *
 * Function: pragma_intrinsic                                               *
 *                                                                          *
 * Purpose : Handle #pragma {intrinsic|function}( name [[,] name ...] ).    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-07-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void pragma_intrinsic(bool_t enable)
{
    if ((tok = getrowtok()) != '(')
    {
        apperror(RCWARNING1(ERROR_EXPECTED_TOKEN_X), '(');
    }
    else
    {
        while ((tok = getrowtok()) == ID || tok == ',')
        {
            if (tok == ID && !enable_or_disable_intrinsic(enable, tokstr) && enable)
                apperror(RCWARNING1(ERROR_INVALID_INTRINSIC_PRAGMA), tokstr);
        }

        if (tok != ')')
            apperror(RCWARNING1(ERROR_EXPECTED_TOKEN_X), ')');
        else
            tok = getrowtok();
    }
}

/****************************************************************************
 *                                                                          *
 * Function: pragma_codepage                                                *
 *                                                                          *
 * Purpose : Handle #pragma code_page( cp ).                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-12-10  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void pragma_codepage(void)
{
    if ((tok = getrowtok()) != '(')
    {
        apperror(RCWARNING1(ERROR_EXPECTED_TOKEN_X), '(');
    }
    else if ((tok = getrowtok()) == ')')
    {
        options.codepage = -1;  /* default */
        tok = getrowtok();
    }
    else
    {
        if (tok == ICONST)
        {
            int codepage = (toksym->type->op == INT_) ? (int)toksym->u.c.v.i : (int)toksym->u.c.v.u;

            if (!IsValidCodePage(codepage))
                apperror(RCWARNING1(ERROR_INVALID_CODEPAGE), codepage);
            else
                options.codepage = codepage;

            tok = getrowtok();
        }

        if (tok != ')')
            apperror(RCWARNING1(ERROR_EXPECTED_TOKEN_X), ')');
        else
            tok = getrowtok();
    }
}

/****************************************************************************
 *                                                                          *
 * Function: getrowtok                                                      *
 *                                                                          *
 * Purpose : Return next token, but don't leave the current line.           *
 *                                                                          *
 * Comment : Since we call gettok(), the src struct will be updated.        *
 *           This means that any error message we will produce,             *
 *           contains the correct filename and line number.                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int getrowtok(void)
{
    while (*cp == ' ' || *cp == '\t')
        cp++;

    if (*cp == '\n' || *cp == '\0')
        return EOI;
    else
        return gettok();
}

/****************************************************************************
 *                                                                          *
 * Function: is_initexit_function                                           *
 *                                                                          *
 * Purpose : Check if the type is a valid startup or exit function.         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           03-08-01  Created                                              *
 *                                                                          *
 ****************************************************************************/

static bool_t is_initexit_function(TYPE *ty)
{
    /* must be void function(void) or void function() */
    return (ty && isfunc(ty) && ty->type == voidtype && (ty->u.fcn.prototype == 0 || !ty->u.fcn.prototype[0]));
}
