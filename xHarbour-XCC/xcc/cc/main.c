/****************************************************************************
 *                                                                          *
 * File    : main.c                                                         *
 *                                                                          *
 * Purpose : ISO C Compiler; Main module.                                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           02-06-17  All messages moved to the resources.                 *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "lcc.h"

#pragma comment(lib, "user32")

/* Define the MS C version we most closely resembles;

   Compiler                         _MSC_VER
   --------                         --------
   Visual C++, 32-bit, version 1.0    800
   Visual C++, 32-bit, version 2.x    900
   Visual C++, 32-bit, version 4.0    1000
   Visual C++, 32-bit, version 5.0    1100
   Visual C++, 32-bit, version 6.0    1200
*/
#define MSC_VERSTR  "1100"

/* Maximum errors before abortion */
#define ERROR_LIMIT  100

#ifdef PROF
int findfunc(char *name, char *file) { return -1; (name); (file); }
int findcount(char *file, int x, int y) { return -1; (file); (x); (y); }
#endif

struct options options = {0};

HANDLE hmod = 0;

INTERFACE *IR = NULL;
OUTFMT *OF = NULL;

int nerrs = 0;

FILE *fdo;

#ifdef PRERELEASE
size_t tot_size = 0;
size_t cur_size = 0;
size_t tot_lines = 0;
#endif

/* Locals */
static char *outfile;
#if !(defined(XHARBOUR) && defined(LIBMAIN))
static HANDLE hThread;
#endif /* XHARBOUR && LIBMAIN */

/* Static function prototypes */
int __cdecl main(int, char **);
static void typedbg(SYMBOL *, void *);
static void cc_init(void);
static void cc_args(int, char **);
static void cc_usage(void);
static BOOL WINAPI ctrl_handler(DWORD);
static int my_exception_filter(struct _EXCEPTION_POINTERS *);

/****************************************************************************
 *                                                                          *
 * Function: main                                                           *
 *                                                                          *
 * Purpose : Main entry point, of course!                                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-12-13  Added 'catch all' exception handler.                 *
 *                                                                          *
 ****************************************************************************/

#if defined(XHARBOUR) && defined(LIBMAIN)
#include <setjmp.h>
static jmp_buf libenv;

BOOL WINAPI DllMain(HANDLE hDLL, DWORD dwReason, LPVOID lpReserved)
{
    hmod = hDLL;
    return TRUE;
}

__declspec(dllexport) int __cdecl libmain(int argc, char **argv)
{
    int code;

    if ((code = setjmp(libenv)) != 0)
        return code;
#elif defined(XHARBOUR)
int __cdecl main(int argc, char **argv)
{
    /* ugly non-standard way to force 'Micro$oft extensions' on by default */
    *(argv--) = "-Ze", ++argc;
#else /* !XHARBOUR && !LIBMAIN */
int __cdecl main(int argc, char **argv)
{
    __try
    {
#endif /* !XHARBOUR && !LIBMAIN */

#if defined(PODEBUG) && defined(__POCC__) && !defined(XHARBOUR)
    crt_leaktrace();
#endif

    cc_init();
    cc_args(argc, argv);

    if (options.preproc)  /* preprocess only */
    {
        uchar_t *buf = my_alloc(BUFSIZE+MAXLINE);
        size_t bsize;

        while ((bsize = pp_read(buf, BUFSIZE)) != 0)
            fwrite(buf, bsize, 1, stdout);

        my_free(buf);
        return 0;
    }
    else if (options.assemble)  /* assemble only */
    {
        uchar_t *buf = my_alloc(BUFSIZE+MAXLINE);
        size_t bsize, tsize = 0;

        while ((bsize = pp_read(buf+tsize, BUFSIZE-tsize)) != 0)
        {
            uchar_t *p, *q;

            buf[tsize + bsize] = '\0';
            for (q = buf; (p = (uchar_t *)strchr((char *)q, '\n')) != NULL; q = p+1)
                aslist = listappend(stringn((char *)q, p-q), aslist);

            tsize = buf + tsize + bsize - q;
            memmove(buf, q, tsize);
        }

        my_free(buf);
    }
    else  /* compile */
    {
        cc_input_init();
        tok = gettok();

        (*IR->progbeg)();

        if (options.dbglevel > 0 && IR->dbginit)
            (*IR->dbginit)(firstfile, outfile);

        parse_program();

#ifdef PROF
        if (events.end)
            apply(events.end, NULL, NULL);

        memset(&events, 0, sizeof(events));
#endif

#ifdef XREF
        if (options.dbglevel > 1 || options.xreflevel > 1)
#else
        if (options.dbglevel > 1)
#endif
        {
            SYMBOL *symroot = NULL;
            COORDINATE src;

            for_each_symbol(types, GLOBAL, typedbg, &symroot);
            for_each_symbol(identifiers, GLOBAL, typedbg, &symroot);

            src.file = firstfile;
            src.x = 0;
            src.y = lineno;

#ifdef XREF
            if (options.xreflevel > 0 && IR->dbgend)
                (*IR->dbgend)(&src, symroot, listvector(&loci, PERM), listvector(&symbols, PERM), NULL);
            else if (IR->dbgend)
                (*IR->dbgend)(&src, NULL, NULL, NULL, NULL);
#else
            if (IR->dbgend)
                (*IR->dbgend)(&src, NULL, NULL, NULL, NULL);
#endif
        }
        else if (options.dbglevel > 0 && IR->dbgend)
            (*IR->dbgend)(&src, NULL, NULL, NULL, NULL);

        finalize();

        (*IR->progend)();
    }

    if (nerrs != 0)
        errorexit(1);

    if (OF != NULL)
    {
        if (OF->assemble != NULL)
            (*OF->assemble)();
        if (fdo)
            fclose(fdo);
    }

    memfree(PERM);

#ifdef PRERELEASE
    printf("Max allocation: %u\n", tot_size);
    printf("Current allocation: %u\n", cur_size);
    printf("Total source lines: %u\n", tot_lines);
#endif

#if defined(PODEBUG) && defined(__POCC__) && !defined(XHARBOUR)
    extern void __cdecl heap_verify(int);
    heap_verify(0);

    /**/
    {
        FILE *fp = fopen("memtrace.log", "w");
        crt_dumpleaktrace(fp);
        fclose(fp);
    }
#endif

    if (nerrs != 0)
        errorexit(1);

#if !defined(XHARBOUR) && !defined(LIBMAIN)
    }
    __except (my_exception_filter(GetExceptionInformation()))
    {
    }
#endif /* !XHARBOUR && !LIBMAIN */

    return 0;
}

/****************************************************************************
 *                                                                          *
 * Function: typedbg                                                        *
 *                                                                          *
 * Purpose : Emit debugging information.                                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void typedbg(SYMBOL *sym, void *cl)
{
    if (*(SYMBOL **)cl == 0 && sym->sclass && sym->sclass != TYPEDEF)
        *(SYMBOL **)cl = sym;

    if ((sym->sclass == TYPEDEF || sym->sclass == 0) && IR->dbgtype)
        (*IR->dbgtype)(sym);
}

/****************************************************************************
 *                                                                          *
 * Function: cc_init                                                        *
 *                                                                          *
 * Purpose : Program initialization.                                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void cc_init(void)
{
#if !(defined(XHARBOUR) && defined(LIBMAIN))
    /* Make a copy of the thread handle, for ctrl_handler() */
    if (!DuplicateHandle(GetCurrentProcess(), GetCurrentThread(),
        GetCurrentProcess(), &hThread, 0, FALSE, DUPLICATE_SAME_ACCESS))
        apperror(RCFATAL(ERROR_INTERNAL), "cc_init()");

    SetConsoleCtrlHandler(ctrl_handler, TRUE);
#endif /* XHARBOUR && LIBMAIN */

    select_binding("x86-coff");

    /* Before -I or -D arguments */
    pp_init();
}

/****************************************************************************
 *                                                                          *
 * Function: cc_args                                                        *
 *                                                                          *
 * Purpose : Decode command line or environment options.                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           03-10-19  Added option -Go (use oldnames.lib).                 *
 *           03-10-28  Added option -Zx (enable Pelles C extensions).       *
 *           04-03-03  Added option -Op[-] (improve f.p. consistency).      *
 *           04-03-14  Added option -Gn (no mangled names).                 *
 *           04-05-19  Added option -V<n> (set verbosity level).            *
 *           04-06-17  Added Microsoft mode macros: _INTEGRAL_MAX_BITS,     *
 *                     _PUSHPOP_SUPPORTED and _STDCALL_SUPPORTED.           *
 *           04-06-22  Added aliases for /Ot and /Os: /O2 and /O1.          *
 *           04-06-27  Added option -MD (use runtime DLL).                  *
 *           04-07-02  Added option -Gr (fastcall).                         *
 *           04-07-12  Only define __STDC__ macros if not Microsoft mode.   *
 *           04-08-18  Bugfix: changed _CHAR_UNSIGNED to _CHAR_UNSIGNED=1.  *
 *           04-11-05  Added option -Gm (true unmangle).                    *
 *                                                                          *
 ****************************************************************************/

static void cc_args(int argc, char **argv)
{
    bool_t unschr = FALSE;  /* default char is signed */
    int calltype = CDECL_;
    int fltopt = -1;
    char objfile[MAXPATH] = "";
    char *srcfile = NULL;
    char *s;

    options.defaultlib = TRUE;
    options.warnlevel = 1;
    options.infolevel = 0;

    while (--argc > 0 && *(++argv))
    {
        if (**argv == '-' || **argv == '/')
        {
            s = (*argv)+1;

            while (*s)
            {
                switch (*s++)
                {
                    case '?':
                        cc_usage();
                        break;

                    case 'c':
                        if (*s == '\0')  /* compile only */
                            continue;
                        else if (strcmp(s, "bstring") == 0)
                            options.cbstring = TRUE;
                        else
                            apperror(RCFATAL(ERROR_UNKNOWN_OPTION), --s);
                        break;
#ifdef PROF
                    case 'b':
                        options.prof = TRUE;
                        continue;
#endif
#ifdef PODEBUG
                    case 'd':
                        options.debug = TRUE;
                        continue;
#endif
                    case 'v':
                        options.verbose = TRUE;
                        continue;

                    case 'E':
                        options.preproc = TRUE;
                        continue;
#ifdef XREF
                    case 'x':
                        options.xreflevel = 1;
                        continue;
#endif
                    case 'J':
                        unschr = TRUE;
                        continue;

                    case 'X':
                        delete_includes();
                        continue;

                    case 'I':
                        if (*s == '\0')
                        {
                            if (--argc == 0)
                                apperror(RCFATAL(ERROR_OPTION_ARG_MISSING), s-1);
                            s = *++argv;
                        }
                        add_include(s, INC_STDPLACE, FALSE);
                        break;

                    case 'D':
                    case 'U':
                        if (*s == '\0')
                        {
                            if (--argc == 0)
                                apperror(RCFATAL(ERROR_OPTION_ARG_MISSING), s-1);
                            s = *++argv;
                        }
                        pp_define(s, s[-1]);
                        break;

                    case 'F':
                        if (*s == 'o')
                        {
                            if (*++s == '\0')
                            {
                                if (--argc == 0)
                                    apperror(RCFATAL(ERROR_OPTION_ARG_MISSING), "Fo");
                                s = *++argv;
                            }
                            strcpy(objfile, s);
                            break;
                        }
                        apperror(RCFATAL(ERROR_UNKNOWN_OPTION), --s);
                        break;

                    case 'G':
                        if (*s == 'd')
                            calltype = CDECL_;
                        else if (*s == 'h')
                            options.hookcall = TRUE;
                        else if (*s == 'm')
                            options.truenomangle = TRUE;
                        else if (*s == 'n')
                            options.nomangle = TRUE;
                        else if (*s == 'r')
                            calltype = FASTCALL_;
                        else if (*s == 'z')
                            calltype = STDCALL_;
#ifndef XHARBOUR
                        else if (*s == 'o')
                        {
                            options.oldnames = TRUE;
                            pp_define("__POCC__OLDNAMES", 'D');
                        }
#endif /* XHARBOUR */
                        else
                            apperror(RCFATAL(ERROR_UNKNOWN_OPTION), --s);
                        break;

                    case 'M':
                        if (*s == 'T' || *s == 'D')
                        {
                            /* enable multi-threading */
                            options.multithread = TRUE;
                            pp_define("__MT__", 'D');
                            if (options.microsoft) pp_define("_MT", 'D');
                            /* use dynamic runtime? */
                            if (*s == 'D')
                            {
                                options.crtdll = TRUE;
                                pp_define("_DLL", 'D');
                            }
                        }
                        else
                        {
                            apperror(RCFATAL(ERROR_UNKNOWN_OPTION), --s);
                        }
                        break;

                    case 'O':
                        if (*s == 'p')
                            fltopt = *++s == '-';
                        else if (*s == 's' || *s == '1')  /* -Os or -O1 */
                            options.optimize = MINSPACE;
                        else if (*s == 't' || *s == '2')  /* -Ot or -O2 */
                            options.optimize = MAXSPEED;
                        else if (*s == 'x')
                            options.maxopt = TRUE;
                        else
                            apperror(RCFATAL(ERROR_UNKNOWN_OPTION), --s);
                        options.pragmaopt = options.optimize;
                        break;

                    case 'T':
                        if (*s == '\0' || *s == '?')
                        {
                            printmsg(MSG_TARGET_LIST);
                            list_bindings();
                            errorexit(1);
                        }
                        else if (!select_binding(s))
                            apperror(RCFATAL(ERROR_UNKNOWN_OPTION), --s);
                        break;

                    case 'Z':
                        if (*s == 'g')
                        {
                            /* generate function prototypes */
                            select_binding("null");
                            options.printproto = TRUE;
                        }
                        else if (*s == 's')
                        {
                            /* syntax check only */
                            select_binding("null");
                        }
                        else if (*s == 'd')
                        {
                            /* enable line number debugging information */
                            if (options.dbglevel < 1)
                                options.dbglevel = 1;
                        }
                        else if (*s == 'i')
                        {
                            /* enable full debugging information */
                            if (options.dbglevel < 2)
                                options.dbglevel = 2;
                        }
                        else if (*s == 'e')
                        {
                            /* enable Micro$oft extensions */
                            options.microsoft = TRUE;
                            pp_define("_MSC_EXTENSIONS=1", 'D');
                            pp_define("_MSC_VER=" MSC_VERSTR, 'D');
                            pp_define("_INTEGRAL_MAX_BITS=64", 'D');
                            pp_define("_PUSHPOP_SUPPORTED=1", 'D');
                            pp_define("_STDCALL_SUPPORTED=1", 'D');
                            /* should be real typedef's, but have to do for now */
                            pp_define("__int8=char", 'D');
                            pp_define("__int16=short", 'D');
                            pp_define("__int32=int", 'D');
                            pp_define("__int64=long long", 'D');
                            if (options.multithread) pp_define("_MT", 'D');
                        }
                        else if (*s == 'l')
                        {
                            options.defaultlib = FALSE;
                        }
                        else if (*s == 'x')
                        {
                            options.extensions = TRUE;
                        }
                        else
                        {
                            apperror(RCFATAL(ERROR_UNKNOWN_OPTION), --s);
                        }
                        break;

                    case 'V':
                        if (*s == '0')
                            options.infolevel = 0;
                        else if (*s == '1')
                            options.infolevel = 1;
                        else if (*s == '2')
                            options.infolevel = 2;
                        else
                            apperror(RCFATAL(ERROR_UNKNOWN_OPTION), --s);
                        break;

                    case 'W':
                        if (*s == '0')
                            options.warnlevel = 0;
                        else if (*s == '1')
                            options.warnlevel = 1;
                        else if (*s == '2')
                            options.warnlevel = 2;
                        else
                            apperror(RCFATAL(ERROR_UNKNOWN_OPTION), --s);
                        break;

                    default:
                        apperror(RCFATAL(ERROR_UNKNOWN_OPTION), --s);
                        break;
                }

                /* always exit while */
                break;
            }
        }
        else
        {
            if (!srcfile)
                srcfile = *argv;
        }
    }

    if (!srcfile)
        cc_usage();

    if (options.maxopt && options.optimize == DONTOPT)
        options.pragmaopt = options.optimize = MAXSPEED;

    if (options.microsoft)
    {
        extern INTERFACE x86IR;  /* Intel target */
        extern INTERFACE armIR;  /* ARM target */
        if (IR == &x86IR) pp_define(options.maxopt ? "_M_IX86=600" : "_M_IX86=300", 'D');  /* architecture Pentium II+ or 386+ */
        if (IR == &armIR) pp_define("_M_ARM=4", 'D');  /* architecture 4 (StrongARM) */
    }
    else
    {
        /* define __STDC__ macros */
        setup_kwtab(TRUE);
    }

    /* set default calling convention */
    options.calltype = (options.microsoft) ? calltype : CDECL_;

    /* use explicit option or turn on in Micro$oft mode */
    options.fltopt = (fltopt != -1) ? fltopt : options.microsoft;

    /* default is no wide char translation */
    options.codepage = -1;

#ifdef XREF
    if (options.xreflevel > 0)
    {
        extern INTERFACE x86IR;  /* Intel target */
        extern INTERFACE armIR;  /* ARM target */
        if (IR == &x86IR) select_binding("x86-xref");  /* X86 -> X86 cross reference binding */
        if (IR == &armIR) select_binding("arm-xref");  /* ARM -> ARM cross reference binding */
    }
#endif

    /* before type initialization */
    if (unschr && IR != NULL)
    {
        IR->unsigned_char = 1;
        pp_define("_CHAR_UNSIGNED=1", 'D');
    }

    /* when we know about signed/unsigned chars */
    setup_basic_types();

#ifdef PROF
    /* after type initialization */
    if (options.prof && IR != NULL)
        prof_init();
#endif

    if (OF != NULL)
    {
        if (OF->init != NULL)
            (*OF->init)();

        if (OF->assemble != NULL)
        {
            /*
             * Let the output driver see the input name and also,
             * when needed, create a default output name.
             */
            (*OF->filename)(objfile, srcfile);

            fdo = fopen(objfile, OF->binary_mode ? "wb" : "wt");
            if (!fdo) apperror(RCFATAL(ERROR_CANNOT_CREATE_FILE), objfile);

            /* Remember name for errorexit() */
            outfile = my_strdup(objfile);

            /* Set silly Micro$oft flag */
            options.fltuse = options.microsoft;
        }
    }

    /* use extension to filter out assembly files */
    s = strrchr(srcfile, '.');
    if (s != 0 && (_stricmp(s, ".asm") == 0 || _stricmp(s, ".s") == 0))
        options.assemble = TRUE;

    pp_start(srcfile);
}

/****************************************************************************
 *                                                                          *
 * Function: cc_usage                                                       *
 *                                                                          *
 * Purpose : Display usage information.                                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void cc_usage(void)
{
    printmsg(MSG_USAGE_TITLE);
    printmsg(MSG_USAGE_COPYRIGHT);
    printmsg(MSG_USAGE_SYNTAX, PROGRAM_NAME);
    printmsg(MSG_USAGE_OPTIONS);
#ifdef PROF
    printmsg(MSG_USAGE_OPTION_B);
#endif
#ifdef PODEBUG
    printf("/d *** DEBUG *** DEBUG *** DEBUG *** DEBUG *** DEBUG ***\n");
#endif
    printmsg(MSG_USAGE_OPTION_D);
    printmsg(MSG_USAGE_OPTION_E);
    printmsg(MSG_USAGE_OPTION_FO);
    printmsg(MSG_USAGE_OPTION_GD);
    printmsg(MSG_USAGE_OPTION_GH);
    printmsg(MSG_USAGE_OPTION_GM);
    printmsg(MSG_USAGE_OPTION_GN);
#ifndef XHARBOUR
    printmsg(MSG_USAGE_OPTION_GO);
#endif /* XHARBOUR */
    printmsg(MSG_USAGE_OPTION_GR);
    printmsg(MSG_USAGE_OPTION_GZ);
    printmsg(MSG_USAGE_OPTION_I);
    printmsg(MSG_USAGE_OPTION_J);
    printmsg(MSG_USAGE_OPTION_MD);
    printmsg(MSG_USAGE_OPTION_MT);
    printmsg(MSG_USAGE_OPTION_O1);
    printmsg(MSG_USAGE_OPTION_O2);
    printmsg(MSG_USAGE_OPTION_OP);
    printmsg(MSG_USAGE_OPTION_OS);
    printmsg(MSG_USAGE_OPTION_OT);
    printmsg(MSG_USAGE_OPTION_OX);
    printmsg(MSG_USAGE_OPTION_T);
    printmsg(MSG_USAGE_OPTION_U);
    printmsg(MSG_USAGE_OPTION_V);
    printmsg(MSG_USAGE_OPTION_W);
    printmsg(MSG_USAGE_OPTION_X);
    printmsg(MSG_USAGE_OPTION_ZD);
    printmsg(MSG_USAGE_OPTION_ZE);
    printmsg(MSG_USAGE_OPTION_ZG);
    printmsg(MSG_USAGE_OPTION_ZI);
    printmsg(MSG_USAGE_OPTION_ZL);
    printmsg(MSG_USAGE_OPTION_ZS);
    printmsg(MSG_USAGE_OPTION_ZX);

    errorexit(1);
}

/****************************************************************************
 *                                                                          *
 * Function: apperror                                                       *
 *                                                                          *
 * Purpose : Display error message. Terminate program if it's fatal.        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           02-06-17  Rewritten to support resource messages.              *
 *           04-07-10  Added support for disabled warnings.                 *
 *                                                                          *
 ****************************************************************************/

void apperror(WINERR err, ...)
{
    if (ERRNUM(err) != ERROR_SUCCESS)
    {
        char fmt[256];
        va_list ap;

        /* ignore any unwanted warnings */
        if (ISWARNING(err) && (WARNLEVEL(err) > options.warnlevel || disabled_warning(err)))
            return;

        if (src.file && src.y)
            printmsg(MSG_ERRTEXT_FILE_LINE, src.file, src.y);

        /* C language warnings and errors contains the number */
        if (ERRNUM(err) >= CLANG_ERRNUM_MIN && ERRNUM(err) < CLANG_ERRNUM_MAX)
            sprintf(fmt, " #%u", ERRNUM(err));
        else
            *fmt = '\0';

        if (ISFATAL(err)) printmsg(MSG_ERRTEXT_FATAL, fmt);
        if (ISERROR(err)) printmsg(MSG_ERRTEXT_ERROR, fmt);
        if (ISWARNING(err)) printmsg(MSG_ERRTEXT_WARNING, fmt);

        /* load message from the resources */
        if (!FormatMessage(FORMAT_MESSAGE_FROM_HMODULE|FORMAT_MESSAGE_IGNORE_INSERTS, hmod,
            err & ~(ERROR_SEVERITY_FATAL|ERROR_SEVERITY_ERROR|ERROR_SEVERITY_WARNING|ERROR_SEVERITY_WARNING2),
            0, fmt, NELEMS(fmt), NULL))
        {
            sprintf(fmt, "*** No message for error 0x%X ***", err);
        }

        CharToOemA(fmt, fmt);

        va_start(ap, err);
        vfprint(stdout, NULL, fmt, ap);
        va_end(ap);

        fflush(stdout);

        if (ISFATAL(err)) errorexit(1);
        if (ISERROR(err)) nerrs++;

        if (nerrs > ERROR_LIMIT)
            apperror(RCFATAL(ERROR_TOO_MANY_ERRORS), ERROR_LIMIT);
    }
}

/****************************************************************************
 *                                                                          *
 * Function: printmsg                                                       *
 *                                                                          *
 * Purpose : Display private message.                                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           02-06-17  Created                                              *
 *                                                                          *
 ****************************************************************************/

void printmsg(int msg, ...)
{
    char fmt[256];
    va_list ap;

    if (!FormatMessage(FORMAT_MESSAGE_FROM_HMODULE|FORMAT_MESSAGE_IGNORE_INSERTS, hmod,
        msg, 0, fmt, NELEMS(fmt), NULL))
        *fmt = '\0';

    CharToOemA(fmt, fmt);

    va_start(ap, msg);
    vfprint(stdout, NULL, fmt, ap);
    va_end(ap);
}

/****************************************************************************
 *                                                                          *
 * Function: errorexit                                                      *
 *                                                                          *
 * Purpose : Bail out.                                                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void errorexit(int code)
{
    if (OF != NULL) (*OF->fileend)(TRUE);

    if (fdo) fclose(fdo);
    if (outfile) remove(outfile);

#if defined(XHARBOUR) && defined(LIBMAIN)
    fflush(stdout);
    longjmp(libenv, code);
#else
    exit(code);
#endif /* XHARBOUR && LIBMAIN */
}

/****************************************************************************
 *                                                                          *
 * Function: ctrl_handler                                                   *
 *                                                                          *
 * Purpose : Ctrl+C and Ctrl+Break handler for console apps.                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#if !(defined(XHARBOUR) && defined(LIBMAIN))
static BOOL WINAPI ctrl_handler(DWORD dwCtrlType)
{
    UNREFERENCED_PARAMETER(dwCtrlType);

    printmsg(MSG_CTRL_BREAK, PROGRAM_NAME);

    /*
     * By definition (according to Microsoft), a Ctrl-Break
     * handler is running as a *new* thread.
     *
     * Make sure we suspend the main thread, so it doesn't
     * continue to run when we call the cleanup code, in
     * errorexit(). If we don't do this, the app will very
     * likely blow up, due to some access violation.
     *
     * Note that this requires single threading, or the
     * following code will dead-lock (no time to investigate
     * why, right now).
     */
    SuspendThread(hThread);

#ifdef _MT
//Ron Pinkas commented#error ctrl_handler() requires single threading.
#endif

    errorexit(1);
    return 1;
}
#endif /* XHARBOUR && LIBMAIN */

/****************************************************************************
 *                                                                          *
 * Function: my_exception_filter                                            *
 *                                                                          *
 * Purpose : Generic exception filter.                                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-12-13  Created                                              *
 *                                                                          *
 ****************************************************************************/

#if !defined(XHARBOUR) && !defined(LIBMAIN)
static int my_exception_filter(struct _EXCEPTION_POINTERS *exp)
{
    const char *desc;
    char buf[80];

    switch (exp->ExceptionRecord->ExceptionCode)
    {
        case EXCEPTION_ACCESS_VIOLATION: desc = "Access violation"; break;
        case EXCEPTION_FLT_DENORMAL_OPERAND: desc = "Float denormal operand"; break;
        case EXCEPTION_FLT_DIVIDE_BY_ZERO: desc = "Float divide by zero"; break;
        case EXCEPTION_FLT_INEXACT_RESULT: desc = "Float inexact result"; break;
        case EXCEPTION_FLT_INVALID_OPERATION: desc = "Float invalid operation"; break;
        case EXCEPTION_FLT_OVERFLOW: desc = "Float overflow"; break;
        case EXCEPTION_FLT_STACK_CHECK: desc = "Float stack check"; break;
        case EXCEPTION_FLT_UNDERFLOW: desc = "Float underflow"; break;
        case EXCEPTION_INT_DIVIDE_BY_ZERO: desc = "Integer divide by zero"; break;
        case EXCEPTION_INT_OVERFLOW: desc = "Integer overflow"; break;
        case EXCEPTION_IN_PAGE_ERROR: desc = "In page error"; break;
        case EXCEPTION_ILLEGAL_INSTRUCTION: desc = "Illegal instruction"; break;
        case EXCEPTION_NONCONTINUABLE_EXCEPTION: desc = "Noncontinuable exception"; break;
        case EXCEPTION_STACK_OVERFLOW: desc = "Stack overflow"; break;
        case EXCEPTION_INVALID_DISPOSITION: desc = "Invalid disposition"; break;
        case EXCEPTION_GUARD_PAGE: desc = "Guard page violation"; break;
        default: desc = "Unknown"; break;
    }

    sprintf(buf, "'%s' at 0x%p", desc, exp->ExceptionRecord->ExceptionAddress);

#if defined(PODEBUG)
    apperror(RCERROR(ERROR_INTERNAL), buf);
    return EXCEPTION_CONTINUE_SEARCH;
#else
    apperror(RCFATAL(ERROR_INTERNAL), buf);
    return EXCEPTION_EXECUTE_HANDLER;
#endif
}
#endif /* !XHARBOUR && !LIBMAIN */
