/****************************************************************************
 *                                                                          *
 * File    : main.c                                                         *
 *                                                                          *
 * Purpose : Win32 Library Manager; main module.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *           98-01-18  Several new messages added.                          *
 *           02-06-16  All messages moved to the resources.                 *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop
#include <limits.h>

#include "lib.h"

enum opttok {
    C_NOTHING,
    C_DEF,
    C_EXPLODE,
    C_EXTRACT,
    C_HELP,
    C_LIST,
    C_MACHINE,
    C_OLDIMPLIB,
    C_OLDNAMES,
    C_OUT,
    C_REMOVE,
    C_VERBOSE
};

struct opts {
    enum opttok tok;
    char *name;
    bool_t has_args;
};

/* global options array */
static const struct opts opts[] =
{
    C_DEF,          "DEF",          TRUE,
    C_EXPLODE,      "EXPLODE",      FALSE,
    C_EXTRACT,      "EXTRACT",      TRUE,
    C_HELP,         "HELP",         FALSE,
    C_HELP,         "?",            FALSE,
    C_LIST,         "LIST",         FALSE,
    C_MACHINE,      "MACHINE",      TRUE,
    C_OLDIMPLIB,    "OLDIMPLIB",    FALSE,
    C_OLDNAMES,     "OLDNAMES",     FALSE,  /* undocumented hack for oldnames.lib */
    C_OUT,          "OUT",          TRUE,
    C_REMOVE,       "REMOVE",       TRUE,
    C_VERBOSE,      "VERBOSE",      FALSE
};

int nerrs = 0;

FILEINFO *obj_list = NULL;
FILEINFO *lib_file = NULL;
FILEINFO *tmp_file = NULL;

NAMENTRY *extract_list = NULL;
NAMENTRY *remove_list = NULL;

time_t time_stamp;

struct options options = {0};

static HANDLE hThread;

/* Static function prototypes */
static void lib_init_1(void);
static void lib_init_2(void);
static enum opttok lookup_option(char **);
static void add_file_to_list(FILEINFO **, const char *);
static void add_extension_to_file(char *, const char *);
static void parse_module_definition(const char *, char *);
static void print_usage(void);
static BOOL WINAPI ctrl_handler(DWORD);

/****************************************************************************
 *                                                                          *
 * Function: main                                                           *
 *                                                                          *
 * Purpose : Main entry point, of course!                                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

int __cdecl main(int argc, char **argv)
{
    lib_init_1();

    /* Process command line args */
    lib_args(--argc, ++argv);

    lib_init_2();

    /* Perform requested operation(s) */
    if (options.create) create_archive_file();
    if (options.explode) explode_archive_file();
    if (options.extract) extract_archive_members();
    if (options.list) list_archive_file();

    if (nerrs != 0)
        errorexit(1);

    return 0;
}

/****************************************************************************
 *                                                                          *
 * Function: lib_init_1                                                     *
 *                                                                          *
 * Purpose : Program initialization; part 1.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *           99-08-14  Thread handle duplication added.                     *
 *                                                                          *
 ****************************************************************************/

static void lib_init_1(void)
{
    char tempname[MAX_PATH];

    /* Make a copy of the thread handle, for ctrl_handler() */
    if (!DuplicateHandle(GetCurrentProcess(), GetCurrentThread(),
        GetCurrentProcess(), &hThread, 0, FALSE, DUPLICATE_SAME_ACCESS))
        apperror(RCFATAL(ERROR_INTERNAL), "lib_init_1");

    SetConsoleCtrlHandler(ctrl_handler, TRUE);

    SetFileApisToOEM();

    sprintf(tempname, "~%.7lX.TMP", GetTickCount());
    lookup_file(&tmp_file, tempname);

    time_stamp = time(NULL);

    options.machine = MACHINE_UNKNOWN;
}

/****************************************************************************
 *                                                                          *
 * Function: lib_init_2                                                     *
 *                                                                          *
 * Purpose : Program initialization; part 2.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void lib_init_2(void)
{
    if (!lib_file)
    {
        if (!obj_list)
            apperror(RCFATAL(ERROR_NO_ARCHIVE_FILE));

        /* No explicit archive name; assume the first file */
        add_file_to_list(&lib_file, obj_list->name);
        remove_file_from_list(&obj_list, obj_list);
    }

    if (obj_list != NULL)
        options.create = TRUE;

    if (export_list != NULL && *export_fname == '\0')
        apperror(RCFATAL(ERROR_NO_EXPORT_FILE));
}

/****************************************************************************
 *                                                                          *
 * Function: lib_args                                                       *
 *                                                                          *
 * Purpose : Decode command line or environment options.                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *           98-01-07  Changed where, and when, we call print_usage().      *
 *           98-01-18  Moved some code to lib_init_2().                     *
 *           98-11-07  Option /OLDIMPLIB added.                             *
 *           99-01-29  Response file handling improved.                     *
 *           03-10-19  Undocumented option /OLDNAMES added.                 *
 *           04-01-12  Options /DEF and /MACHINE added.                     *
 *                                                                          *
 ****************************************************************************/

void lib_args(int argc, char **argv)
{
    char *s;

    if (argc == 0)
        print_usage();

    for (; argc && *argv; argv++, argc--)
    {
        if (**argv == '-' || **argv == '/')
        {
            s = ++*argv;

            switch (lookup_option(&s))
            {
                char fname[MAX_PATH];

                case C_DEF:
                {
                    char *p = read_respfile(s, '\n');
                    if (!p) apperror(RCFATAL(ERROR_CANT_READ_DEFFILE), s);
                    parse_module_definition(s, p);
                    my_free(p);
                    options.create = TRUE;
                    break;
                }

                case C_EXPLODE:
                    options.explode = TRUE;
                    options.extract = FALSE;
                    break;

                case C_MACHINE:
                    if (_stricmp(s, "IX86") == 0)
                        options.machine = MACHINE_X86;
                    else if (_stricmp(s, "ARM") == 0)
                        options.machine = MACHINE_ARM;
                    else
                        apperror(RCFATAL(ERROR_MACHINE_OPTION));
                    break;

                case C_EXTRACT:
                    lookup_name(&extract_list, s);
                    options.extract = TRUE;
                    break;

                case C_REMOVE:
                    lookup_name(&remove_list, s);
                    options.create = TRUE;
                    break;

                case C_OLDIMPLIB:
                    options.old_implib = TRUE;
                    break;

                case C_OLDNAMES:
                    options.old_names = TRUE;
                    break;

                case C_OUT:
                    add_extension_to_file(strcpy(fname, s), EXT_LIB);
                    remove_file_from_list(&lib_file, lib_file);
                    lookup_file(&lib_file, fname);
                    break;

                case C_LIST:
                    options.list = TRUE;
                    break;

                case C_VERBOSE:
                    options.verbose = TRUE;
                    break;

                case C_HELP:
                    print_usage();
                    break;

                default:
                    apperror(RCWARNING(ERROR_UNKNOWN_OPTION), s);
                    break;
            }
        }
        else
        {
            if (**argv == '@')
            {
                char **eargv;
                int eargc;

                s = read_respfile((*argv)+1, '\t');
                if (!s) apperror(RCFATAL(ERROR_CANT_READ_CMDFILE), (*argv)+1);

                tokenize(s, &eargc, &eargv);
                lib_args(eargc, eargv);
                my_free(eargv);

                my_free(s);
            }
            else
            {
                add_file_to_list(&obj_list, *argv);
            }
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: lookup_option                                                  *
 *                                                                          *
 * Purpose : Lookup a option in the options array.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

static enum opttok lookup_option(char **opt)
{
    int i;

    for (i = 0; i < NELEMS(opts); i++)
    {
        if (_strnicmp(*opt, opts[i].name, strlen(opts[i].name)) == 0)
        {
            (*opt) += strlen(opts[i].name);

            /* Special treatment of options with arguments */
            if (opts[i].has_args && (**opt != ':' || *++*opt == '\0'))
            {
                apperror(RCFATAL(ERROR_OPTION_ARG_MISSING), opts[i].name);
            }

            return opts[i].tok;
        }
    }

    return C_NOTHING;
}

/****************************************************************************
 *                                                                          *
 * Function: add_file_to_list                                               *
 *                                                                          *
 * Purpose : Add a new filename to the given list. Expand wildcards.        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *           98-10-16  Bugfix: didn't handle drive without backslash.       *
 *                                                                          *
 ****************************************************************************/

static void add_file_to_list(FILEINFO **list, const char *filename)
{
    struct _finddata_t find;
    long handle;

    handle = _findfirst(filename, &find);
    if (handle == -1)
    {
        apperror(RCFATAL(ERROR_OPEN_FILE), filename);
    }

    do
    {
        char fname[MAX_PATH];
        char *s;
        size_t plen;
        FILEINFO *file;

        /* Combine path with the current filename */
        plen = (s = strrchr(filename, '\\')) != NULL || (s = strrchr(filename, ':')) != NULL ? (s-filename+1) : 0;
        strcpy(strncpy(fname, filename, plen) + plen, find.name);

        file = lookup_file(list, fname);
        file->time = find.time_write;
        file->size = find.size;

    } while (_findnext(handle, &find) == 0);
    _findclose(handle);
}

/****************************************************************************
 *                                                                          *
 * Function: add_extension_to_file                                          *
 *                                                                          *
 * Purpose : Add extension to filename without it.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void add_extension_to_file(char *filename, const char *ext)
{
    char *s;

    s = strrchr(filename, '\\');
    if (!strchr((s) ? s : filename, '.'))
        strcat(filename, ext);
}

/****************************************************************************
 *                                                                          *
 * Function: parse_module_definition                                        *
 *                                                                          *
 * Purpose : Parse a module definition (.DEF) file.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-01-12  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void parse_module_definition(const char *filename, char *p)
{
    /*
     * Microsoft syntax:
     *
     * ; comment
     * NAME [appname][BASE=address]
     * LIBRARY [libname][BASE=address]
     * DESCRIPTION "text"
     * STACKSIZE reserve[,commit]
     * SECTIONS
     *   name [CLASS 'classname'] [{EXECUTE|READ|WRITE|SHARED}]
     * EXPORTS
     *   entryname[=internalname] [@ordinal [NONAME]] [DATA] [PRIVATE]
     * VERSION major[.minor]
     *
     * Pelles C syntax:
     *
     * ; comment
     * NAME [appname]
     * LIBRARY [libname]
     * BASE address
     * DESCRIPTION "text"
     * STACKSIZE reserve[,commit]
     * HEAPSIZE reserve[,commit]
     * SECTIONS
     *   name [{EXECUTE|READ|WRITE|SHARED}]
     * EXPORTS
     *   entryname[=internalname] [@ordinal] [DATA]
     * VERSION major[.minor]
     */
    enum
    {
        ST_NOTHING = 0,
        ST_NAME,
        ST_LIBRARY,
        ST_DESCRIPTION,
        ST_BASE,
        ST_STACKSIZE,
        ST_HEAPSIZE,
        ST_VERSION,
        ST_SECTIONS,
        ST_EXPORTS
    } state = ST_NOTHING;

    while (*p != '\0')
    {
        /* Skip white-space and comments */
        if (*p == ' ' || *p == '\t' || *p == '\n')
            p++;
        else if (*p == ';')
        {
            while (*++p && *p != '\n')
                ;
        }
        /* Parse keywords and (un)quoted names */
        else if (*p == '.' || *p == '_' || *p == '\'' || *p == '"' || isalpha((unsigned char)*p))
        {
            char name[80], *np = name;
            int new_state;

            /* Quoted string */
            if (*p == '\'' || *p == '"')
            {
                char quote = *p++;
                while (*p != '\0' && *p != quote)
                    *np++ = *p++;
                *np = '\0';

                if (*p) p++;
                new_state = ST_NOTHING;
            }
            /* Keyword or name */
            else
            {
                do
                    *np++ = *p++;
                while (*p == '_' || isalnum((unsigned char)*p));
                *np = '\0';

                if (_stricmp(name, "NAME") == 0)
                    new_state = ST_NAME;
                else if (_stricmp(name, "LIBRARY") == 0)
                    new_state = ST_LIBRARY;
                else if (_stricmp(name, "DESCRIPTION") == 0)
                    new_state = ST_DESCRIPTION;
                else if (_stricmp(name, "STACKSIZE") == 0)
                    new_state = ST_STACKSIZE;
                else if (_stricmp(name, "HEAPSIZE") == 0)
                    new_state = ST_HEAPSIZE;
                else if (_stricmp(name, "SECTIONS") == 0)
                    new_state = ST_SECTIONS;
                else if (_stricmp(name, "SEGMENTS") == 0)  /* alias for SECTIONS */
                    new_state = ST_SECTIONS;
                else if (_stricmp(name, "EXPORTS") == 0)
                    new_state = ST_EXPORTS;
                else if (_stricmp(name, "VERSION") == 0)
                    new_state = ST_VERSION;
                else
                    new_state = ST_NOTHING;
            }

            switch (state)
            {
                case ST_NOTHING:
                    if (new_state == ST_NOTHING)
                    {
                        apperror(RCWARNING(ERROR_BAD_DEFFILE_KEYWORD), filename, name);
                        while (*p && *p != '\n') p++;  /* skip rest of line */
                    }
                    state = new_state;
                    break;

                case ST_NAME:
                    state = ST_NOTHING;
                    break;

                case ST_LIBRARY:
                    add_extension_to_file(strcpy(export_fname, name), EXT_DLL);
                    state = ST_NOTHING;
                    break;

                case ST_DESCRIPTION:
                    /* Ignore the description, for now */
                    state = ST_NOTHING;
                    break;

                case ST_SECTIONS:
                    if (new_state == ST_NOTHING)
                    {
                        for (;;)
                        {
                            while (*p == ' ' || *p == '\t')
                                p++;

                            if (isalpha((unsigned char)*p))
                            {
                                while (isalnum((unsigned char)*++p))
                                    ;
                            }
                            else break;
                        }
                    }
                    else
                        state = new_state;
                    break;

                case ST_EXPORTS:
                    if (new_state == ST_NOTHING)
                    {
                        int ordinal = 0;
                        bool_t is_func = TRUE;
                        EXPENTRY *exp;

                        if (_stricmp(name, "NONAME") == 0 || 
                            _stricmp(name, "PRIVATE") == 0)
                        {
                            /* Warn about unsupported Microsoft junk */
                            apperror(RCWARNING(ERROR_BAD_DEFFILE_KEYWORD), filename, name);
                            break;
                        }

                        while (*p == ' ' || *p == '\t')
                            p++;

                        if (*p == '=')
                        {
                            p++;
                            while (*p == ' ' || *p == '\t')
                                p++;

                            /* Quoted string */
                            if (*p == '\'' || *p == '"')
                            {
                                char quote = *p++;
                                while (*p != '\0' && *p++ != quote)
                                    ;
                            }
                            /* Name */
                            else if (*p == '_' || isalpha((unsigned char)*p))
                            {
                                while (*++p == '_' || *p == '@' || isalnum((unsigned char)*p))
                                    ;
                            }

                            while (*p == ' ' || *p == '\t')
                                p++;
                        }

                        if (*p == '@')
                        {
                            ordinal = strtol(++p, &p, 0);
                            if (ordinal < 1 || ordinal > USHRT_MAX)
                                apperror(RCFATAL(ERROR_BAD_DEFFILE_EXPORT), filename);

                            while (*p == ' ' || *p == '\t')
                                p++;
                        }

                        if (_strnicmp(p, "DATA", 4) == 0)
                        {
                            p += 4;
                            is_func = FALSE;
                        }

                        exp = lookup_export(name, ordinal, is_func);
                    }
                    else
                        state = new_state;
                    break;

                default:
                    apperror(RCFATAL(ERROR_BAD_DEFFILE_SYNTAX), filename, name);
                    break;
            }
        }
        /* Parse numeric value */
        else if (isdigit((unsigned char)*p))
        {
            char *q = p;
            long num = strtol(p, &p, 0);

            switch (state)
            {
                case ST_BASE:
                    state = ST_NOTHING;
                    break;

                case ST_STACKSIZE:
                    if (*p == ',')
                        num = strtol(++p, &p, 0);
                    state = ST_NOTHING;
                    break;

                case ST_HEAPSIZE:
                    if (*p == ',')
                        num = strtol(++p, &p, 0);
                    state = ST_NOTHING;
                    break;

                case ST_VERSION:
                    if (*p == '.')
                        num = strtol(++p, &p, 0);
                    state = ST_NOTHING;
                    break;

                default:
                    *p = '\0';
                    apperror(RCFATAL(ERROR_BAD_DEFFILE_SYNTAX), filename, q);
                    break;
            }
        }
        else p++;  /* avoid infinite loop */
    }
}

/****************************************************************************
 *                                                                          *
 * Function: print_usage                                                    *
 *                                                                          *
 * Purpose : Display usage information.                                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void print_usage(void)
{
    printmsg(MSG_USAGE_TITLE);
    printmsg(MSG_USAGE_COPYRIGHT);
    printmsg(MSG_USAGE_SYNTAX, PROGRAM_NAME);
    printmsg(MSG_USAGE_OPTIONS);
    printmsg(MSG_USAGE_OPTION_DEF);
    printmsg(MSG_USAGE_OPTION_EXPLODE);
    printmsg(MSG_USAGE_OPTION_EXTRACT);
    printmsg(MSG_USAGE_OPTION_LIST);
    printmsg(MSG_USAGE_OPTION_MACHINE);
    printmsg(MSG_USAGE_OPTION_OLDIMPLIB);
    printmsg(MSG_USAGE_OPTION_OUT);
    printmsg(MSG_USAGE_OPTION_REMOVE);
    printmsg(MSG_USAGE_OPTION_VERBOSE);

    errorexit(1);
}

/****************************************************************************
 *                                                                          *
 * Function: apperror                                                       *
 *                                                                          *
 * Purpose : Display error message. Terminate program if it's fatal.        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *           02-06-16  Rewritten to support resource messages.              *
 *                                                                          *
 ****************************************************************************/

void apperror(WINERR err, ...)
{
    if (LOWORD(err) != ERROR_SUCCESS)
    {
        char buf[512];
        va_list ap;

        printmsg(MSG_ERRTEXT_PROGNAME, PROGRAM_NAME);

        if (ISFATAL(err)) printmsg(MSG_ERRTEXT_FATAL);
        if (ISERROR(err)) printmsg(MSG_ERRTEXT_ERROR);
        if (ISWARNING(err)) printmsg(MSG_ERRTEXT_WARNING);

        va_start(ap, err);
        if (!FormatMessage(FORMAT_MESSAGE_FROM_HMODULE|FORMAT_MESSAGE_FROM_SYSTEM, NULL,
            err & ~(ERROR_SEVERITY_FATAL|ERROR_SEVERITY_ERROR|ERROR_SEVERITY_WARNING),
            0, buf, NELEMS(buf), &ap))
        {
            sprintf(buf, "*** No message for error 0x%X ***", err);
        }
        va_end(ap);

        CharToOemA(buf, buf);
        printf(buf);

        if (ISFATAL(err)) errorexit(1);
        if (ISERROR(err)) nerrs++;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: printmsg                                                       *
 *                                                                          *
 * Purpose : Display private message.                                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           02-06-16  Created                                              *
 *                                                                          *
 ****************************************************************************/

void printmsg(int msg, ...)
{
    char buf[512];
    va_list ap;

    va_start(ap, msg);
    if (!FormatMessage(FORMAT_MESSAGE_FROM_HMODULE, NULL, msg, 0, buf, NELEMS(buf), &ap))
        *buf = '\0';
    va_end(ap);

    CharToOemA(buf, buf);
    printf(buf);
}

/****************************************************************************
 *                                                                          *
 * Function: errorexit                                                      *
 *                                                                          *
 * Purpose : Bail out.                                                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

void errorexit(int code)
{
    FILEINFO *file;

    if (tmp_file != NULL)
    {
        my_closemap(tmp_file, FALSE);
        my_deletefile(tmp_file->name);
    }

    if (lib_file != NULL)
        my_closemap(lib_file, FALSE);

    for (file = obj_list; file != NULL; file = file->next)
        my_closemap(file, FALSE);

    exit(code);
}

/****************************************************************************
 *                                                                          *
 * Function: ctrl_handler                                                   *
 *                                                                          *
 * Purpose : Ctrl+C and Ctrl+Break handler for console apps.                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *           99-08-14  Suspension of main thread added.                     *
 *                                                                          *
 ****************************************************************************/

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
//Ron Pinkas commented #error ctrl_handler() requires single threading.
#endif

    errorexit(1);
    return 1;
}

