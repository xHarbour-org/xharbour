/****************************************************************************
 *                                                                          *
 * File    : rc.c                                                           *
 *                                                                          *
 * Purpose : Win32 Resource Compiler; DLL compiler driver.                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-11-09  Created                                              *
 *                                                                          *
 ****************************************************************************/

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "resdll.h"
#include "msg.h"

#define PROGRAM_NAME  "xRC"

#define EXT_RC   ".RC"
#define EXT_RES  ".RES"

/* Error definitions */
#define ERROR_SEVERITY_FATAL  0x8000U

#define RCFATAL(err)    (ERROR_SEVERITY_FATAL|(err))
#define RCERROR(err)    (ERROR_SEVERITY_ERROR|(err))
#define RCWARNING(err)  (ERROR_SEVERITY_WARNING|(err))

#define ISFATAL(err)    (((err) & ERROR_SEVERITY_FATAL) ? 1 : 0)
#define ISERROR(err)    (((err) & 0xC0000000) == ERROR_SEVERITY_ERROR)
#define ISWARNING(err)  (((err) & 0xC0000000) == ERROR_SEVERITY_WARNING)

/* Private WINERR error codes */
#define ERROR_UNKNOWN_OPTION    (APPLICATION_ERROR_MASK|MSG_UNKNOWN_OPTION)
#define ERROR_INVALID_CODEPAGE  (APPLICATION_ERROR_MASK|MSG_INVALID_CODEPAGE)
#define ERROR_INVALID_LANGUAGE  (APPLICATION_ERROR_MASK|MSG_INVALID_LANGUAGE)

#define NELEMS(arr)  (sizeof(arr) / sizeof(arr[0]))

/* Matches GetLastError() type */
typedef DWORD WINERR;

/* Name of input (.RC) and output (.RES) files */
static char input[MAX_PATH] = "";
static char output[MAX_PATH] = "";
static HMODULE hmod = NULL;

/* Static function prototypes */
static void rc_init(void);
static void rc_args(int, char **);
static void add_extension_to_file(char *, const char *);
static void print_usage(void);
static void apperror(WINERR, ...);
static void printmsg(int, ...);
static void error_function(enum errclass, const char *, int, const char *);
static void info_function(const char *);
static void printmsg(int, ...);
static BOOL WINAPI ctrl_handler(DWORD);

/****************************************************************************
 *                                                                          *
 * Function: main                                                           *
 *                                                                          *
 * Purpose : Main entry point, of course!                                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-11-09  Created                                              *
 *                                                                          *
 ****************************************************************************/

int __cdecl main(int argc, char **argv)
{
    rc_init();

    /* Process command line args */
    rc_args(argc, argv);

    /* Process resource file */
    if (!compile_script(input, output))
        return 1;

    return 0;
}

/****************************************************************************
 *                                                                          *
 * Function: rc_init                                                        *
 *                                                                          *
 * Purpose : Program initialization.                                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-11-09  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void rc_init(void)
{
    /* Make sure we catch ctrl+break */
    SetConsoleCtrlHandler(ctrl_handler, TRUE);

    /* Since both resource script and argv uses ANSI... */
    SetFileApisToANSI();

    /* Set error callback function (non-IDE mode) */
    set_error_function(error_function, 0);

    /* Get handle to module with messages */
    hmod = GetModuleHandle("xrc.dll");
}

/****************************************************************************
 *                                                                          *
 * Function: rc_args                                                        *
 *                                                                          *
 * Purpose : Decode command line options.                                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-11-09  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void rc_args(int argc, char **argv)
{
    int codepage;
    int language;
    char *s;

    while (--argc > 0 && *(++argv))
    {
        if (**argv == '-' || **argv == '/')
        {
            s = ++*argv;

            while (*s)
            {
                switch (*s++)
                {
                    case '?':
                        print_usage();
                        break;

                    case 'r': case 'R':
                        /* Compatibility; nothing to do */
                        continue;

                    case 'v': case 'V':
                        set_information_function(info_function);
                        continue;

                    case 'x': case 'X':
                        delete_standard_include_paths();
                        continue;

                    case 'c': case 'C':
                        if (*s == '\0')
                        {
                            if (--argc == 0) print_usage();
                            s = *++argv;
                        }

                        codepage = atoi(s);

                        if (!set_codepage(codepage))
                            apperror(RCFATAL(ERROR_INVALID_CODEPAGE), codepage);
                        break;

                    case 'l': case 'L':
                        if (*s == '\0')
                        {
                            if (--argc == 0) print_usage();
                            s = *++argv;
                        }

                        language = atoi(s);

                        if (!set_language(language))
                            apperror(RCFATAL(ERROR_INVALID_LANGUAGE), language);
                        break;

                    case 'i': case 'I':
                        if (*s == '\0')
                        {
                            if (--argc == 0) print_usage();
                            s = *++argv;
                        }

                        add_include_path(s);
                        break;

                    case 'd': case 'D':
                        if (*s == '\0')
                        {
                            if (--argc == 0) print_usage();
                            s = *++argv;
                        }

                        define_symbol(s);
                        break;

                    case 'f': case 'F':
                        if (*s == 'o' || *s == 'O')
                        {
                            if (*++s == '\0')
                            {
                                if (--argc == 0) print_usage();
                                s = *++argv;
                            }

                            strcpy(output, s);
                            break;
                        }

                        /* fall through */

                    default:
                        apperror(RCFATAL(ERROR_UNKNOWN_OPTION), --s);
                        break;
                }

                /* Always exit while */
                break;
            }
        }
        else
        {
            /* One filename; no more, no less */
            if (*input) print_usage();

            strcpy(input, *argv);
            add_extension_to_file(input, EXT_RC);
        }
    }

    /* One filename; no more, no less */
    if (!*input) print_usage();

    if (!*output)
    {
        strcpy(output, input);
        *strrchr(output, '.') = '\0';
    }

    add_extension_to_file(output, EXT_RES);
}

/****************************************************************************
 *                                                                          *
 * Function: add_extension_to_file                                          *
 *                                                                          *
 * Purpose : Add extension to filename without it.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-11-09  Created                                              *
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
 * Function: print_usage                                                    *
 *                                                                          *
 * Purpose : Display usage information.                                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-11-09  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void print_usage(void)
{
    printmsg(MSG_USAGE_TITLE);
    printmsg(MSG_USAGE_COPYRIGHT);
    printmsg(MSG_USAGE_SYNTAX, PROGRAM_NAME);
    printmsg(MSG_USAGE_OPTIONS);
    printmsg(MSG_USAGE_OPTION_C, GetACP());
    printmsg(MSG_USAGE_OPTION_D);
    printmsg(MSG_USAGE_OPTION_FO);
    printmsg(MSG_USAGE_OPTION_I);
    printmsg(MSG_USAGE_OPTION_L);
    printmsg(MSG_USAGE_OPTION_R);
    printmsg(MSG_USAGE_OPTION_V);
    printmsg(MSG_USAGE_OPTION_X);

    /*
     * Also defined by Microsoft RC:
     *
     * /u    Undefine a symbol
     * /w    Warn on Invalid codepage in .rc (default is an error)
     * /y    Don't warn if there are duplicate control ID's
     * /n    Append null's to all strings in the string tables.
     *
     */

    exit(1);
}

/****************************************************************************
 *                                                                          *
 * Function: apperror                                                       *
 *                                                                          *
 * Purpose : Display error message. Terminate program if it's fatal.        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           02-06-17  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void apperror(WINERR err, ...)
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
        if (!FormatMessage(FORMAT_MESSAGE_FROM_HMODULE|FORMAT_MESSAGE_FROM_SYSTEM, hmod,
            err & ~(ERROR_SEVERITY_FATAL|ERROR_SEVERITY_ERROR|ERROR_SEVERITY_WARNING),
            0, buf, NELEMS(buf), &ap))
        {
            sprintf(buf, "*** No message for error 0x%X ***", err);
        }
        va_end(ap);

        CharToOemA(buf, buf);
        printf(buf);

        if (ISFATAL(err)) exit(1);
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

static void printmsg(int msg, ...)
{
    char buf[512];
    va_list ap;

    va_start(ap, msg);
    if (!FormatMessage(FORMAT_MESSAGE_FROM_HMODULE, hmod, msg, 0, buf, NELEMS(buf), &ap))
        *buf = '\0';
    va_end(ap);

    CharToOemA(buf, buf);
    printf(buf);
}

/****************************************************************************
 *                                                                          *
 * Function: error_function                                                 *
 *                                                                          *
 * Purpose : Error reporting function (callback).                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-11-09  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void error_function(enum errclass class, const char *filename, int line, const char *msg)
{
    char buf[512];

    printmsg(MSG_ERRTEXT_PROGNAME, PROGRAM_NAME);

    if (filename != NULL)
        printf("%s(%d): ", filename, line);

    if (class == ERRCLASS_FATAL)
        printmsg(MSG_ERRTEXT_FATAL);
    else if (class == ERRCLASS_ERROR)
        printmsg(MSG_ERRTEXT_ERROR);
    else if (class == ERRCLASS_WARNING)
        printmsg(MSG_ERRTEXT_WARNING);

    CharToOemA(msg, buf);
    printf("%s\n", buf);
}

/****************************************************************************
 *                                                                          *
 * Function: info_function                                                  *
 *                                                                          *
 * Purpose : Verbose information reporting function (callback).             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-11-09  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void info_function(const char *msg)
{
    char buf[512];

    CharToOemA(msg, buf);
    printf("%s\n", buf);
}

/****************************************************************************
 *                                                                          *
 * Function: ctrl_handler                                                   *
 *                                                                          *
 * Purpose : Ctrl+C and Ctrl+Break handler for console apps.                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-11-09  Created                                              *
 *                                                                          *
 ****************************************************************************/

static BOOL WINAPI ctrl_handler(DWORD dwCtrlType)
{
    static int aborted = 0;

    UNREFERENCED_PARAMETER(dwCtrlType);

    if (!aborted)
    {
        printmsg(MSG_CTRL_BREAK, PROGRAM_NAME);
        set_abort_flag();
        aborted = 1;
    }

    return 1;
}

