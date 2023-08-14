/****************************************************************************
 *                                                                          *
 * File    : main.c                                                         *
 *                                                                          *
 * Purpose : ISO C Compiler Driver; Main module.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           02-06-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "cc.h"

#define REGSTR_PATH_FOLDERS  "Software\\Pelle Orinius\\PellesC\\Directories"
#define REGSTR_VAL_PATH  "PathDirs"
#define REGSTR_VAL_LIB  "LibraryDirs"
#define REGSTR_VAL_INCLUDE  "IncludeDirs"

#define MAX_ARGS  128
#define MAX_FILES  256

struct options options = {0};

/* Locals */
static char *cc_argv[MAX_ARGS + MAX_FILES + 1];
static int cc_argc = 0;

static char *ld_argv[MAX_ARGS + MAX_FILES + 1];
static int ld_argc = 0;

static char *srcfiles[MAX_FILES];
static int nfiles = 0;

/* Static function prototypes */
static void cc_init(const char *);
static void cc_args(int, char **);
static void cc_usage(void);
static int cc_spawn(int, const char *, char **);
static BOOL WINAPI ctrl_handler(DWORD);

/****************************************************************************
 *                                                                          *
 * Function: main                                                           *
 *                                                                          *
 * Purpose : Main entry point, of course!                                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           02-06-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

int __cdecl main(int argc, char **argv)
{
    int ret;
    int i;

    /* initialize program */
    cc_init(argv[0]);

    /* process command line args */
    cc_args(argc, argv);

    /* send all files through the compiler */
    for (i = 0; i < nfiles; i++)
    {
        cc_argv[cc_argc] = srcfiles[i];
        cc_argv[cc_argc+1] = NULL;

        printf("%s\n", srcfiles[i]);

        /* spawn the compiler */
        ret = cc_spawn(_P_WAIT, cc_argv[0], cc_argv);
        if (ret < 0)
        {
            printmsg(MSG_COMPILER_PROBLEM);
            ret = 1;
        }

        if (ret)
            return ret;
    }

    if (options.compile_only)
        return 0;

    /* send all files to the linker */
    for (i = 0; i < nfiles; i++)
    {
        char fname[MAX_PATH];
        char *p;

        strcpy(fname, srcfiles[i]);
        if ((p = strrchr(fname, '.')) == 0)
            p = fname + strlen(fname);
        strcpy(p, ".obj");

        ld_argv[ld_argc++] = tstrcpy(fname);
    }
    ld_argv[ld_argc] = NULL;

    /* spawn the linker */
    ret = cc_spawn(_P_WAIT, ld_argv[0], ld_argv);
    if (ret < 0)
    {
        printmsg(MSG_LINKER_PROBLEM);
        ret = 1;
    }

    if (ret)
        return ret;

    return 0;
}

/****************************************************************************
 *                                                                          *
 * Function: cc_init                                                        *
 *                                                                          *
 * Purpose : Program initialization.                                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           02-06-27  Created                                              *
 *           03-07-01  Use startup path for linker too.                     *
 *                                                                          *
 ****************************************************************************/

static void cc_init(const char *arg0)
{
    char fname[MAX_PATH];
    char *p;
    size_t plen;
    HKEY key;

    SetConsoleCtrlHandler(ctrl_handler, TRUE);

    /* combine startup drive or path with compiler filename */
    plen = (p = strrchr(arg0, '\\')) != NULL || (p = strrchr(arg0, ':')) != NULL ? (p - arg0 + 1) : 0;
    strcpy(strncpy(fname, arg0, plen) + plen, "xcc.exe");
    cc_argv[cc_argc++] = tstrcpy(fname);

    /* combine startup drive or path with linker filename */
    strcpy(strncpy(fname, arg0, plen) + plen, "xlink.exe");
    ld_argv[ld_argc++] = tstrcpy(fname);

    /* try using INCLUDE and LIB directories from the IDE */
    if (RegOpenKeyEx(HKEY_CURRENT_USER, REGSTR_PATH_FOLDERS, 0, KEY_READ, &key) == ERROR_SUCCESS)
    {
        unsigned long type;
        unsigned long size;
        char buf[4096];

        size = sizeof(buf);
        if (RegQueryValueExA(key, REGSTR_VAL_INCLUDE, NULL, &type, (unsigned char *)buf, &size) == ERROR_SUCCESS)
        {
            if (type == REG_SZ && size > 0)
            {
                buf[size] = '\0';
                SetEnvironmentVariable("INCLUDE", buf);
            }
        }

        size = sizeof(buf);
        if (RegQueryValueExA(key, REGSTR_VAL_LIB, NULL, &type, (unsigned char *)buf, &size) == ERROR_SUCCESS)
        {
            if (type == REG_SZ && size > 0)
            {
                buf[size] = '\0';
                SetEnvironmentVariable("LIB", buf);
            }
        }

        RegCloseKey(key);
    }
}

/****************************************************************************
 *                                                                          *
 * Function: cc_args                                                        *
 *                                                                          *
 * Purpose : Decode command line options.                                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           02-06-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void cc_args(int argc, char **argv)
{
    while (--argc > 0 && *(++argv))
    {
        if (**argv == '-' || **argv == '/')
        {
            switch ((*argv)[1])
            {
                case '?':
                    cc_usage();
                    break;

                case 'c':
                    options.compile_only = TRUE;
                    break;

                default:
                    if (nfiles == 0)
                    {
                        if (cc_argc == MAX_ARGS)
                        {
                            printmsg(MSG_TOO_MANY_COMPILER_OPTIONS);
                            exit(1);
                        }

                        /* yet another compiler option */
                        cc_argv[cc_argc++] = tstrcpy(*argv);
                    }
                    else
                    {
                        if (ld_argc == MAX_ARGS)
                        {
                            printmsg(MSG_TOO_MANY_LINKER_OPTIONS);
                            exit(1);
                        }

                        /* yet another linker option */
                        ld_argv[ld_argc++] = tstrcpy(*argv);
                    }
                    break;
            }
        }
        else
        {
            char *p = strrchr(*argv, '.');

            if (p != 0 && (_stricmp(p, ".c") == 0 || _stricmp(p, ".s") == 0 || _stricmp(p, ".asm") == 0))
            {
                struct _finddata_t find;
                long handle;

                handle = _findfirst(*argv, &find);
                if (handle != -1)
                {
                    char fname[MAX_PATH];
                    size_t plen;

                    do
                    {
                        /* combine drive or path with the current filename */
                        plen = (p = strrchr(*argv, '\\')) != NULL || (p = strrchr(*argv, ':')) != NULL ? (p - (*argv)+1) : 0;
                        strcpy(strncpy(fname, *argv, plen) + plen, find.name);

                        if (nfiles == MAX_FILES)
                        {
                            printmsg(MSG_TOO_MANY_SOURCE_FILES);
                            exit(1);
                        }

                        /* yet another source file */
                        srcfiles[nfiles++] = tstrcpy(fname);

                    } while (_findnext(handle, &find) == 0);
                    _findclose(handle);
                }
                else
                {
                    printmsg(MSG_FILE_NOT_FOUND, *argv);
                    exit(1);
                }
            }
            else
            {
                if (ld_argc == MAX_ARGS)
                {
                    printmsg(MSG_TOO_MANY_LINKER_OPTIONS);
                    exit(1);
                }

                /* yet another linker option */
                ld_argv[ld_argc++] = tstrcpy(*argv);
            }
        }
    }

    if (nfiles == 0)
        cc_usage();
}

/****************************************************************************
 *                                                                          *
 * Function: cc_usage                                                       *
 *                                                                          *
 * Purpose : Display usage information.                                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           02-06-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void cc_usage(void)
{
    printmsg(MSG_USAGE_TITLE);
    printmsg(MSG_USAGE_COPYRIGHT);
    printmsg(MSG_USAGE_SYNTAX, PROGRAM_NAME);
    printmsg(MSG_USAGE_OPTIONS);
    printmsg(MSG_USAGE_OPTION_B);

    exit(1);
}

/****************************************************************************
 *                                                                          *
 * Function: cc_spawn                                                       *
 *                                                                          *
 * Purpose : Help _spawn() handle filnames with embedded spaces.            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           02-06-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int cc_spawn(int mode, const char *cmd, char **argv)
{
    int i;

    /* quote all arguments containing spaces */
    for (i = 0; argv[i] != NULL; i++)
    {
        char *s;

        for (s = argv[i]; *s && *s != ' '; s++)
            ;

        if (*s)
        {
            char *p = malloc(strlen(argv[i])+1+2);
            if (p != NULL)
            {
                s = argv[i]; argv[i] = p;

                *p++ = '\"';
                while (*s)
                    *p++ = *s++;
                *p++ = '\"';
                *p = '\0';
            }
        }
    }

    return _spawnvp(mode, cmd, argv);
}

/****************************************************************************
 *                                                                          *
 * Function: printmsg                                                       *
 *                                                                          *
 * Purpose : Display private message.                                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           02-06-27  Created                                              *
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
 * Function: ctrl_handler                                                   *
 *                                                                          *
 * Purpose : Ctrl+C and Ctrl+Break handler for console apps.                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           02-06-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static BOOL WINAPI ctrl_handler(DWORD dwCtrlType)
{
    UNREFERENCED_PARAMETER(dwCtrlType);

    printf("\n");
    exit(1);

    return 1;  /* shut up compiler */
}

