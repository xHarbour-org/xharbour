/****************************************************************************
 *                                                                          *
 * File    : _setargv.c                                                     *
 *                                                                          *
 * Purpose : __setargv function -- win32 version.                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include "xalloc.h"
#include "xcrt.h"

int __argc;
char **__argv;

/* Parses the command line and sets up the argv[] array */
static void parse_cmdline(char *cmdstart, char **argv, char *args, int *numargs, int *numchars)
{
    char *p;
    unsigned char c;
    int inquote;            /* 1 = inside quotes */
    int copychar;           /* 1 = copy char to *args */
    unsigned numslash;      /* num of backslashes seen */

    *numchars = 0;
    *numargs = 1;           /* the program name at least */

    /* first scan the program name, copy it, and count the bytes */
    p = cmdstart;
    if (argv) *argv++ = args;

#ifdef WILDCARD
    /*
     * To handle later wild card expansion, we prefix each entry by
     * it's first character before quote handling.  This is done
     * so _[w]cwild() knows whether to expand an entry or not.
     */
    if (args) *args++ = *p;
    ++*numchars;
#endif /* WILDCARD */

    /*
     * A quoted program name is handled here. The handling is much
     * simpler than for other arguments. Basically, whatever lies
     * between the leading double-quote and next one, or a terminal null
     * character is simply accepted. Fancier handling is not required
     * because the program name must be a legal NTFS/HPFS file name.
     * Note that the double-quote characters are not copied, nor do they
     * contribute to numchars.
     */
    if (*p == '\"')
    {
        /*
         * scan from just past the first double-quote through the next
         * double-quote, or up to a null, whichever comes first.
         */
        while (*(++p) != '\"' && *p != '\0')
        {
            ++*numchars;
            if (args) *args++ = *p;
        }
        /* append the terminating null */
        ++*numchars;
        if (args) *args++ = '\0';

        /* if we stopped on a double-quote (usual case), skip over it */
        if (*p == '\"')
            p++;
    }
    else
    {
        /* Not a quoted program name */
        do
        {
            ++*numchars;
            if (args) *args++ = *p;

            c = (unsigned char)*p++;
        } while (c != ' ' && c != '\0' && c != '\t');

        if (c == '\0')
        {
            p--;
        }
        else
        {
            if (args) *(args-1) = '\0';
        }
    }

    inquote = 0;

    /* loop on each argument */
    for (;;)
    {
        if (*p)
        {
            while (*p == ' ' || *p == '\t')
                ++p;
        }

        if (*p == '\0')
            break;  /* end of args */

        /* scan an argument */
        if (argv) *argv++ = args;   /* store ptr to arg */
        ++*numargs;

#ifdef WILDCARD
        /*
         * To handle later wild card expansion, we prefix each entry by
         * it's first character before quote handling.  This is done
         * so _[w]cwild() knows whether to expand an entry or not.
         */
        if (args) *args++ = *p;
        ++*numchars;
#endif /* WILDCARD */

        /* loop through scanning one argument */
        for (;;)
        {
            copychar = 1;

            /*
             * Rules: 2N backslashes + " ==> N backslashes and begin/end quote
             * 2N+1 backslashes + " ==> N backslashes + literal "
             * N backslashes ==> N backslashes
             */
            numslash = 0;
            while (*p == '\\')
            {
                /* count number of backslashes for use below */
                ++p;
                ++numslash;
            }

            if (*p == '\"')
            {
                /* if 2N backslashes before, start/end quote, otherwise copy literally */
                if (numslash % 2 == 0)
                {
                    if (inquote)
                    {
                        if (p[1] == '\"')
                            p++;    /* double quote inside quoted string */
                        else        /* skip first quote char and copy second */
                            copychar = 0;
                    }
                    else
                    {
                        copychar = 0;  /* don't copy quote */
                    }

                    inquote = !inquote;
                }
                numslash /= 2;
            }

            /* copy slashes */
            while (numslash--)
            {
                if (args) *args++ = '\\';
                ++*numchars;
            }

            /* if at end of arg, break loop */
            if (*p == '\0' || (!inquote && (*p == ' ' || *p == '\t')))
                break;

            /* copy character into argument */
            if (copychar)
            {
                if (args) *args++ = *p;
                ++*numchars;
            }
            ++p;
        }

        /* null-terminate the argument */
        if (args) *args++ = '\0';  /* terminate string */
        ++*numchars;
    }

    /* We put one last argument in -- a null ptr */
    if (argv) *argv++ = 0;
    ++*numargs;
}


/* Read command line and create the argv array for C programs */
#ifdef WILDCARD
void __Setargv(void)
#else
void __setargv(void)
#endif /* WILDCARD */
{
    char pgmname[MAX_PATH];
    char *p;
    char *cmdstart;
    int numargs, numchars;

    /*
     * if there's no command line at all (won't happen from cmd.exe, but
     * possibly another program), then we use _pgmptr as the command line
     * to parse, so that argv[0] is initialized to the program name.
     */
    cmdstart = (char *)GetCommandLineA();
    if (*cmdstart == '\0')
    {
        /* get the program name pointer from Windows */
        GetModuleFileName(0, pgmname, sizeof(pgmname) / sizeof(char));
        cmdstart = pgmname;
    }

    /* find out how much space is needed to store args */
    parse_cmdline(cmdstart, 0, 0, &numargs, &numchars);

    /* allocate space for argv[] vector and strings */
    p = malloc(numargs * sizeof(char *) + numchars * sizeof(char));
    if (!p) _Exit(1);

    /* store args and argv ptrs in just allocated block */
    parse_cmdline(cmdstart, (char **)p, p + numargs * sizeof(char *), &numargs, &numchars);

    /* set argv and argc */
    __argc = numargs - 1;
    __argv = (char **)p;

#ifdef WILDCARD
    /* call _[w]cwild to expand wildcards in arg vector */
    if (_cwild()) _Exit(1);  /* out of space */
#endif /* WILDCARD */
}

