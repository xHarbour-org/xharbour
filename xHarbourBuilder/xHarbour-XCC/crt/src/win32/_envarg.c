/****************************************************************************
 *                                                                          *
 * File    : _envarg.c                                                      *
 *                                                                          *
 * Purpose : __cenvarg function -- win32 version.                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-04-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include "xcrt.h"

/* set up command line and environment */
int __cenvarg(char * const *argv, char * const *envp, char **argblk, char **envblk)
{
    char * const *vp;
    unsigned int len;
    char *cptr;
    int cwd_start;
    int cwd_end;

    /* calculate size of the command line string */
    for (vp = argv, len = 2; *vp != '\0'; len += strlen(*vp++) + 1)
        ;

    /* allocate space for the command line */
    if ((*argblk = malloc(len * sizeof(char))) == 0)
    {
        *envblk = 0;
        errno = ENOMEM;
        return -1;
    }

    if (envp)
    {
        /* calculate size of the environment strings */
        for (vp = envp, len = 2; *vp != '\0'; len += strlen(*vp++) + 1)
            ;

        /* search for the first one */
        for (cwd_start = 0;
            __envp[cwd_start] != '\0' &&  __envp[cwd_start] != '=';
            cwd_start += strlen(&__envp[cwd_start]) + 1)
            ;

        /* find the total size of all contiguous ones */
        cwd_end = cwd_start;
        while (__envp[cwd_end+0] == '=' &&
               __envp[cwd_end+1] != '\0' &&
               __envp[cwd_end+2] == ':' &&
               __envp[cwd_end+3] == '=')
        {
            cwd_end += 4 + strlen(&__envp[cwd_end+4]) + 1;
        }
        len += cwd_end - cwd_start;

        /* allocate space for the environment strings */
        if ((*envblk = malloc(len * sizeof(char))) == 0)
        {
            free(*argblk);
            *argblk = 0;
            errno = ENOMEM;
            return -1;
        }
    }
    else
    {
        *envblk = 0;
    }

    /*
     * Build the command line by concatenating the argument strings
     * with spaces between, and two null bytes at the end.
     * NOTE: The argv[0] argument is followed by a null.
     */

    cptr = *argblk;
    vp = argv;

    if (!*vp)
    {
        ++cptr;
    }
    else
    {
        strcpy(cptr, *vp);
        cptr += strlen(*vp++) + 1;
    }

    while (*vp)
    {
        strcpy(cptr, *vp);
        cptr += strlen(*vp++);
        *cptr++ = ' ';
    }

    *cptr = cptr[-1] = '\0';  /* remove extra blank, add double null */

    /*
     * Build the environment block by concatenating the environment
     * strings with nulls between and two null bytes at the end.
     */

    cptr = *envblk;

    if (envp != 0)
    {
        /*
         * Copy the "cwd" strings to the new environment.
         */
        memcpy(cptr, &__envp[cwd_start], (cwd_end - cwd_start) * sizeof(char));
        cptr += cwd_end - cwd_start;

        /*
         * Copy the environment strings from "envp".
         */
        vp = envp;
        while (*vp)
        {
            strcpy(cptr, *vp);
            cptr += 1 + strlen(*vp++);
        }
    }

    if (cptr != 0)
    {
        if (cptr == *envblk)
        {
            /* empty environment block; this requires two nulls */
            *cptr++ = '\0';
        }
        /* extra null terminates the segment */
        *cptr = '\0';
    }

    return 0;
}

