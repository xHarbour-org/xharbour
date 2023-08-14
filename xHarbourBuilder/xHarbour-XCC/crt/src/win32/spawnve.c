/****************************************************************************
 *                                                                          *
 * File    : spawnve.c                                                      *
 *                                                                          *
 * Purpose : _spawnve function -- win32 version.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-04-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <string.h>
#include <process.h>
#include <errno.h>
#include "xalloc.h"
#include "xcrt.h"

/* extension array - ordered in search order from right to left */
static char *ext_strings[] = { ".cmd", ".bat", ".exe", ".com" };
enum { CMD, BAT, EXE, COM, EXTFIRST=CMD, EXTLAST=COM };

/* build arguments and execute process */
static int comexecmd(int mode, const char *cmd, char * const *argv, char * const *envp)
{
    char *argblk;
    char *envblk;
    int rc;

    if (__cenvarg(argv, envp, &argblk, &envblk) == -1)
        return -1;

    rc = __spawn(mode, cmd, argblk, envblk);

    /* free memory */
    free(argblk);
    free(envblk);

    return rc;
}

/* spawns a child process (actually does some work!) */
int __cdecl (_spawnve)(int mode, const char *cmd, char * const *argv, char * const *envp)
{
    char *pathname = (char *)cmd;
    char *ext;   /* where the extension goes if we have to add one */
    char *p;
    char *q;
    int rc;
    int i;

    p = strrchr(pathname, '\\');
    q = strrchr(pathname, '/');

    /* ensure that pathname is an absolute or relative pathname. also,
     * position p to point at the filename portion of pathname (i.e., just
     * after the last occurence of a colon, slash or backslash character
     */
    if (q == 0)
    {
        if (p == 0)
        {
            if ((p = strchr(pathname, ':')) == 0)
            {
                /* pathname is a filename only, force it to be
                 * a relative pathname. note that an extra byte
                 * is malloc-ed just in case pathname is NULL,
                 * to keep the heap from being trashed by strcpy.
                 */
                if ((pathname = malloc((strlen(pathname) + 3) * sizeof(char))) == 0)
                    return -1;

                strcpy(pathname, ".\\");
                strcat(pathname, cmd);

                /* set p to point to the start of the filename
                 * (i.e., past the ".\\" prefix)
                 */
                p = pathname + 2;
            }
            /* else pathname has drive specifier prefix and p is
             * is pointing to the ':' */
        }
    }
    else if (p == 0 || q > p)
        p = q;


    rc = -1;  /* init to error value */

    if ((ext = strrchr(p, '.')) != NULL)
    {
        /* extension given; only do filename */
        if (_access(pathname, 0) != -1)
        {
            rc = comexecmd(mode, pathname, argv, envp);
        }
    }
    else
    {
        /* no extension; try .cmd/.bat, then .com and .exe */
        if ((p = malloc((strlen(pathname) + 5) * sizeof(char))) == 0)
            return -1;

        strcpy(p, pathname);
        ext = p + strlen(pathname);

        for (i = EXTLAST; i >= EXTFIRST; --i)
        {
            strcpy(ext, ext_strings[i]);
            if (_access(p, 0) != -1)
            {
                rc = comexecmd(mode, p, argv, envp);
                break;
            }
        }

        free(p);
    }

    if (pathname != cmd)
        free(pathname);

    return rc;
}
