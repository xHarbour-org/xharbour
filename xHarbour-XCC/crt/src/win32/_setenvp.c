/****************************************************************************
 *                                                                          *
 * File    : _setenvp.c                                                     *
 *                                                                          *
 * Purpose : __setenvp function -- win32 version.                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include "xalloc.h"
#include "xcrt.h"

char *__envp;

/* build the environment area */
void __setenvp(void)
{
    char *env, *p, *s;
    int length, numchars;

    env = (char *)GetEnvironmentStringsA();

    /*
     * NOTE: starting with single null indicates no environment.
     * Count the number of strings. Skip drive letter settings
     * ("=C:=C:\foo" type) by skipping all environment variables
     * that begin with '=' character.
     */
    numchars = 0;
    for (p = env; *p != '\0'; p += length)
    {
        length = strlen(p) + 1;

        if (*p != '=')
            numchars += length;
    }

    /* allocate space for environment and terminating \0 */
    __envp = s = malloc((numchars + 1) * sizeof(char));
    if (!__envp) _Exit(1);

    /* copy strings to malloc'd memory */
    for (p = env; *p != '\0'; p += length)
    {
        length = strlen(p) + 1;

        if (*p != '=')
            strcpy(s, p), s += length;
    }

    /* terminate */
    *s = '\0';
}

