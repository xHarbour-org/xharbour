/****************************************************************************
 *                                                                          *
 * File    : spawnvpe.c                                                     *
 *                                                                          *
 * Purpose : _spawnvpe function -- win32 version.                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-04-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>
#include <string.h>
#include <process.h>
#include <errno.h>
#include <stdio.h>
#include "xcrt.h"

#define ISSLASH(c)  ((c) == '\\' || (c) == '/')

/* spawn a child process with env - search along PATH */
int __cdecl (_spawnvpe)(int mode, const char *cmd, char * const *argv, char * const *envp)
{
    int retval;
    char *env;
    char *buf;

    if ((retval = _spawnve(mode, cmd, argv, envp)) != -1 || errno != ENOENT ||
        strchr(cmd, '/') != 0 || (env = getenv("PATH")) == 0 || (buf = malloc(FILENAME_MAX)) == 0)
    {
        return retval;
    }

    /* could not find the file as specified, search PATH. try each
     * component of the PATH until we get either no error return, or the
     * error is not ENOENT and the component is not a UNC name, or we run
     * out of components to try.
     */
    while ((env = __getpath(env, buf, FILENAME_MAX-1)) != 0 && *buf != '\0')
    {
        char *endp = buf + strlen(buf) - 1;

        /* if necessary, append a backslash */
        if (*endp != '\\' && *endp != '/')
            strcat(buf, "\\");

        /* check that the final path will be of legal size. if so,
         * build it. otherwise, return to the caller (return value
         * and errno rename set from initial call to _spawnve()).
         */
        if ((strlen(buf) + strlen(cmd)) < FILENAME_MAX)
            strcat(buf, cmd);
        else
            break;

        /* try spawning it. if successful, or if errno comes back with a
         * value other than ENOENT and the pathname is not a UNC name,
         * return to the caller.
         */
        if ((retval = _spawnve(mode, buf, argv, envp)) != -1 ||
            (errno != ENOENT && (!ISSLASH(buf[0]) || !ISSLASH(buf[1]))))
            break;
    }

    free(buf);
    return retval;
}

