/****************************************************************************
 *                                                                          *
 * File    : system.c                                                       *
 *                                                                          *
 * Purpose : system function -- win32 version.                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-04-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <stdlib.h>
#include <process.h>
#include <errno.h>
#include <io.h>

int __cdecl (system)(const char *s)
{
    char *argv[4];

    argv[0] = getenv("COMSPEC");

    /* if the command is NULL, only check if shell is present */
    if (s == 0)
        return (argv[0] != 0) ? !_access(argv[0], 0) : 0;

    argv[1] = "/c";
    argv[2] = (char *)s;
    argv[3] = 0;

    /* if there is a COMSPEC defined, try spawning the shell */
    if (argv[0] != 0)
    {
        int retval = _spawnve(_P_WAIT, argv[0], argv, 0);
        if (retval != -1 || (errno != ENOENT && errno != EACCES))
            return retval;
    }

    /* No COMSPEC so set argv[0] to what COMSPEC should be */
    argv[0] = (GetVersion() & 0x8000) ? "command.com" : "cmd.exe";

    /* let the _spawnvpe routine do the path search and spawn */
    return _spawnvpe(_P_WAIT, argv[0], argv, 0);
}

