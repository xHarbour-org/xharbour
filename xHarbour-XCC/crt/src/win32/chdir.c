/****************************************************************************
 *                                                                          *
 * File    : chdir.c                                                        *
 *                                                                          *
 * Purpose : _chdir function -- win32 version.                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-01-25  Created                                              *
 *                                                                          *
 ****************************************************************************/

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <stdio.h>
#include "xcrt.h"

/* change directory */
int __cdecl (_chdir)(const char *path)
{
    char abspath[FILENAME_MAX+1];
    char evar[4];

    if (SetCurrentDirectoryA(path))
    {
        /*
         * If the new current directory path is NOT a UNC path, we must
         * update the OS environment variable specifying the current
         * directory for what is now current drive. To do this, get the
         * full current directory, build the environment variable string
         * and call SetEnvironmentVariable(). We need to do this because
         * SetCurrentDirectory does not (i.e., does not update the
         * current-directory-on-drive environment variables) and other
         * functions (fullpath, spawn, etc) need them to be set.
         *
         * If associated with a 'drive', the current directory should
         * have the form of the example below:
         *
         *  D:\nt\private\mytests
         *
         * so that the environment variable should be of the form:
         *
         *  =D:=D:\nt\private\mytests
         *
         */
        if (GetCurrentDirectoryA(sizeof(abspath), abspath) != 0)
        {
            /*
             * check if it is a UNC name, just return if is.
             */
            if ((abspath[0] == '\\' || abspath[0] == '/') && abspath[0] == abspath[1])
                return 0;

            evar[0] = '=';
            evar[1] = toupper(abspath[0]);
            evar[2] = ':';
            evar[3] = '\0';

            if (SetEnvironmentVariable(evar, abspath))
                return 0;
        }
    }

    __maposerr(GetLastError());
    return -1;
}

