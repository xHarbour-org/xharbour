/****************************************************************************
 *                                                                          *
 * File    : _wincmdln.c                                                    *
 *                                                                          *
 * Purpose : __wincmdln function -- win32 version.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include "xcrt.h"

/* parses the command line for WinMain */
char *__wincmdln(void)
{
    char *cmdstart;

    /*
     * Skip past program name (first token in command line).
     * Check for and handle quoted program name.
     */
    cmdstart = (char *)GetCommandLineA();
    if (*cmdstart == '\"')
    {
        /*
         * Scan, and skip over, subsequent characters until
         * another double-quote or a null is encountered.
         */
        while (*++cmdstart != '\"' && *cmdstart != '\0')
            ;

        /*
         * If we stopped on a double-quote (usual case), skip over it.
         */
        if (*cmdstart == '\"')
            cmdstart++;
    }
    else
    {
        while (*cmdstart > ' ')
            cmdstart++;
    }

    /*
     * Skip past any white space preceeding the second token.
     */
    while (*cmdstart && *cmdstart <= ' ')
        cmdstart++;

    return cmdstart;
}

