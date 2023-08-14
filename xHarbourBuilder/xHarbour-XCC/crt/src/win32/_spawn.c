/****************************************************************************
 *                                                                          *
 * File    : _spawn.c                                                       *
 *                                                                          *
 * Purpose : __spawn function -- win32 version.                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-04-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <process.h>
#include <stdlib.h>
#include "xio.h"
#include "xcrt.h"

/* spawns a child process (really!) */
int __spawn(int mode, const char *cmd, char *cmdblk, char *envblk)
{
    STARTUPINFO si;
    PROCESS_INFORMATION pi;
    char *cmdline;
    DWORD retval;
    DWORD cpflags = 0;              /* flags for CreateProcess */
    int i;
    ioinfo *pio;
    char *posfile;
    /*UNALIGNED*/ long *posfhnd;
    int nh;                         /* number of file handles to be passed to the child */

    /* check input mode */
    switch (mode)
    {
        case _P_WAIT:       /* synchronous execution */
        case _P_OVERLAY:
        case _P_NOWAITO:    /* asynchronous execution */
        case _P_NOWAIT:     /* asynch + remember result */
        case _P_DETACH:     /* detached in null screen group */
            break;
        default:
            /* invalid mode */
            errno = EINVAL;
            return -1;
    }

    /*
     * Loop over null separate arguments, and replace null separators
     * with spaces to turn it back into a single null terminated
     * command line.
     */
    cmdline = cmdblk;
    while (*cmdblk)
    {
        while (*cmdblk)
            cmdblk++;

        /*
         * If not last argument, turn null separator into a space.
         */
        if (cmdblk[1] != '\0')
            *cmdblk++ = ' ';
    }

    memset(&si, 0, sizeof(si));
    si.cb = sizeof(si);

    for (nh = __iolim; nh && !_osfile(nh-1); nh--)
        ;

    si.cbReserved2 = (WORD)(sizeof(int) + (nh * (sizeof(char) + sizeof(long))));
    si.lpReserved2 = calloc(si.cbReserved2, 1);
    *((/*UNALIGNED*/ int *)(si.lpReserved2)) = nh;

    for (i = 0,
         posfile = (char *)(si.lpReserved2 + sizeof(int)),
         posfhnd = (/*UNALIGNED*/ long *)(si.lpReserved2 + sizeof(int) + (nh * sizeof(char)));
         i < nh;
         i++, posfile++, posfhnd++)
    {
        pio = _pioinfo(i);
        if ((pio->osfile & FNOINHERIT) == 0)
        {
            *posfile = pio->osfile;
            *posfhnd = (long)pio->osfhnd;
        }
        else
        {
            *posfile = 0;
            *posfhnd = (long)INVALID_HANDLE_VALUE;
        }
    }

    /*
     * if the child process is detached, it cannot access the console, so
     * we must nuke the information passed for the first three handles.
     */
    if (mode == _P_DETACH)
    {
        for (i = 0,
             posfile = (char *)(si.lpReserved2 + sizeof(int)),
             posfhnd = (/*UNALIGNED*/ long *)(si.lpReserved2 + sizeof(int) + (nh * sizeof(char)));
             i < (nh < 3 ? nh : 3);
             i++, posfile++, posfhnd++)
        {
            *posfile = 0;
            *posfhnd = (long)INVALID_HANDLE_VALUE;
        }

        cpflags |= DETACHED_PROCESS;
    }

    if (!CreateProcess(cmd, cmdline, 0, 0, TRUE, cpflags, envblk, 0, &si, &pi))
    {
        unsigned long oserr = GetLastError();
        free(si.lpReserved2);
        __maposerr(oserr);
        return -1;
    }
    else
    {
        /*
         * Set errno to 0 to distinguish a child process
         * which returns -1L from an error in the spawning
         * (which will set errno to something non-zero
         */
        errno = 0;
        free(si.lpReserved2);
    }

    if (mode == _P_OVERLAY)
    {
        /* destroy ourselves */
        _Exit(0);
    }
    else if (mode == _P_WAIT)
    {
        WaitForSingleObject(pi.hProcess, (DWORD)(-1L));

        /* return termination code and full exit code */
        GetExitCodeProcess(pi.hProcess, &retval);
        CloseHandle(pi.hProcess);
    }
    else if (mode == _P_DETACH)
    {
        /* like totally detached asynchronous spawn, dude,
           close process handle, return 0 for success */
        CloseHandle(pi.hProcess);
        retval = 0;
    }
    else
    {
        /* asynchronous spawn -- return PID */
        retval = (DWORD)pi.hProcess;
    }

    CloseHandle(pi.hThread);
    return retval;
}

