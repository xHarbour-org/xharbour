/****************************************************************************
 *                                                                          *
 * File    : wait.c                                                         *
 *                                                                          *
 * Purpose : _cwait function -- win32 version.                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-04-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <process.h>
#include <errno.h>
#include "xcrt.h"

/* wait for specific child process */
int __cdecl (_cwait)(int *status, int process_id, int action_code)
{
    int retval;
    int retstatus;

    /*
     * Explicitly check for process_id being -1 or -2. In Windows NT,
     * -1 is a handle on the current process, -2 is a handle on the
     * current thread, and it is perfectly legal to to wait (forever)
     * on either
     */
    if (process_id == -1 || process_id == -2)
    {
        errno = ECHILD;
        return -1;
    }

    /* wait for child process, then fetch its exit code */
    if (WaitForSingleObject((HANDLE)process_id, (DWORD)(-1L)) == 0 &&
      GetExitCodeProcess((HANDLE)process_id, (DWORD *)&retstatus))
    {
        retval = process_id;
    }
    else
    {
        /*
         * one of the API calls failed. map the error and set up to
         * return failure. note the invalid handle error is mapped in-
         * line to ECHILD
         */
        if (GetLastError() == ERROR_INVALID_HANDLE)
            errno = ECHILD;
        else
            __maposerr(GetLastError());

        retval = -1;
        retstatus = -1;
    }

    CloseHandle((HANDLE)process_id);

    if (status != 0)
        *status = retstatus;

    return retval;
}

