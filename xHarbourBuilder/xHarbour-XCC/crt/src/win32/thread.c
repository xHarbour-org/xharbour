/****************************************************************************
 *                                                                          *
 * File    : thread.c                                                       *
 *                                                                          *
 * Purpose : _beginthread and _endthread functions -- win32 version.        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#ifdef __MT__
#include <stdlib.h>
#include "xthread.h"
#include "xcrt.h"

/* new thread begins here */
static unsigned long __stdcall _threadstart(void *mtd)
{
    /*
     * Stash the pointer to the per-thread data stucture in TLS.
     */
    if (!TlsSetValue(__tlsindex, mtd))
        _Exit(1);

    /*
     * Guard call to user code with a try - except statement to
     * implement runtime errors and signal support.
     */
    __try
    {
        ((void(__cdecl *)(void *))(((tiddata *)mtd)->initaddr))(((tiddata *)mtd)->initarg);

        _endthread();
    }
    __except(__xcptfilter(GetExceptionCode(), GetExceptionInformation()))
    {
        _Exit(GetExceptionCode());
    }

    /*
     * Never executed!
     */
    return 0;
}


/* create a new thread */
unsigned long __cdecl _beginthread(void (__cdecl *initialcode)(void *), unsigned int stacksize, void *argument)
{
    tiddata *mtd;                   /* pointer to per-thread data */
    unsigned long thdl;             /* thread handle */
    unsigned long errcode = 0;      /* Return from GetLastError() */

    /*
     * Allocate and initialize a per-thread data structure for
     * the soon-to-be-created thread.
     */
    if ((mtd = malloc(sizeof(*mtd))) == 0)
        goto error_return;

    /* Initialize the per-thread data */
    __init_mtd(mtd);

    mtd->initaddr = (void *)initialcode;
    mtd->initarg = argument;

    /*
     * Create the new thread. Bring it up in a suspended state so that
     * the thandle and tid fields are filled in before execution starts.
     */
    if ((mtd->thandle = thdl = (unsigned long)CreateThread(0, stacksize, _threadstart,
        (LPVOID)mtd, CREATE_SUSPENDED, (DWORD *)&mtd->tid)) == 0L)
    {
        errcode = GetLastError();
        goto error_return;
    }

    /*
     * Start the new thread executing.
     */
    if (ResumeThread((HANDLE)thdl) == (DWORD)(-1L))
    {
        errcode = GetLastError();
        goto error_return;
    }

    /*
     * Good return.
     */
    return thdl;

    /*
     * Error return.
     */
error_return:
    /*
     * Either mtd is NULL, or it points to the no-longer-necessary block
     * malloc'ed for the tiddata struct which should now be freed up.
     */
    free(mtd);

    /*
     * Map the error, if necessary.
     */
    if (errcode != 0)
        __maposerr(errcode);

    return (unsigned long)-1L;
}


/* terminate the calling thread */
void __cdecl _endthread(void)
{
    tiddata *mtd;

    if ((mtd = __get_mtd()) == 0)
        _Exit(1);

    /*
     * Close the thread handle (if there was one).
     */
    if (mtd->thandle != (unsigned long)(-1L))
        (void)CloseHandle((HANDLE)mtd->thandle);

    /*
     * Free up the tiddata structure & its subordinate buffers
     * __free_mtd() will also clear the value for this thread
     * of the TLS variable __tlsindex.
     */
    __free_mtd(mtd);

    /*
     * Terminate the thread.
     */
    ExitThread(0);
}

#endif /* __MT__ */

