/****************************************************************************
 *                                                                          *
 * File    : threadex.c                                                     *
 *                                                                          *
 * Purpose : _beginthreadex and _endthreadex functions -- win32 version.    *
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
static unsigned long __stdcall _threadstartex(void *mtd)
{
    /*
     * Stash the pointer to the per-thread data stucture in TLS.
     */
    if (!TlsSetValue(__tlsindex, mtd))
        _Exit(1);

    /*
     * Set the thread ID field -- parent thread cannot set it after
     * CreateThread() returns since the child thread might have run
     * to completion and already freed its per-thread data block!
     */
    ((tiddata *)mtd)->tid = GetCurrentThreadId();

    /*
     * Guard call to user code with a try - except statement to
     * implement runtime errors and signal support.
     */
    __try
    {
        _endthreadex(((unsigned int (__stdcall *)(void *))(((tiddata *)mtd)->initaddr))(((tiddata *)mtd)->initarg));
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
unsigned long __cdecl _beginthreadex(void *security, unsigned int stacksize,
    unsigned int (__stdcall *initialcode)(void *), void *argument, unsigned int createflag, unsigned int *thrdaddr)
{
    tiddata *mtd;                   /* pointer to per-thread data */
    unsigned long thdl;             /* thread handle */
    unsigned long errcode = 0L;     /* Return from GetLastError() */

    /*
     * Allocate and initialize a per-thread data structure for the
     * soon-to-be-created thread.
     */
    if ((mtd = malloc(sizeof(*mtd))) == 0)
        goto error_return;

    /* Initialize the per-thread data */
    __init_mtd(mtd);

    mtd->initaddr = (void *)initialcode;
    mtd->initarg = argument;
    mtd->thandle = (unsigned long)-1L;

    /*
     * Create the new thread using the parameters supplied by the caller.
     */
    if ((thdl = (unsigned long)CreateThread(security, stacksize, _threadstartex,
        (void *)mtd, createflag, (unsigned long *)thrdaddr)) == 0L)
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
     *
     * Note: this routine returns 0 for failure, just like the Win32
     * API CreateThread, but _beginthread() returns -1 for failure.
     */
    if (errcode != 0L)
        __maposerr(errcode);

    return (unsigned long)0;
}


/* terminate the calling thread */
void __cdecl _endthreadex(unsigned int retcode)
{
    tiddata *mtd;

    if ((mtd = __get_mtd()) == 0)
        _Exit(1);

    /*
     * Free up the _tiddata structure & its subordinate buffers
     * __free_mtd() will also clear the value for this thread
     * of the TLS variable __tlsindex.
     */
    __free_mtd(mtd);

    /*
     * Terminate the thread.
     */
    ExitThread(retcode);
}

#endif /* __MT__ */

