/****************************************************************************
 *                                                                          *
 * File    : signal.c                                                       *
 *                                                                          *
 * Purpose : signal function -- win32 version.                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-03-28  Added support for exceptions and threads.            *
 *                                                                          *
 ****************************************************************************/

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <signal.h>
#include "xthread.h"
#include "xalloc.h"
#include "xcrt.h"

__sigfunc *__sigtab[_NSIG] = {0};

static int handler_installed = 0;

/* console Ctrl+C handler */
static BOOL WINAPI ctrlevent_capture(DWORD CtrlType)
{
    /*
     * Capture Ctrl+C and Ctrl+Break events from the console and dispose
     * of them according to values i signal handler table. CtrlType
     * indicates type of event, either CTRL_C_EVENT, CTRL_BREAK_EVENT,
     * CTRL_CLOSE_EVENT, CTRL_LOGOFF_EVENT or CTRL_SHUTDOWN_EVENT.
     */
    __sigfunc *const s = signal(SIGINT, SIG_IGN);

    if (s == SIG_ERR || s == SIG_DFL)
        return FALSE;
    else if (s == SIG_IGN)
        ;
    else
    {
        /* revert and call handler */
        signal(SIGINT, SIG_DFL);
        (*s)(SIGINT);
    }

    /* event has been handled */
    return TRUE;
}

#ifdef __MT__
/* specify handling for a signal */
__sigfunc * __cdecl (signal)(int sig, __sigfunc *fun)
{
    __sigfunc *s;

    if (sig <= 0 || sig >= _NSIG || fun == SIG_ERR)
        return SIG_ERR;  /* bad signal */

    if (sig == SIGILL || sig == SIGFPE || sig == SIGSEGV)
    {
        tiddata *mtd = __get_mtd();

        /*
         * Check that there a per-thread instance of the exception-action
         * table for this thread. if there isn't, create one.
         */
        if (mtd->sigtab == __sigtab)
        {
            /* allocate space for a signal table */
            if ((mtd->sigtab = malloc(sizeof(__sigtab))) != 0)
            {
                /* initialize the table from the global master table */
                memcpy(mtd->sigtab, __sigtab, sizeof(__sigtab));
            }
            else
            {
                /* memory allocation failure */
                return SIG_ERR;
            }
        }

        s = ((__sigfunc **)mtd->sigtab)[sig], ((__sigfunc **)mtd->sigtab)[sig] = fun;
        return s;
    }
    else if (sig == SIGINT && !handler_installed)
    {
        if (!SetConsoleCtrlHandler(ctrlevent_capture, TRUE))
            return SIG_ERR;  /* bad shit */

        handler_installed = 1;
    }

    s = __sigtab[sig], __sigtab[sig] = fun;
    return s;
}
#else /* __MT__ */
/* specify handling for a signal */
__sigfunc * __cdecl (signal)(int sig, __sigfunc *fun)
{
    __sigfunc *s;

    if (sig <= 0 || sig >= _NSIG || fun == SIG_ERR)
        return SIG_ERR;  /* bad signal */

    if (sig == SIGINT && !handler_installed)
    {
        if (!SetConsoleCtrlHandler(ctrlevent_capture, TRUE))
            return SIG_ERR;  /* bad shit */

        handler_installed = 1;
    }

    s = __sigtab[sig], __sigtab[sig] = fun;
    return s;
}
#endif /* __MT__ */
