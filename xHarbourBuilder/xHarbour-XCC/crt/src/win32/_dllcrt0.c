/****************************************************************************
 *                                                                          *
 * File    : _dllcrt0.c                                                     *
 *                                                                          *
 * Purpose : C runtime initialization routine - DLL version.                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           03-08-01  Added startup and exit procedures.                   *
 *           03-10-07  Added call to library wrapup functions.              *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>
#include "xio.h"
#ifdef __MT__
#include "xthread.h"
#endif /* __MT__ */
#include "xcrt.h"

#ifdef __INITEXIT__
extern INITEXIT __xi_a[];
extern INITEXIT __xi_z[];
extern INITEXIT __xt_a[];
extern INITEXIT __xt_z[];
#endif  /* __INITEXIT__ */

extern void *__crtheap;

extern void (*_Atfuns[])(void);
extern size_t _Atcount0;

#define MAXIMUM_HEAP_SIZE  0x4000000  /* 64 MB */

/* flag set if _CRTDLL_INIT was called with DLL_PROCESS_ATTACH */
static int __proc_attached = 0;

/* User routine DllMain is called on all notifications */
extern BOOL WINAPI DllMain(HANDLE, DWORD, LPVOID);


BOOL WINAPI _CRT_INIT(HANDLE hDllHandle, DWORD dwReason, LPVOID lpreserved)
{
    if (dwReason == DLL_PROCESS_ATTACH)
    {
        /* initialize heap */
        __crtheap = VirtualAlloc(0, MAXIMUM_HEAP_SIZE, MEM_RESERVE, PAGE_NOACCESS);
        if (!__crtheap)
            return FALSE;

#ifdef __MT__
        /* initialize multi-threading */
        if(!__mtinit())
        {
            VirtualFree(__crtheap, 0, MEM_RELEASE);
            __crtheap = 0;
            return FALSE;
        }
#endif /* __MT__ */

        /* initialize clock */
        __clockinit();

        /* initialize low I/O */
        __ioinit();

        /* initialize command line and environment */
        __setargv();
        __setenvp();

        /*
         * increment flag to indicate process attach notification has been received.
         */
        __proc_attached++;

#ifdef __INITEXIT__
        {
            INITEXIT *initfn;

            for (initfn = (INITEXIT *)(__xi_a);
                 initfn < (INITEXIT *)(__xi_z);
                 initfn++)
            {
                /* Call C initializers */
                (*initfn)();
            }
        }
#endif /* __INITEXIT__ */
    }
    else if (dwReason == DLL_PROCESS_DETACH)
    {
        if (__proc_attached > 0)
        {
#ifdef __INITEXIT__
        {
            INITEXIT *exitfn;

            for (exitfn = (INITEXIT *)(__xt_a);
                 exitfn < (INITEXIT *)(__xt_z);
                 exitfn++)
            {
                /* Call C terminators */
                (*exitfn)();
            }
        }
#endif /* __INITEXIT__ */

            __proc_attached--;

            /* call library wrapup functions (see exit.c) */
            while (_Atcount0 > 0)
                (*_Atfuns[--_Atcount0])();

            /* shut down low I/O */
            __ioterm();

#ifdef __MT__
            /* shut down multi-threading */
            __mtterm();
#endif /* __MT__ */

            /* this should be the last thing the C run-time does */
            if (__crtheap)
            {
                VirtualFree(__crtheap, 0, MEM_RELEASE);
                __crtheap = 0;
            }
        }
        else
        {
            /* no prior process attach, just return */
            return FALSE;
        }
    }
#ifdef __MT__
    else if (dwReason == DLL_THREAD_DETACH)
    {
        /* free per-thread data */
        __free_mtd(0);
    }
#endif /* __MT__ */

    return TRUE;
}


BOOL WINAPI _DllMainCRTStartup(HANDLE hDllHandle, DWORD dwReason, LPVOID lpreserved)
{
    BOOL retcode = TRUE;

    /*
     * If this is a process detach notification, check that there has
     * been a prior process attach notification.
     */
    if (dwReason == DLL_PROCESS_DETACH && __proc_attached == 0)
    {
        /*
         * no prior process attach notification. just return without doing anything.
         */
        return FALSE;
    }

    if (dwReason == DLL_PROCESS_ATTACH || dwReason == DLL_THREAD_ATTACH)
    {
        if (retcode)
            retcode = _CRT_INIT(hDllHandle, dwReason, lpreserved);

        if (!retcode)
            return FALSE;
    }

    retcode = DllMain(hDllHandle, dwReason, lpreserved);

    if (dwReason == DLL_PROCESS_ATTACH && !retcode)
    {
        /*
         * The user's DllMain routine returned failure, the C runtime
         * needs to be cleaned up. Do this by calling _CRT_INIT again,
         * this time imitating DLL_PROCESS_DETACH. Note this will also
         * clear the __proc_attached flag so the cleanup will not be
         * repeated upon receiving the real process detach notification.
         */
        _CRT_INIT(hDllHandle, DLL_PROCESS_DETACH, lpreserved);
    }

    if (dwReason == DLL_PROCESS_DETACH || dwReason == DLL_THREAD_DETACH)
    {
        if (_CRT_INIT(hDllHandle, dwReason, lpreserved) == FALSE)
            retcode = FALSE;
    }

    return retcode;
}

