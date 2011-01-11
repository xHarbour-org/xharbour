/****************************************************************************
 *                                                                          *
 * File    : _crt0.c                                                        *
 *                                                                          *
 * Purpose : C runtime startup.                                             *
 *                                                                          *
 *           This is the actual startup routine for apps. It calls the      *
 *           user's main routine main() or WinMain() after performing       *
 *           C Run-Time Library initialization.                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           03-08-01  Added startup and exit procedures.                   *
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

#define MAXIMUM_HEAP_SIZE  0x8000000  /* 128 MB */


#ifdef __WINMAIN__
void WinMainCRTStartup(void)
#else
void mainCRTStartup(void)
#endif /* __WINMAIN__ */
{
    int mainret;

    /* initialize heap */
    __crtheap = VirtualAlloc(0, MAXIMUM_HEAP_SIZE, MEM_RESERVE, PAGE_NOACCESS);
    if (!__crtheap) _Exit(1);

#ifdef __MT__
        /* initialize multi-threading */
        if(!__mtinit())
        {
            VirtualFree(__crtheap, 0, MEM_RELEASE);
            _Exit(1);
        }
#endif /* __MT__ */

    /*
     * Guard the remainder of the initialization code and the call to main().
     */
    __try
    {
#ifdef __WINMAIN__
        char *cmdline;
        STARTUPINFO si;
#else
        extern int __cdecl main(int, char **);
#endif /* __WINMAIN__ */

        /* initialize clock */
        __clockinit();

        /* initialize low I/O */
        __ioinit();

        /* initialize command line and environment */
        __setargv();
        __setenvp();

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

#ifdef __WINMAIN__
        si.dwFlags = 0;
        GetStartupInfo(&si);

        cmdline = __wincmdln();
        mainret = WinMain(GetModuleHandleA(0), 0, cmdline,
            (si.dwFlags & STARTF_USESHOWWINDOW) ? si.wShowWindow : SW_SHOWDEFAULT);
#else
        mainret = main(__argc, __argv);
#endif /* __WINMAIN__ */

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

        exit(mainret);
    }
    __except(__xcptfilter(GetExceptionCode(), GetExceptionInformation()))
    {
        _Exit(GetExceptionCode());
    }
}
