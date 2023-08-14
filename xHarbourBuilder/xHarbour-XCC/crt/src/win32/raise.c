/****************************************************************************
 *                                                                          *
 * File    : raise.c                                                        *
 *                                                                          *
 * Purpose : raise function -- win32 version.                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <signal.h>
#include <stdio.h>
#include <stdlib.h>

/* raise a signal */
int __cdecl (raise)(int sig)
{
    __sigfunc *const s = signal(sig, SIG_IGN);

    if (s == SIG_ERR)
        return -1;  /* bad signal */
    else if (s == SIG_IGN)
        ;
    else if (s != SIG_DFL)
    {
        /* revert and call handler */
        signal(sig, SIG_DFL);
        (*s)(sig);
    }
    else
    {
        /* default handling */
        char ac[10], *p;

        switch (sig)
        {
            /* print known signals by name */
            case SIGABRT:
                p = "abort";
                break;
            case SIGFPE:
                p = "arithmetic error";
                break;
            case SIGILL:
                p = "invalid executable code";
                break;
            case SIGINT:
                p = "interruption";
                break;
            case SIGSEGV:
                p = "invalid storage access";
                break;
            case SIGTERM:
                p = "termination request";
                break;
            default:
                *(p = &ac[sizeof(ac)-1]) = '\0';
                do *--p = sig % 10 + '0'; while ((sig /= 10) != 0);
                fputs("signal #", stderr);
                break;
        }

        fputs(p, stderr);
        fputs(" -- terminating\n", stderr);
        exit(EXIT_FAILURE);
    }

    return 0;
}

