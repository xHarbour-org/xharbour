/****************************************************************************
 *                                                                          *
 * File    : _xcptfilter.c                                                  *
 *                                                                          *
 * Purpose : C runtime exception filter - win32 version.                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-03-28  Added support for CRT signals.                       *
 *                                                                          *
 ****************************************************************************/

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <signal.h>

typedef struct xcpt_signal {
    unsigned long xcptnum;      /* Windows exception code */
    int sig;                    /* CRT signal number */
} xcpt_signal;

static xcpt_signal xcpttab[] =
{
    { (unsigned long)STATUS_ACCESS_VIOLATION,         SIGSEGV },
    { (unsigned long)STATUS_ILLEGAL_INSTRUCTION,      SIGILL },
    { (unsigned long)STATUS_PRIVILEGED_INSTRUCTION,   SIGILL },
    { (unsigned long)STATUS_FLOAT_DENORMAL_OPERAND,   SIGFPE },
    { (unsigned long)STATUS_FLOAT_DIVIDE_BY_ZERO,     SIGFPE },
    { (unsigned long)STATUS_FLOAT_INEXACT_RESULT,     SIGFPE },
    { (unsigned long)STATUS_FLOAT_INVALID_OPERATION,  SIGFPE },
    { (unsigned long)STATUS_FLOAT_OVERFLOW,           SIGFPE },
    { (unsigned long)STATUS_FLOAT_STACK_CHECK,        SIGFPE },
    { (unsigned long)STATUS_FLOAT_UNDERFLOW,          SIGFPE }
};

#define NEXCEPTIONS  ((sizeof xcpttab)/sizeof(xcpttab[0]))


/* filter and process C runtime exceptions */
int __xcptfilter(unsigned long xcptnum, struct _EXCEPTION_POINTERS *xcptinfo)
{
    xcpt_signal *xcpt;

    /* walk through the table looking for the proper entry */
    for (xcpt = xcpttab; xcpt < xcpttab + NEXCEPTIONS; xcpt++)
    {
        if (xcpt->xcptnum == xcptnum)
        {
            __sigfunc *const s = signal(xcpt->sig, SIG_IGN);

            if (s == SIG_ERR || s == SIG_DFL)
                break;
            else if (s != SIG_IGN)
            {
                /* revert and call handler */
                signal(xcpt->sig, SIG_DFL);
                (*s)(xcpt->sig);
            }

            return EXCEPTION_CONTINUE_EXECUTION;
        }
    }

    return UnhandledExceptionFilter(xcptinfo);
}

