/****************************************************************************
 *                                                                          *
 * File    : exit.c                                                         *
 *                                                                          *
 * Purpose : exit function.                                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-05  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "xthread.h"
#include "xcrt.h"

/* macros */
#define NATS  80    /* 64 + extra for fclose, xgetloc, locks, etc. */

/* static data */
void (__cdecl *_Atfuns[NATS])(void) = {0};
size_t _Atcount = {NATS};
size_t _Atcount0 = {0};

void __cdecl _Exit(int);

#ifdef __MT__
/* tidy up and exit to system */
void __cdecl (exit)(int status)
{
    __mtlock(_EXIT_LOCK);
    __try
    {
        while (_Atcount < NATS)
            (*_Atfuns[_Atcount++])();
        while (_Atcount0 > 0)  /* library wrapup stack */
            (*_Atfuns[--_Atcount0])();
    }
    __finally
    {
        __mtunlock(_EXIT_LOCK);
    }

    _Exit(status);
}
#else
/* tidy up and exit to system */
void __cdecl (exit)(int status)
{
    while (_Atcount < NATS)
        (*_Atfuns[_Atcount++])();
    while (_Atcount0 > 0)  /* library wrapup stack */
        (*_Atfuns[--_Atcount0])();

    _Exit(status);
}
#endif /* __MT__ */

