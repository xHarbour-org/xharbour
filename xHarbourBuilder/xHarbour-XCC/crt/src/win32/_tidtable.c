/****************************************************************************
 *                                                                          *
 * File    : _tidtable.c                                                    *
 *                                                                          *
 * Purpose : thread support functions -- win32 version.                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-03-28  Added initialization of signal table.                *
 *           01-09-12  Free'ing of mtd->randarr added.                      *
 *                                                                          *
 ****************************************************************************/

#ifdef __MT__
#include "xthread.h"
#include "xalloc.h"
#include "xcrt.h"

#define INVALID_TLSINDEX  0xffffffff

unsigned long __tlsindex = INVALID_TLSINDEX;

/* init multi-thread data bases */
int __mtinit(void)
{
    tiddata *mtd;

    /* Initialize the lock data base */
    __mtinitlocks();

    /* Allocate a TLS index to maintain pointers to per-thread data */
    if ((__tlsindex = TlsAlloc()) == INVALID_TLSINDEX)
        return 0;

    /* Create a per-thread data structure for this (i.e., the startup) thread */
    if ((mtd = malloc(sizeof(*mtd))) == 0 || !TlsSetValue(__tlsindex, (void *)mtd))
        return 0;

    /* Initialize the per-thread data */
    __init_mtd(mtd);

    mtd->tid = GetCurrentThreadId();
    mtd->thandle = (unsigned long)(-1L);

    return 1;
}


/* clean-up multi-thread data bases */
void __mtterm(void)
{
    /* Clean up the lock data base */
    __mttermlocks();

    /*
     * Free up the TLS index.
     * (Set the variable __tlsindex back to the unused state (-1L).)
     */
    if (__tlsindex != INVALID_TLSINDEX)
    {
        TlsFree(__tlsindex);
        __tlsindex = INVALID_TLSINDEX;
    }
}


/* initialize a per-thread data structure */
void __init_mtd(tiddata *mtd)
{
    memset(mtd, 0, sizeof(*mtd));
    mtd->sigtab = (void *)__sigtab;
    mtd->randseed = 1;
}


/* get per-thread data structure for the current thread */
tiddata *__get_mtd(void)
{
    tiddata *mtd;
    unsigned long TL_LastError;

    TL_LastError = GetLastError();
    if ((mtd = TlsGetValue(__tlsindex)) == 0)
    {
        /* No per-thread data structure for this thread. Try to create one */
        if ((mtd = malloc(sizeof(*mtd))) != 0 && TlsSetValue(__tlsindex, (void *)mtd))
        {
            /* Initialize of per-thread data */
            __init_mtd(mtd);

            mtd->tid = GetCurrentThreadId();
            mtd->thandle = (unsigned long)-1L;
        }
        else
        {
            _Exit(1);
        }
    }
    SetLastError(TL_LastError);
    return mtd;
}


/* free up a per-thread data structure */
void __free_mtd(tiddata *mtd)
{
    /*
     * Do nothing unless per-thread data has been allocated for this module!
     */
    if (__tlsindex != INVALID_TLSINDEX)
    {
        /*
         * if parameter "mtd" is NULL, get the per-thread data pointer
         * Must NOT call __get_mtd because it will allocate one if none exists!
         */
        if (!mtd) mtd = TlsGetValue(__tlsindex);

        /*
         * Free up the _tiddata structure & its malloc'ed buffers.
         */
        if (mtd)
        {
            if (mtd->randarr) free((void *)mtd->randarr);
            if (mtd->errbuf) free((void *)mtd->errbuf);
            if (mtd->tmpnambuf) free((void *)mtd->tmpnambuf);
            if (mtd->asctimebuf) free((void *)mtd->asctimebuf);
            if (mtd->timebuf) free((void *)mtd->timebuf);
            if (mtd->sigtab != __sigtab) free((void *)mtd->sigtab);
            free((void *)mtd);
        }

        /* Clear the pointer to the per-thread data block */
        TlsSetValue(__tlsindex, (void *)0);
    }
}

#endif /* __MT__ */

