/****************************************************************************
 *                                                                          *
 * File    : _mtlock.c                                                      *
 *                                                                          *
 * Purpose : multi-thread locking functions -- win32 version.               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-26  Added _LOCALE_LOCK and _STREAM_LOCK types.           *
 *                                                                          *
 ****************************************************************************/

#ifdef __MT__
#include "xthread.h"
#include "xalloc.h"
#include "xcrt.h"

/*
 * Statically allocated critical section structures for
 * _HEAP_LOCK, _SIGNAL_LOCK, _EXIT_LOCK and _LOCKTAB_LOCK.
 */
static CRITICAL_SECTION csheap;
static CRITICAL_SECTION cssignal;
static CRITICAL_SECTION csexit;
static CRITICAL_SECTION cslocktab;

/* pointer to the critical section structure for each lock */
static CRITICAL_SECTION *locktab[_NLOCKS] = {
    &csheap,        /* 0 == _HEAP_LOCK */
    &cssignal,      /* 1 == _SIGNAL_LOCK */
    &csexit,        /* 2 == _EXIT_LOCK */
    &cslocktab,     /* 3 == _LOCKTAB_LOCK */
    0,              /* 4 == _OSFHND_LOCK */
    0,              /* 5 == _LOCALE_LOCK */
    0,              /* 6 == _STREAM_LOCK */
};


/* initialize the multi-thread lock scheme */
void __mtinitlocks(void)
{
    /*
     * Perform whatever initialization is required for the multi-thread
     * locking (synchronization) scheme. This routine should be called
     * exactly once, during startup, and this must be before any requests
     * are made to assert locks.
     *
     * NOTES: In Win32, the multi-thread locks are created individually,
     * each upon its first use. That is when any particular lock is asserted
     * for the first time, the underlying critical section is then allocated,
     * initialized and (finally) entered. This allocation and initialization
     * is protected under _LOCKTAB_LOCK.
     */

    /*
     * Initialize the special locks.
     */
    InitializeCriticalSection(locktab[_HEAP_LOCK]);
    InitializeCriticalSection(locktab[_SIGNAL_LOCK]);
    InitializeCriticalSection(locktab[_EXIT_LOCK]);
    InitializeCriticalSection(locktab[_LOCKTAB_LOCK]);
}


/* delete all initialized locks */
void __mttermlocks(void)
{
    /*
     * Walks locktab[] and _lockmap, and deletes every 'lock' (i.e. critical section)
     * which has been initialized.
     */
    int locknum;

    for (locknum = 0; locknum < _NLOCKS; locknum++)
    {
        /*
         * If the 'lock' has been created, delete it.
         */
        if (locktab[locknum] != 0)
        {
            if (locknum != _HEAP_LOCK && locknum != _SIGNAL_LOCK && locknum != _EXIT_LOCK && locknum != _LOCKTAB_LOCK)
            {
                /*
                 * Free the memory for the critical section after deleting it.
                 * It's okay to call free() if the heap lock is kept valid
                 * until after all calls to the heap.
                 */
                DeleteCriticalSection(locktab[locknum]);
                free(locktab[locknum]);
            }
        }
    }

    /*
     * Finally, clean up the special locks.
     */
    DeleteCriticalSection(locktab[_HEAP_LOCK]);
    DeleteCriticalSection(locktab[_SIGNAL_LOCK]);
    DeleteCriticalSection(locktab[_EXIT_LOCK]);
    DeleteCriticalSection(locktab[_LOCKTAB_LOCK]);
}


/* acquire a multi-thread lock */
void __mtlock(int locknum)
{
    /* Note that it is legal for a thread to aquire _EXIT_LOCK multiple times */
    CRITICAL_SECTION *csp;

    /*
     * Create/open the lock, if necessary.
     */
    if (locktab[locknum] == 0)
    {
        if ((csp = malloc(sizeof(CRITICAL_SECTION))) == 0)
            _Exit(1);

        __mtlock(_LOCKTAB_LOCK);  /*** recursive call ***/

        if (locktab[locknum] == 0)
        {
            InitializeCriticalSection(csp);
            locktab[locknum] = csp;
        }
        else
        {
            free(csp);
        }

        __mtunlock(_LOCKTAB_LOCK);
    }

    /* enter the critical section */
    EnterCriticalSection(locktab[locknum]);
}


/* release a multi-thread lock */
void __mtunlock(int locknum)
{
    /* leave the critical section */
    LeaveCriticalSection(locktab[locknum]);
}

#endif /* __MT__ */

