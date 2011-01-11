/****************************************************************************
 *                                                                          *
 * File    : _osfinfo.c                                                     *
 *                                                                          *
 * Purpose : _osfhnd[] support routines -- win32 version.                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xio.h"
#include "xalloc.h"
#include "xthread.h"
#include "xcrt.h"

/* get free _ioinfo struct */
int __new_osfhnd(void)
{
    int fh = -1;  /* file handle */
    int i;
    ioinfo *pio;

    /*
     * Search the arrays of ioinfo structs, in order, looking for the
     * first free entry. The compound index of this free entry is the
     * return value. Here, the compound index of the ioinfo struct
     * *(__iotab[i] + j) is k = i * IOINFO_ARRAY_ELEMS + j, and k = 0,
     * 1, 2,... is the order of the search.
     */
    for (i = 0; i < IOINFO_ARRAYS; i++)
    {
        /*
         * If __iotab[i] is non-empty array, search it looking for
         * the first free entry. Otherwise, allocate a new array and use
         * its first entry.
         */
        if (__iotab[i] != 0)
        {
            /*
             * Search for an available entry.
             */
            for (pio = __iotab[i];
                 pio < __iotab[i] + IOINFO_ARRAY_ELEMS;
                 pio++)
            {
                if ((pio->osfile & FOPEN) == 0)
                {
                    pio->osfhnd = INVALID_HANDLE_VALUE;
                    fh = i * IOINFO_ARRAY_ELEMS + (pio - __iotab[i]);
                    break;
                }
            }

            /*
             * Check if a free entry has been found.
             */
            if (fh != -1)
                break;
        }
        else
        {
            /*
             * Allocate and initialize another array of ioinfo structs.
             */
            if ((pio = malloc(IOINFO_ARRAY_ELEMS * sizeof(ioinfo))) != 0)
            {
                /*
                 * Update __iotab[] and __iolim.
                 */
                __iotab[i] = pio;
                __iolim += IOINFO_ARRAY_ELEMS;

                for (; pio < __iotab[i] + IOINFO_ARRAY_ELEMS; pio++)
                {
                    pio->osfile = 0;
                    pio->osfhnd = INVALID_HANDLE_VALUE;
                    pio->pipech = 10;
                }

                /*
                 * The first element of the newly allocated array of ioinfo
                 * structs, *(__iotab[i]), is our first free entry.
                 */
                fh = i * IOINFO_ARRAY_ELEMS;
            }

            break;
        }
    }

    /*
     * return the index of the previously free table entry, if one was
     * found. return -1 otherwise.
     */
    return fh;
}


/* mark osfhnd field of ioinfo struct as free */
int __free_osfhnd(int fh)
{
    if ((unsigned)fh < (unsigned)__iolim && (_osfile(fh) & FOPEN) && _osfhnd(fh) != INVALID_HANDLE_VALUE)
    {
#ifndef NO_CONSOLE_APP
        switch (fh)
        {
            case 0:
                SetStdHandle(STD_INPUT_HANDLE, 0);
                break;
            case 1:
                SetStdHandle(STD_OUTPUT_HANDLE, 0);
                break;
            case 2:
                SetStdHandle(STD_ERROR_HANDLE, 0);
                break;
        }
#endif

        _osfhnd(fh) = INVALID_HANDLE_VALUE;
        return 0;
    }
    else
    {
        errno = EBADF;  /* bad handle */
        return -1;
    }
}


/* set Win32 HANDLE value */
int __set_osfhnd(int fh, HANDLE value)
{
    if ((unsigned)fh < (unsigned)__iolim && _osfhnd(fh) == INVALID_HANDLE_VALUE)
    {
#ifndef NO_CONSOLE_APP
        switch (fh)
        {
            case 0:
                SetStdHandle(STD_INPUT_HANDLE, value);
                break;
            case 1:
                SetStdHandle(STD_OUTPUT_HANDLE, value);
                break;
            case 2:
                SetStdHandle(STD_ERROR_HANDLE, value);
                break;
        }
#endif

        _osfhnd(fh) = value;
        return 0;
    }
    else
    {
        errno = EBADF;  /* bad handle */
        return -1;
    }
}


/* get Win32 HANDLE value */
HANDLE __get_osfhnd(int fh)
{
    if ((unsigned)fh < (unsigned)__iolim && (_osfile(fh) & FOPEN))
        return _osfhnd(fh);
    else
    {
        errno = EBADF;  /* bad handle */
        return INVALID_HANDLE_VALUE;
    }
}


#ifdef __MT__
/* lock file handle */
void __lock_osfhnd(int fh)
{
    ioinfo *pio = _pioinfo(fh);

    /* Make sure the lock has been initialized */
    if (pio->lockinit == 0)
    {
        __mtlock(_LOCKTAB_LOCK);
        if (pio->lockinit == 0)
        {
            InitializeCriticalSection(&pio->cslock);
            pio->lockinit++;
        }
        __mtunlock(_LOCKTAB_LOCK);
    }

    EnterCriticalSection(&_pioinfo(fh)->cslock);
}


/* unlock file handle */
void __unlock_osfhnd(int fh)
{
    LeaveCriticalSection(&_pioinfo(fh)->cslock);
}
#endif /* __MT__ */

