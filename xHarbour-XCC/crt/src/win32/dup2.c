/****************************************************************************
 *                                                                          *
 * File    : dup2.c                                                         *
 *                                                                          *
 * Purpose : lowio _dup2 function -- win32 version.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xalloc.h"
#include "xio.h"
#include "xcrt.h"

static int extend_ioinfo_arrays(int);

/* force handle 2 to refer to handle 1 */
int __cdecl (_dup2)(int fh1, int fh2)
{
    HANDLE new_osfhandle;

    /* validate handles */
    if ((unsigned)fh1 >= (unsigned)__iolim || !(_osfile(fh1) & FOPEN) || (unsigned)fh2 >= IOINFO_MAX_HANDLES)
    {
        errno = EBADF;
        return -1;
    }

    /* make sure there is an ioinfo struct corresponding to fh2 */
    if (fh2 >= __iolim && extend_ioinfo_arrays(fh2) != 0)
    {
        errno = ENOMEM;
        return -1;
    }

    /* take of the case of equal handles */
    if (fh1 == fh2)
    {
        /*
         * Since fh1 is known to be open, return 0 indicating success.
         * This is in conformance with the POSIX specification for dup2.
         */
        return 0;
    }

    /* if fh2 is open, close it */
    if (_osfile(fh2) & FOPEN)
    {
        /*
         * close the handle. ignore the possibility of an error - an
         * error simply means that an OS handle value may remain bound
         * for the duration of the process.
         */
        (void)_close(fh2);
    }

    /* Duplicate source file onto target file */
    if (!DuplicateHandle(GetCurrentProcess(), __get_osfhnd(fh1),
        GetCurrentProcess(), &new_osfhandle, 0L, TRUE, DUPLICATE_SAME_ACCESS))
    {
        /* error occured -- map error code and return */
        __maposerr(GetLastError());
        return -1;
    }
    else
    {
        __set_osfhnd(fh2, new_osfhandle);
    }

    /* copy the _osfile information, with the FNOINHERIT bit cleared */
    _osfile(fh2) = _osfile(fh1) & ~FNOINHERIT;

    return 0;
}

/* extend ioinfo arrays to fh */
static int extend_ioinfo_arrays(int fh)
{
    ioinfo *pio;
    int i;

    /*
     * walk __iotab[], allocating an array of ioinfo structs for each
     * empty entry, until there is an ioinfo struct corresponding to fh.
     */
    for (i = 0; fh >= __iolim; i++)
    {
        if (__iotab[i] == 0)
        {
            if ((pio = malloc(IOINFO_ARRAY_ELEMS * sizeof(ioinfo))) != 0)
            {
                __iotab[i] = pio;
                __iolim += IOINFO_ARRAY_ELEMS;

                for (; pio < __iotab[i] + IOINFO_ARRAY_ELEMS ; pio++)
                {
                    pio->osfile = 0;
                    pio->osfhnd = INVALID_HANDLE_VALUE;
                    pio->pipech = 10;  /* linefeed/newline char */
                }
            }
            else
            {
                /* couldn't allocate another array, return failure */
                return -1;
            }
        }
    }

    return 0;
}

