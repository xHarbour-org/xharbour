/****************************************************************************
 *                                                                          *
 * File    : _ioinit.c                                                      *
 *                                                                          *
 * Purpose : lowio initialization functions -- win32 version.               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           03-10-07  Must duplicate handles for STDOUT and STDERR.        *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>
#include "xio.h"
#include "xalloc.h"

/*
 * Number of ioinfo structs allocated at any given time. This number ranges
 * from a minimum of IOINFO_ARRAY_ELEMS to a maximum of IOINFO_MAX_HANDLES
 * (== IOINFO_ARRAY_ELEMS * IOINFO_ARRAYS) in steps of IOINFO_ARRAY_ELEMS.
 */
int __iolim;

/* array of pointers to arrays of ioinfo structs */
ioinfo *__iotab[IOINFO_ARRAYS];

/* macro used to map 0, 1 and 2 to right value for call to GetStdHandle */
#define stdhndl(fh)  ((fh == 0) ? STD_INPUT_HANDLE : (fh == 1) ? STD_OUTPUT_HANDLE : STD_ERROR_HANDLE)

/* allocate and initialize ioinfo structs */
void __ioinit(void)
{
    /*
     * Allocates and initializes initial array(s) of ioinfo structs.
     * Then, obtains and processes information on inherited file
     * handles from the parent process (e.g., cmd.exe).
     *
     * Obtains the StartupInfo structure from the OS. The inherited
     * file handle information is pointed to by the lpReserved2 field.
     * The format of the information is as follows:
     *
     * bytes 0 -- 3       - integer value, say N, which is the number
     *                      of handles information is passed about
     *
     * bytes 4 -- N+3     - the N values for osfile
     *
     * bytes N+4 -- 5*N+3 - N double-words, the N OS HANDLE values
     *                      being passed
     *
     * Next, osfhnd and osfile for the first three ioinfo structs,
     * corrsponding to handles 0, 1 and 2, are initialized as follows:
     *
     * If the value in osfhnd is INVALID_HANDLE_VALUE, then try to
     * obtain a HANDLE by calling GetStdHandle, and call GetFileType
     * to help set osfile. Otherwise, assume _osfhndl and _osfile
     * are valid, but force it to text mode (standard input/output/
     * error are to always start out in text mode).
     *
     * Notes:
     * 1. In general, not all of the passed info from the parent
     *    process will describe open handles! If, for example, only
     *    C handle 1 (STDOUT) and C handle 6 are open in the parent,
     *    info for C handles 0 thru 6 is passed to the the child.
     *
     * 2. Care is taken not to 'overflow' the arrays of ioinfo structs.
     *
     * 3. See exec\dospawn.c for the encoding of the file handle info
     *    to be passed to a child process.
     */
    STARTUPINFO si;
    int cfi_len;
    int fh;
    int i;
    ioinfo *pio;
    char *posfile;
    /* UNALIGNED */ HANDLE *posfhnd;
    HANDLE stdfh;
    DWORD htype;

    /*
     * Allocate and initialize the first array of ioinfo structs.
     * This array is pointed to by __iotab[0].
     */
    if ((pio = malloc(IOINFO_ARRAY_ELEMS * sizeof(ioinfo))) == 0)
        _Exit(1);

    __iotab[0] = pio;
    __iolim = IOINFO_ARRAY_ELEMS;

    for (; pio < __iotab[0] + IOINFO_ARRAY_ELEMS; pio++)
    {
        pio->osfile = 0;
        pio->osfhnd = INVALID_HANDLE_VALUE;
        pio->pipech = 10;  /* linefeed/newline char */
    }

    /*
     * Process inherited file handle information, if any.
     */
    GetStartupInfo(&si);
    if (si.cbReserved2 != 0 && si.lpReserved2 != 0)  /* way to go, Bill! */
    {
        /* Get the number of handles inherited */
        cfi_len = *(/*UNALIGNED*/ int *)(si.lpReserved2);

        /*
         * Set pointers to the start of the passed file info and OS HANDLE values.
         */
        posfile = (char *)(si.lpReserved2) + sizeof(int);
        posfhnd = (/*UNALIGNED*/ HANDLE *)(posfile + cfi_len);

        /* Ensure cfi_len does not exceed the number of supported handles! */
        if (cfi_len > IOINFO_MAX_HANDLES) cfi_len = IOINFO_MAX_HANDLES;

        /*
         * Allocate sufficient arrays of ioinfo structs to hold inherited file information.
         */
        for (i = 1; __iolim < cfi_len; i++)
        {
            /* Allocate another array of ioinfo structs */
            if ((pio = malloc(IOINFO_ARRAY_ELEMS * sizeof(ioinfo))) == 0)
            {
                /*
                 * No room for another array of ioinfo structs, reduce
                 * the number of inherited handles we process.
                 */
                cfi_len = __iolim;
                break;
            }

            /*
             * Update __iotab[] and __iolim.
             */
            __iotab[i] = pio;
            __iolim += IOINFO_ARRAY_ELEMS;

            for (; pio < __iotab[i] + IOINFO_ARRAY_ELEMS; pio++)
            {
                pio->osfile = 0;
                pio->osfhnd = INVALID_HANDLE_VALUE;
                pio->pipech = 10;  /* linefeed/newline char */
            }
        }

        /* Validate and copy the passed file information */
        for (fh = 0; fh < cfi_len; fh++, posfile++, posfhnd++)
        {
            /*
             * Copy the passed file info if it appears to describe
             * an open, valid file or device.
             *
             * Note that GetFileType cannot be called for pipe handles
             * since it may 'hang' if there is blocked read pending on
             * the pipe in the parent.
             */
            if (*posfhnd != INVALID_HANDLE_VALUE && (*posfile & FOPEN) &&
                ((*posfile & FPIPE) || GetFileType(*posfhnd) != FILE_TYPE_UNKNOWN))
            {
                pio = _pioinfo(fh);
                pio->osfhnd = *posfhnd;
                pio->osfile = *posfile;
            }
        }
    }

    /*
     * If valid HANDLE's for standard input, output and error were not
     * inherited, try to obtain them directly from the OS. Also, set the
     * appropriate bits in the osfile fields.
     */
    for (fh = 0; fh < 3; fh++)
    {
        pio = __iotab[0] + fh;

        if (pio->osfhnd == INVALID_HANDLE_VALUE)
        {
            /* Mark the handle as open in text mode */
            pio->osfile = (char)(FOPEN|FTEXT);

            if (((stdfh = GetStdHandle(stdhndl(fh))) != INVALID_HANDLE_VALUE) &&
                ((htype =  GetFileType(stdfh)) != FILE_TYPE_UNKNOWN))
            {
                HANDLE new_stdfh;

                /* We must duplicate the file handle, so it still works during DLL shutdown! */
                if ((DuplicateHandle(GetCurrentProcess(), stdfh,
                    GetCurrentProcess(), &new_stdfh, 0L, TRUE, DUPLICATE_SAME_ACCESS)))
                    stdfh = new_stdfh;  /* OK - use duplicate handle, otherwise ignore the error */

                /* Obtained a valid HANDLE from GetStdHandle */
                pio->osfhnd = stdfh;

                /*
                 * Finish setting osfile: determine if it is a character
                 * device or pipe.
                 */
                if ((htype & 0xFF) == FILE_TYPE_CHAR)
                    pio->osfile |= FDEV;
                else if ((htype & 0xFF) == FILE_TYPE_PIPE)
                    pio->osfile |= FPIPE;
            }
            else
            {
                /*
                 * If there is no valid HANDLE, treat the CRT handle as
                 * being open in text mode on a device (with
                 * INVALID_HANDLE_VALUE underlying it).
                 */
                pio->osfile |= FDEV;
            }
        }
        else
        {
            /*
             * Handle was passed to us by parent process. make
             * sure it is text mode.
             */
            pio->osfile |= FTEXT;
        }
    }

    /* Set the number of supported HANDLE's to __iolim */
    (void)SetHandleCount((unsigned)__iolim);
}


/* free the memory holding the ioinfo arrays */
void __ioterm(void)
{
    int i;

    for (i = 0; i < IOINFO_ARRAYS; i++)
    {
        if (__iotab[i] != 0)
        {
            /*
             * Free the memory which held the ioinfo array.
             */
            free(__iotab[i]);
            __iotab[i] = 0;
        }
    }
}

