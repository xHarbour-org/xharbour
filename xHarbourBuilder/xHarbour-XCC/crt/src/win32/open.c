/****************************************************************************
 *                                                                          *
 * File    : open.c                                                         *
 *                                                                          *
 * Purpose : lowio open function -- win32 version.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stdio.h>
#include <fcntl.h>
#include "xio.h"
#include "xcrt.h"

/* open or create a file */
int __cdecl (_open)(const char *fname, int oflag, ...)
{
    /*
     * Opens the file and prepares for subsequent reading or writing.
     * The flag argument specifies how to open the file:
     * _O_APPEND - reposition file ptr to end before every write.
     * _O_BINARY - open in binary mode.
     * _O_CREAT -  create a new file - no effect if file already exists.
     * _O_EXCL -   return error if file exists, only use with O_CREAT.
     * _O_RDONLY - open for reading only.
     * _O_RDWR -   open for reading and writing.
     * _O_TEXT -   open in text mode.
     * _O_TRUNC -  open and truncate to zero length.
     * _O_WRONLY - open for writing only.
     * _O_NOINHERIT - handle will not be inherited by child processes.
     *
     * Exactly one of _O_RDONLY, _O_WRONLY, _O_RDWR must be given.
     *
     * The pmode argument is only required when _O_CREAT is specified.
     * Its flag settings:
     * _S_IWRITE - writing permitted
     * _S_IREAD -  reading permitted
     * _S_IREAD|_S_IWRITE - both reading and writing permitted.
     *
     * Note, the _creat() function also uses this function but
     * setting up the correct arguments and calling _open().
     * _creat() sets the __creat_flag to 1 prior to calling _open()
     * so _open() can return correctly. _open() returns the file
     * handle in eax in this case.
     */
    va_list ap;
    int pmode;

    va_start(ap, oflag);
    pmode = va_arg(ap, int);
    va_end(ap);

    /* default sharing mode is DENY NONE */
    return _sopen(fname, oflag, _SH_DENYNO, pmode);
}


/* open a file with sharing */
int __cdecl (_sopen)(const char *fname, int oflag, int shflag, ...)
{
    /*
     * Opens the file with possible file sharing.
     * shflag defines the sharing flags:
     * _SH_COMPAT - set compatability mode.
     * _SH_DENYRW - deny read and write access to the file.
     * _SH_DENYWR - deny write access to the file.
     * _SH_DENYRD - deny read access to the file.
     * _SH_DENYNO - permit read and write access.
     * Other flags are the same as _open().
     */
    int fh;                         /* handle of opened file */
    int filepos;                    /* length of file - 1 */
    char ch;                        /* character at end of file */
    char fileflags;                 /* _osfile flags */
    va_list ap;                     /* variable argument (pmode) */
    int pmode;
    HANDLE osfh;                    /* OS handle of opened file */
    DWORD faccess;                  /* OS file access (requested) */
    DWORD fshare;                   /* OS file sharing mode */
    DWORD fcreate;                  /* OS method of opening/creating */
    DWORD fattrib;                  /* OS file attribute flags */
    DWORD isdev;                    /* device indicator in low byte */
    SECURITY_ATTRIBUTES sa;

    sa.nLength = sizeof(sa);
    sa.lpSecurityDescriptor = 0;

    if (oflag & _O_NOINHERIT)
    {
        sa.bInheritHandle = FALSE;
        fileflags = FNOINHERIT;
    }
    else
    {
        sa.bInheritHandle = TRUE;
        fileflags = 0;
    }

    /* figure out binary/text mode */
    if ((oflag & _O_BINARY) == 0)
    {
        if (oflag & _O_TEXT)
            fileflags |= FTEXT;
    }

    /*
     * decode access flags
     */
    switch(oflag & (_O_RDONLY|_O_WRONLY|_O_RDWR))
    {
        case _O_RDONLY:  /* read access */
            faccess = GENERIC_READ;
            break;
        case _O_WRONLY:  /* write access */
            faccess = GENERIC_WRITE;
            break;
        case _O_RDWR:  /* read and write access */
            faccess = GENERIC_READ|GENERIC_WRITE;
            break;
        default:  /* error, bad oflag */
            errno = EINVAL;
            return -1;
    }

    /*
     * decode sharing flags
     */
    switch (shflag)
    {
        case _SH_DENYRW:  /* exclusive access */
            fshare = 0L;
            break;
        case _SH_DENYWR:  /* share read access */
            fshare = FILE_SHARE_READ;
            break;
        case _SH_DENYRD:  /* share write access */
            fshare = FILE_SHARE_WRITE;
            break;
        case _SH_DENYNO:  /* share read and write access */
            fshare = FILE_SHARE_READ|FILE_SHARE_WRITE;
            break;
        default:  /* error, bad shflag */
            errno = EINVAL;
            return -1;
    }

    /*
     * decode open/create method flags
     */
    switch (oflag & (_O_CREAT|_O_EXCL|_O_TRUNC))
    {
        case 0:
        case _O_EXCL:  /* ignore EXCL w/o CREAT */
            fcreate = OPEN_EXISTING;
            break;
        case _O_CREAT:
            fcreate = OPEN_ALWAYS;
            break;
        case _O_CREAT|_O_EXCL:
        case _O_CREAT|_O_TRUNC|_O_EXCL:
            fcreate = CREATE_NEW;
            break;
        case _O_TRUNC:
        case _O_TRUNC|_O_EXCL:  /* ignore EXCL w/o CREAT */
            fcreate = TRUNCATE_EXISTING;
            break;
        case _O_CREAT|_O_TRUNC:
            fcreate = CREATE_ALWAYS;
            break;
        default:
            /* this can't happen ... all cases are covered */
            errno = EINVAL;
            return -1;
    }

    /*
     * decode file attribute flags if _O_CREAT was specified
     */
    fattrib = FILE_ATTRIBUTE_NORMAL;  /* default */

    if (oflag & _O_CREAT)
    {
        /*
         * set up variable argument list stuff
         */
        va_start(ap, shflag);
        pmode = va_arg(ap, int);
        va_end(ap);

        if ((pmode & _S_IWRITE) == 0)
            fattrib = FILE_ATTRIBUTE_READONLY;
    }

    /*
     * Set temporary file (delete-on-close) attribute if requested.
     */
    if (oflag & _O_TEMPORARY)
    {
        fattrib |= FILE_FLAG_DELETE_ON_CLOSE;
        faccess |= DELETE;
    }

    /*
     * Set temporary file (delay-flush-to-disk) attribute if requested.
     */
    if (oflag & _O_SHORT_LIVED)
        fattrib |= FILE_ATTRIBUTE_TEMPORARY;

    /*
     * Set sequential or random access attribute if requested.
     */
    if (oflag & _O_SEQUENTIAL)
        fattrib |= FILE_FLAG_SEQUENTIAL_SCAN;
    else if (oflag & _O_RANDOM)
        fattrib |= FILE_FLAG_RANDOM_ACCESS;

    /*
     * get an available handle.
     */
    if ((fh = __new_osfhnd()) == -1)
    {
        errno = EMFILE;  /* too many open files */
        return -1;
    }

    /*
     * try to open/create the file.
     */
    if ((osfh = CreateFile((LPTSTR)fname, faccess, fshare, &sa, fcreate, fattrib, 0)) == INVALID_HANDLE_VALUE)
    {
        /*
         * OS call to open/create file failed! map the error, release
         * the lock, and return -1. note that it's not necessary to
         * call __free_osfhnd (it hasn't been used yet).
         */
        __maposerr(GetLastError());
        return -1;
    }

    /* find out what type of file (file/device/pipe) */
    if ((isdev = GetFileType(osfh)) == FILE_TYPE_UNKNOWN)
    {
        CloseHandle(osfh);
        __maposerr(GetLastError());
        return -1;
    }

    /* is isdev value to set flags */
    if (isdev == FILE_TYPE_CHAR)
        fileflags |= FDEV;
    else if (isdev == FILE_TYPE_PIPE)
        fileflags |= FPIPE;

    /* the file is open. remember the o.s. handle */
    __set_osfhnd(fh, osfh);

    /*
     * mark the handle as open. store flags gathered so far in _osfile array.
     */
    fileflags |= FOPEN;
    _osfile(fh) = fileflags;

    if (!(fileflags & (FDEV|FPIPE)) && (fileflags & FTEXT) && (oflag & _O_RDWR))
    {
        /*
         * We have a text mode file. If it ends in CTRL-Z, we wish to
         * remove the CTRL-Z character, so that appending will work.
         * We do this by seeking to the end of file, reading the last
         * byte, and shortening the file if it is a CTRL-Z.
         */
        if ((filepos = _lseek(fh, -1, SEEK_END)) == -1)
        {
            /*
             * OS error -- should ignore negative seek error,
             * since that means we had a zero-length file.
             */
            if (GetLastError() != ERROR_NEGATIVE_SEEK)
            {
                _close(fh);
                return -1;
            }
        }
        else
        {
            /*
             * Seek was OK, read the last char in file. The last
             * char is a CTRL-Z if and only if _read returns 0
             * and ch ends up with a CTRL-Z.
             */
            ch = 0;
            if (_read(fh, &ch, 1) == 0 && ch == 26)
            {
                /* read was OK and we got CTRL-Z! Wipe it out! */
                if (_chsize(fh,filepos) == -1)
                {
                    _close(fh);
                    return -1;
                }
            }

            /* now rewind the file to the beginning */
            if ((filepos = _lseek(fh, 0, SEEK_SET)) == -1)
            {
                _close(fh);
                return -1;
            }
        }
    }

    /*
     * Set FAPPEND flag if appropriate. Don't do this for devices or pipes.
     */
    if (!(fileflags & (FDEV|FPIPE)) && (oflag & _O_APPEND))
        _osfile(fh) |= FAPPEND;

    return fh;
}

