/****************************************************************************
 *                                                                          *
 * File    : read.c                                                         *
 *                                                                          *
 * Purpose : lowio read function -- win32 version.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xio.h"
#include "xcrt.h"

/*
 * Attempts to read cnt bytes from fh into a buffer.
 * If the file is in text mode, CR-LF's are mapped to LF's,
 * thus affecting the number of characters read. This does
 * not affect the file pointer.
 *
 * NOTE: The stdio _IOCTRLZ flag is tied to the use of FEOFLAG.
 * Cross-reference the two symbols before changing FEOFLAG's use.
 */

#define LF  10      /* line feed */
#define CR  13      /* carriage return */
#define CTRLZ  26   /* ctrl-z means eof for text */

/* read bytes from a file handle */
int __cdecl (_read)(int fh, void *buf, unsigned cnt)
{
    int bytes_read;
    char *buffer;           /* buffer to read to */
    int os_read;            /* bytes read on OS call */
    char *p, *q;            /* pointers into buffer */
    //ULONG filepos;          /* file position after seek */
    unsigned long oserr;

    /* validate handle */
    if ((unsigned)fh >= (unsigned)__iolim || !(_osfile(fh) & FOPEN))
    {
        errno = EBADF;
        return -1;
    }

    bytes_read = 0;  /* nothing read yet */
    buffer = buf;

    if (cnt == 0 || (_osfile(fh) & FEOFLAG))
    {
        /* nothing to read or at EOF, so return */
        return 0;
    }

    if ((_osfile(fh) & (FPIPE|FDEV)) && _pipech(fh) != LF)
    {
        /* a pipe/device and pipe lookahead non-empty: read the lookahead char */
        *buffer++ = _pipech(fh);
        ++bytes_read;
        --cnt;
        _pipech(fh) = LF;  /* mark as empty */
    }

    /* read the data */
    if (!ReadFile(_osfhnd(fh), buffer, cnt, (DWORD *)&os_read, 0))
    {
        if ((oserr = GetLastError()) == ERROR_ACCESS_DENIED)
        {
            /* wrong read/write mode should return EBADF, not EACCES */
            errno = EBADF;
            return -1;
        }
        else if (oserr == ERROR_BROKEN_PIPE)
        {
            /*
             * Just return 0 if ERROR_BROKEN_PIPE has occurred. it
             * means the handle is a read-handle on a pipe for which
             * all write-handles have been closed and all data has
             * been read.
             */
            return 0;
        }
        else
        {
            __maposerr(oserr);
            return -1;
        }
    }

    bytes_read += os_read;  /* update bytes read */

    if (_osfile(fh) & FTEXT)
    {
        /* text mode, translate CR-LF to LF in the buffer */

        /* set CRLF flag to indicate LF at beginning of buffer */
        if (os_read != 0 && *(char *)buf == LF)
            _osfile(fh) |= FCRLF;
        else
            _osfile(fh) &= ~FCRLF;

        /* convert chars in the buffer: p is src, q is dest */
        p = q = buf;
        while (p < (char *)buf + bytes_read)
        {
            if (*p == CTRLZ)
            {
                /* if fh is not a device, set ctrl-z flag */
                if (!(_osfile(fh) & FDEV))
                    _osfile(fh) |= FEOFLAG;
                break;  /* stop translating */
            }
            else if (*p != CR)
            {
                *q++ = *p++;
            }
            else
            {
                /* *p is CR, so must check next char for LF */
                if (p < (char *)buf + bytes_read - 1)
                {
                    if (*(p+1) == LF)
                    {
                        p += 2;
                        *q++ = LF;  /* convert CR-LF to LF */
                    }
                    else
                    {
                        *q++ = *p++;  /* store char normally */
                    }
                }
                else
                {
                    char peekchr;

                    /*
                     * This is the hard part. We found a CR at end of
                     * buffer. We must peek ahead to see if next char
                     * is an LF.
                     */
                    ++p;

                    oserr = 0;
                    if (!ReadFile(_osfhnd(fh), &peekchr, 1, (DWORD *)&os_read, 0))
                        oserr = GetLastError();

                    if (oserr != 0 || os_read == 0)
                    {
                        /* couldn't read ahead, store CR */
                        *q++ = CR;
                    }
                    else
                    {
                        /*
                         * peekchr now has the extra character -- we now have several possibilities:
                         *  1. disk file and char is not LF; just seek back and copy CR
                         *  2. disk file and char is LF; seek back and discard CR
                         *  3. disk file, char is LF but this is a one-byte read: store LF, don't seek back
                         *  4. pipe/device and char is LF; store LF.
                         *  5. pipe/device and char isn't LF, store CR and put char in pipe lookahead buffer.
                         */
                        if (_osfile(fh) & (FDEV|FPIPE))
                        {
                            /* non-seekable device */
                            if (peekchr == LF)
                            {
                                *q++ = LF;
                            }
                            else
                            {
                                *q++ = CR;
                                _pipech(fh) = peekchr;
                            }
                        }
                        else
                        {
                            /* disk file */
                            if (q == buf && peekchr == LF)
                            {
                                /* nothing read yet; must make some progress */
                                *q++ = LF;
                            }
                            else
                            {
                                /* seek back */
                                /*filepos =*/ _lseek(fh, -1, FILE_CURRENT);
                                if (peekchr != LF)
                                    *q++ = CR;
                            }
                        }
                    }
                }
            }
        }

        /* we now change bytes_read to reflect the true number of chars in the buffer */
        bytes_read = q - (char *)buf;
    }

    return bytes_read;
}

