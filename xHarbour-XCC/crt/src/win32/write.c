/****************************************************************************
 *                                                                          *
 * File    : write.c                                                        *
 *                                                                          *
 * Purpose : lowio write function -- win32 version.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xio.h"
#include "xcrt.h"

/*
 * Writes count bytes from the buffer to the handle specified.
 * If the file was opened in text mode, each LF is translated to
 * CR-LF. This does not affect the return value. In text mode
 * ^Z indicates end of file.
 */

#define BUF_SIZE  1025      /* size of LF translation buffer */

#define LF  '\n'    /* line feed */
#define CR  '\r'    /* carriage return */
#define CTRLZ  26   /* ctrl-z */

/* write bytes to a file handle */
int __cdecl (_write)(int fh, const void *buf, unsigned cnt)
{
    int lfcount;            /* count of line feeds */
    int charcount;          /* count of chars written so far */
    int written;            /* count of chars written on this write */
    unsigned long oserr;
    char ch;

    /* validate handle */
    if ((unsigned)fh >= (unsigned)__iolim || !(_osfile(fh) & FOPEN))
    {
        errno = EBADF;
        return -1;
    }

    lfcount = charcount = 0;  /* nothing written yet */

    if (cnt == 0)
        return 0;  /* nothing to do */

    if (_osfile(fh) & FAPPEND)
    {
        /* appending - seek to end of file; ignore error, because maybe file doesn't allow seeking */
        (void)_lseek(fh, 0, FILE_END);
    }

    /* check for text mode with LF's in the buffer */
    if (_osfile(fh) & FTEXT)
    {
        char *p;

        /* text mode, translate LF to CR/LF on output */
        p = (char *)buf;
        oserr = 0;

        while ((unsigned)(p - (char *)buf) < cnt)
        {
            char lfbuf[BUF_SIZE], *q = lfbuf;

            /* fill the lf buf, except maybe last char */
            while (q - lfbuf < BUF_SIZE-1 && (unsigned)(p - (char *)buf) < cnt)
            {
                ch = *p++;
                if (ch == LF)
                {
                    ++lfcount;
                    *q++ = CR;
                }
                *q++ = ch;
            }

            /* write the lf buf and update total */
            if (WriteFile(_osfhnd(fh), lfbuf, q - lfbuf, (DWORD *)&written, 0))
            {
                charcount += written;
                if (written < q - lfbuf)
                    break;
            }
            else
            {
                oserr = GetLastError();
                break;
            }
        }
    }
    else
    {
        /* binary mode, no translation */
        if (WriteFile(_osfhnd(fh), buf, cnt, (DWORD *)&written, 0))
        {
            oserr = 0;
            charcount = written;
        }
        else
        {
            oserr = GetLastError();
        }
    }

    if (charcount == 0)
    {
        /*
         * If nothing was written, first check if an OS error,
         * otherwise we return -1 and set errno to ENOSPC,
         * unless a device and first char was CTRL-Z.
         */
        if (oserr != 0)
        {
            if (oserr == ERROR_ACCESS_DENIED)
            {
                /* wrong read/write mode should return EBADF, not EACCES */
                errno = EBADF;
            }
            else
            {
                __maposerr(oserr);
            }
            return -1;
        }
        else if ((_osfile(fh) & FDEV) && *(char *)buf == CTRLZ)
        {
            return 0;
        }
        else
        {
            errno = ENOSPC;
            return -1;
        }
    }
    else
    {
        /* return adjusted bytes written */
        return charcount - lfcount;
    }
}

