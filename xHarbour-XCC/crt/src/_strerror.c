/****************************************************************************
 *                                                                          *
 * File    : _strerror.c                                                    *
 *                                                                          *
 * Purpose : _Strerror function.                                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-10  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <errno.h>
#include <string.h>
#include "xalloc.h"
#include "xthread.h"

#define INIT_STR  "error #xxx"
#define EBUF_SIZE  sizeof(INIT_STR)

/* copy error message into buffer as needed */
char *_Strerror(int errcode, char *buf)
{
    /* switch on known error codes */
    switch (errcode)
    {
        case 0:
            return "no error";
        case ENOENT:
            return "no such file or directory";
        case ENOEXEC:
            return "exec format error";
        case EBADF:
            return "bad file descriptor";
        case ECHILD:
            return "no child processes";
        case EAGAIN:
            return "resource temporarily unavailable";
        case ENOMEM:
            return "not enough space";
        case EACCES:
            return "permission denied";
        case EEXIST:
            return "file exists";
        case EINVAL:
            return "invalid argument";
        case EMFILE:
            return "too many open files";
        case ENOSPC:
            return "no space left on device";
        case EPIPE:
            return "broken pipe";
        case EDOM:
            return "domain error";
        case ERANGE:
            return "range error";
        case EFPOS:
            return "file positioning error";
        case EDEADLK:
            return "resource deadlock avoided";
        case EILSEQ:
            return "multibyte encoding error";
        default:
            if (errcode < 0 || errcode > EILSEQ)
            {
                /* error code outside valid range */
                return "unknown error";
            }
            else
            {
#ifdef __MT__
                if (buf == 0)
                {
                    tiddata *mtd = __get_mtd();
                    if (!mtd->errbuf) mtd->errbuf = (char *)malloc(EBUF_SIZE);
                    buf = mtd->errbuf;
                }

#else /* __MT__ */
                static char sbuf[EBUF_SIZE];
                if (buf == 0)
                    buf = sbuf;
#endif /* __MT__ */

                /* generate numeric error code */
                strcpy(buf, INIT_STR);
                buf[9] = errcode % 10 + '0';
                buf[8] = (errcode /= 10) % 10 + '0';
                buf[7] = (errcode / 10) % 10 + '0';
                return buf;
            }
    }
}

