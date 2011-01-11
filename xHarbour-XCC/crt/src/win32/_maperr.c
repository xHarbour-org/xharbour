/****************************************************************************
 *                                                                          *
 * File    : _maperr.c                                                      *
 *                                                                          *
 * Purpose : Map OS errors to errno values -- win32 version.                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <winerror.h>
#include <errno.h>
#include "xthread.h"
#include "xcrt.h"

/* map OS error to errno value */
void __maposerr(unsigned long oserr)
{
    switch (oserr)
    {
        case ERROR_INVALID_FUNCTION:
        case ERROR_INVALID_ACCESS:
        case ERROR_INVALID_DATA:
        case ERROR_INVALID_PARAMETER:
        case ERROR_NEGATIVE_SEEK:
        default:
            errno = EINVAL;
            break;
        case ERROR_FILE_NOT_FOUND:
        case ERROR_PATH_NOT_FOUND:
        case ERROR_INVALID_DRIVE:
        case ERROR_NO_MORE_FILES:
        case ERROR_BAD_NETPATH:
        case ERROR_BAD_NET_NAME:
        case ERROR_BAD_PATHNAME:
        case ERROR_FILENAME_EXCED_RANGE:
            errno = ENOENT;
            break;
        case ERROR_ARENA_TRASHED:
        case ERROR_NOT_ENOUGH_MEMORY:
        case ERROR_INVALID_BLOCK:
        case ERROR_NOT_ENOUGH_QUOTA:
            errno = ENOMEM;
            break;
        case ERROR_ACCESS_DENIED:
        case ERROR_CURRENT_DIRECTORY:
        case ERROR_LOCK_VIOLATION:
        case ERROR_NETWORK_ACCESS_DENIED:
        case ERROR_CANNOT_MAKE:
        case ERROR_FAIL_I24:
        case ERROR_DRIVE_LOCKED:
        case ERROR_SEEK_ON_DEVICE:
        case ERROR_NOT_LOCKED:
        case ERROR_LOCK_FAILED:
        /* range */
        case ERROR_WRITE_PROTECT:
        case ERROR_BAD_UNIT:
        case ERROR_NOT_READY:
        case ERROR_BAD_COMMAND:
        case ERROR_CRC:
        case ERROR_BAD_LENGTH:
        case ERROR_SEEK:
        case ERROR_NOT_DOS_DISK:
        case ERROR_SECTOR_NOT_FOUND:
        case ERROR_OUT_OF_PAPER:
        case ERROR_WRITE_FAULT:
        case ERROR_READ_FAULT:
        case ERROR_GEN_FAILURE:
        case ERROR_SHARING_VIOLATION:
        case ERROR_WRONG_DISK:
        case ERROR_SHARING_BUFFER_EXCEEDED:
        case ERROR_DIR_NOT_EMPTY:  /* M$ uses ENOTEMPTY */
            errno = EACCES;
            break;
        case ERROR_NO_PROC_SLOTS:
        case ERROR_MAX_THRDS_REACHED:
        case ERROR_NESTING_NOT_ALLOWED:
            errno = EAGAIN;
            break;
        case ERROR_FILE_EXISTS:
        case ERROR_ALREADY_EXISTS:
            errno = EEXIST;
            break;
        case ERROR_INVALID_HANDLE:
        case ERROR_INVALID_TARGET_HANDLE:
        case ERROR_DIRECT_ACCESS_HANDLE:
            errno = EBADF;
            break;
        case ERROR_WAIT_NO_CHILDREN:
        case ERROR_CHILD_NOT_COMPLETE:
            errno = ECHILD;
            break;
        case ERROR_BAD_FORMAT:
        /* range */
        case ERROR_INVALID_STARTING_CODESEG:
        case ERROR_INVALID_STACKSEG:
        case ERROR_INVALID_MODULETYPE:
        case ERROR_INVALID_EXE_SIGNATURE:
        case ERROR_EXE_MARKED_INVALID:
        case ERROR_BAD_EXE_FORMAT:
        case ERROR_ITERATED_DATA_EXCEEDS_64k:
        case ERROR_INVALID_MINALLOCSIZE:
        case ERROR_DYNLINK_FROM_INVALID_RING:
        case ERROR_IOPL_NOT_ENABLED:
        case ERROR_INVALID_SEGDPL:
        case ERROR_AUTODATASEG_EXCEEDS_64k:
        case ERROR_RING2SEG_MUST_BE_MOVABLE:
        case ERROR_RELOC_CHAIN_XEEDS_SEGLIM:
        case ERROR_INFLOOP_IN_RELOC_CHAIN:
            errno = ENOEXEC;
            break;
        case ERROR_TOO_MANY_OPEN_FILES:
            errno = EMFILE;
            break;
        case ERROR_DISK_FULL:
            errno = ENOSPC;
            break;
        case ERROR_BROKEN_PIPE:
            errno = EPIPE;
            break;
        case ERROR_NOT_SAME_DEVICE:
            errno = EXDEV;
            break;
    }
}


#ifdef __MT__
/* return pointer to thread's errno */
int * __cdecl __errno(void)
{
    return &(__get_mtd()->terrno);
}
#endif /* __MT__ */
