/****************************************************************************
 *                                                                          *
 * File    : pipe.c                                                         *
 *                                                                          *
 * Purpose : lowio pipe function -- win32 version.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <fcntl.h>
#include "xio.h"
#include "xcrt.h"

/* open a pipe */
int __cdecl (_pipe)(int phandles[2], unsigned psize, int textmode)
{
    HANDLE read_handle, write_handle;
    SECURITY_ATTRIBUTES sa;

    sa.nLength = sizeof(sa);
    sa.lpSecurityDescriptor = 0;
    sa.bInheritHandle = (textmode & _O_NOINHERIT) ? FALSE : TRUE;

    if (!CreatePipe(&read_handle, &write_handle, &sa, psize))
    {
        __maposerr(GetLastError());
        return -1;
    }

    /* now we must allocate C Runtime handles for Read and Write handles */
    if ((phandles[0] = __new_osfhnd()) != -1)
    {
        _osfile(phandles[0]) = (char)(FOPEN|FPIPE|FTEXT);

        if ((phandles[1] = __new_osfhnd()) != -1)
        {
            _osfile(phandles[1]) = (char)(FOPEN|FPIPE|FTEXT);

            if (textmode & _O_BINARY)
            {
                /* binary mode */
                _osfile(phandles[0]) &= ~FTEXT;
                _osfile(phandles[1]) &= ~FTEXT;
            }

            if (textmode & _O_NOINHERIT)
            {
                _osfile(phandles[0]) |= FNOINHERIT;
                _osfile(phandles[1]) |= FNOINHERIT;
            }

            __set_osfhnd(phandles[0], read_handle);
            __set_osfhnd(phandles[1], write_handle);
        }
        else
        {
            _osfile(phandles[0]) = 0;
            errno = EMFILE;  /* too many files */
        }
    }
    else
    {
        errno = EMFILE;  /* too many files */
    }

    /* If error occurred, close Win32 handles and return -1 */
    if (phandles[0] == -1 || phandles[1] == -1)
    {
        CloseHandle(read_handle);
        CloseHandle(write_handle);
        return -1;
    }

    return 0;
}

