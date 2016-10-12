/****************************************************************************
 *                                                                          *
 * File    : utils.c                                                        *
 *                                                                          *
 * Purpose : Win32 Resource Compiler; misc support functions.               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "rc.h"

/****************************************************************************
 *                                                                          *
 * Function: bitcount                                                       *
 *                                                                          *
 * Purpose : Count number of bits in argument.                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *                                                                          *
 ****************************************************************************/

int bitcount(uint_t n)
{
    int c;

    for (c = 0, --n; n != 0; ++c, n >>= 1)
        ;

    return c;
}

/****************************************************************************
 *                                                                          *
 * Function: getdigit                                                       *
 *                                                                          *
 * Purpose : Return numeric value of a character digit.                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *                                                                          *
 ****************************************************************************/

int getdigit(int n)
{
    if ('0' <= n && n <= '9')
        n -= '0';
    else if ('a' <= n && n <= 'f')
        n -= 'a'-10;
    else if ('A' <= n && n <= 'F')
        n -= 'A'-10;
    else
        n = -1;

    return n;
}

/****************************************************************************
 *                                                                          *
 * Function: newstring                                                      *
 *                                                                          *
 * Purpose : Allocate and intialize a substring from another string.        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *                                                                          *
 ****************************************************************************/

uchar_t *newstring(const uchar_t *s, size_t len, size_t pos)
{
    uchar_t *ns = (uchar_t *)my_alloc((len+pos+1) * sizeof(uchar_t));

    ns[len+pos] = '\0';
    return (uchar_t *)strncpy((char *)ns+pos, (char *)s, len) - pos;
}

/****************************************************************************
 *                                                                          *
 * Function: my_alloc                                                       *
 *                                                                          *
 * Purpose : Allocate a memory block.                                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *                                                                          *
 ****************************************************************************/

#ifndef PODEBUG
void *my_alloc(size_t size)
{
    void *vp = malloc(size);

    if (vp == NULL)
        apperror(RCFATAL(ERROR_OUTOFMEMORY));

    return vp;
}
#endif

/****************************************************************************
 *                                                                          *
 * Function: my_realloc                                                     *
 *                                                                          *
 * Purpose : Re-allocate a memory block.                                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *                                                                          *
 ****************************************************************************/

#ifndef PODEBUG
void *my_realloc(void *vp, size_t newsize)
{
    vp = realloc(vp, newsize);

    if (vp == NULL)
        apperror(RCFATAL(ERROR_OUTOFMEMORY));

    return vp;
}
#endif

/****************************************************************************
 *                                                                          *
 * Function: my_free                                                        *
 *                                                                          *
 * Purpose : Free a memory block.                                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *                                                                          *
 ****************************************************************************/

#ifndef PODEBUG
void my_free(void *vp)
{
    if (vp) free(vp);
}
#endif

/****************************************************************************
 *                                                                          *
 * Function: my_openfile                                                    *
 *                                                                          *
 * Purpose : Wrapper around CreateFile().                                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR my_openfile(const char *filename, HANDLE *phf)
{
    *phf = CreateFileA(filename, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, 0);

    if (*phf == INVALID_HANDLE_VALUE)
        return GetLastError();
    else
        return ERROR_SUCCESS;
}

/****************************************************************************
 *                                                                          *
 * Function: my_createfile                                                  *
 *                                                                          *
 * Purpose : Wrapper around CreateFile().                                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR my_createfile(const char *filename, HANDLE *phf)
{
    *phf = CreateFileA(filename, GENERIC_READ|GENERIC_WRITE, 0,
        NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);

    if (*phf == INVALID_HANDLE_VALUE)
        return GetLastError();
    else
        return ERROR_SUCCESS;
}

/****************************************************************************
 *                                                                          *
 * Function: my_readfile                                                    *
 *                                                                          *
 * Purpose : Wrapper around ReadFile().                                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR my_readfile(HANDLE hf, void *buf, ulong_t nbuf)
{
    ulong_t nread;

    if (ReadFile(hf, buf, nbuf, &nread, NULL))
        return (nread < nbuf) ? ERROR_READ_FILE : ERROR_SUCCESS;
    else
        return GetLastError();
}

/****************************************************************************
 *                                                                          *
 * Function: my_writefile                                                   *
 *                                                                          *
 * Purpose : Wrapper around WriteFile().                                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *           01-11-09  Check for NULL file handle added.                    *
 *                                                                          *
 ****************************************************************************/

WINERR my_writefile(HANDLE hf, const void *buf, ulong_t nbuf)
{
    ulong_t nwritten;

    if (idemode && hf == NULL)
        return ERROR_SUCCESS;

    if (WriteFile(hf, buf, nbuf, &nwritten, NULL))
        return (nwritten < nbuf) ? ERROR_WRITE_FILE : ERROR_SUCCESS;
    else
        return GetLastError();
}

/****************************************************************************
 *                                                                          *
 * Function: my_seekfile                                                    *
 *                                                                          *
 * Purpose : Wrapper around SetFilePointer().                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR my_seekfile(HANDLE hf, long offset, uint_t method)
{
    if (SetFilePointer(hf, offset, NULL, method) == 0xFFFFFFFF)
        return GetLastError();
    else
        return ERROR_SUCCESS;
}

/****************************************************************************
 *                                                                          *
 * Function: my_tellfile                                                    *
 *                                                                          *
 * Purpose : Wrapper around SetFilePointer().                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *                                                                          *
 ****************************************************************************/

ulong_t my_tellfile(HANDLE hf)
{
    return SetFilePointer(hf, 0, NULL, FILE_CURRENT);
}

/****************************************************************************
 *                                                                          *
 * Function: my_filesize                                                    *
 *                                                                          *
 * Purpose : Return the size of the given file.                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR my_filesize(const char *filename, ulong_t *sizep)
{
    WINERR err;
    HANDLE hf;

    err = my_openfile(filename, &hf);
    if (err == ERROR_SUCCESS)
    {
        *sizep = GetFileSize(hf, NULL);
        my_closefile(hf);
    }

    return err;
}

/****************************************************************************
 *                                                                          *
 * Function: my_deletefile                                                  *
 *                                                                          *
 * Purpose : Delete the given file.                                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR my_deletefile(const char *filename)
{
    if (!DeleteFileA(filename))
        return GetLastError();
    else
        return ERROR_SUCCESS;
}

/****************************************************************************
 *                                                                          *
 * Function: my_renamefile                                                  *
 *                                                                          *
 * Purpose : Rename the given file.                                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR my_renamefile(const char *oldname, const char *newname)
{
    if (!MoveFileA(oldname, newname))
        return GetLastError();
    else
        return ERROR_SUCCESS;
}

/****************************************************************************
 *                                                                          *
 * Function: my_isfile                                                      *
 *                                                                          *
 * Purpose : Return true if the given file exists.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *                                                                          *
 ****************************************************************************/

bool_t my_isfile(const char *filename)
{
    WIN32_FIND_DATAA wfd;
    HANDLE hff;

    hff = FindFirstFileA(filename, &wfd);
    if (hff != INVALID_HANDLE_VALUE)
    {
        FindClose(hff);
        return TRUE;
    }

    return FALSE;
}

/****************************************************************************
 *                                                                          *
 * Function: my_copystream                                                  *
 *                                                                          *
 * Purpose : Copy bytes from one file handle to another.                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *           01-11-09  Check for NULL file handle added.                    *
 *                                                                          *
 ****************************************************************************/

WINERR my_copystream(HANDLE hfd, HANDLE hfs)
{
    char buf[4096];  /* don't allocate from heap */
    ulong_t totsize;
    ulong_t blocksize;
    WINERR err;

    if (idemode && hfd == NULL)
        return ERROR_SUCCESS;

    for (totsize = GetFileSize(hfs, NULL) - my_tellfile(hfs);
         totsize != 0;
         totsize -= blocksize)
    {
        blocksize = min(totsize, sizeof(buf));

        err = my_readfile(hfs, buf, blocksize);
        if (err) return err;

        err = my_writefile(hfd, buf, blocksize);
        if (err) return err;
    }

    return ERROR_SUCCESS;
}

/****************************************************************************
 *                                                                          *
 * Function: my_fullpath                                                    *
 *                                                                          *
 * Purpose : Create a full path.                                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-10-14  Created                                              *
 *           99-01-26  Bugfix: didn't return anything from else-part.       *
 *                                                                          *
 ****************************************************************************/

size_t my_fullpath(char *buf, const char *relpath, const char *curpath)
{
    if (relpath[0] == '\\' || (relpath[0] && relpath[1] == ':') ||
        curpath == NULL || *curpath == '\0')
    {
        return strlen(strcpy(buf, relpath));
    }
    else
    {
        const char *p;
        char *s;

        strcpy(buf, curpath);
        s = buf + strlen(curpath);

        p = relpath;
        while (*p)
        {
            if (*p == '.')
            {
                if (*++p == '.')
                {
                    while (--s > buf && *s != '\\')
                        ;

                    p++;
                }

                if (*p++ != '\\')
                    return 0;
            }
            else
            {
                if (s[-1] != '\\')
                    *s++ = '\\';

                while (*p && *p != '\\')
                    *s++ = *p++;

                if (*p) p++;
            }
        }

        *s = '\0';
        return strlen(buf);  /* bugfix: 99-01-26 */
    }
}

