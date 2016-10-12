/****************************************************************************
 *                                                                          *
 * File    : findfile.c                                                     *
 *                                                                          *
 * Purpose : lowio findfile functions -- win32 version.                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <time.h>
#include "xio.h"

/* convert Win32 FILETIME to C time_t */
time_t __cdecl __time_t_from_ft(FILETIME *pft)
{
    SYSTEMTIME st;
    FILETIME lft;
    struct tm tm;

    /*
     * We cannot directly use the ft value. In Win32, the file times
     * returned by the API are ambiguous. In Windows NT, they are UTC.
     * In Win32S, and probably also Win32C, they are local time
     * values. Thus, the value in ft must be converted to a local
     * time value (by an API) before we can use it.
     */

    /* 0 FILETIME returns a -1 time_t */
    if (pft->dwLowDateTime == 0 && pft->dwHighDateTime == 0)
        return -1L;

    /*
     * Convert to a broken down local time value.
     */
    if (!FileTimeToLocalFileTime(pft, &lft) ||
        !FileTimeToSystemTime(&lft, &st))
        return -1L;

    tm.tm_sec = st.wSecond;
    tm.tm_min = st.wMinute;
    tm.tm_hour = st.wHour;
    tm.tm_mday = st.wDay;
    tm.tm_mon = st.wMonth - 1;
    tm.tm_year = st.wYear - 1900;
    tm.tm_wday = 0;
    tm.tm_yday = 0;
    tm.tm_isdst = -1;  /* "don't know" */

    return mktime(&tm);
}


/* find first matching file */
long __cdecl (_findfirst)(const char *filespec, struct _finddata_t *pfd)
{
    WIN32_FIND_DATA wfd;
    HANDLE handle;

    if ((handle = FindFirstFileA(filespec, &wfd)) == INVALID_HANDLE_VALUE)
    {
        switch (GetLastError())
        {
            case ERROR_NO_MORE_FILES:
            case ERROR_FILE_NOT_FOUND:
            case ERROR_PATH_NOT_FOUND:
                errno = ENOENT;
                break;

            case ERROR_NOT_ENOUGH_MEMORY:
                errno = ENOMEM;
                break;

            default:
                errno = EINVAL;
                break;
        }
        return -1;
    }

    pfd->attrib = (wfd.dwFileAttributes == FILE_ATTRIBUTE_NORMAL) ? 0 : wfd.dwFileAttributes;
    pfd->time_create = __time_t_from_ft(&wfd.ftCreationTime);
    pfd->time_access = __time_t_from_ft(&wfd.ftLastAccessTime);
    pfd->time_write  = __time_t_from_ft(&wfd.ftLastWriteTime);
#ifdef __FSIZE_T_64
    pfd->size = (long long)wfd.nFileSizeHigh * 0x100000000LL + (long long)wfd.nFileSizeLow;
#else
    pfd->size = wfd.nFileSizeLow;
#endif
    strcpy(pfd->name, (char *)wfd.cFileName);

    return (long)handle;
}


/* find next matching file */
int __cdecl (_findnext)(long handle, struct _finddata_t *pfd)
{
    WIN32_FIND_DATA wfd;

    if (!FindNextFile((HANDLE)handle, &wfd))
    {
        switch (GetLastError())
        {
            case ERROR_NO_MORE_FILES:
            case ERROR_FILE_NOT_FOUND:
            case ERROR_PATH_NOT_FOUND:
                errno = ENOENT;
                break;

            case ERROR_NOT_ENOUGH_MEMORY:
                errno = ENOMEM;
                break;

            default:
                errno = EINVAL;
                break;
        }
        return -1;
    }

    pfd->attrib = (wfd.dwFileAttributes == FILE_ATTRIBUTE_NORMAL) ? 0 : wfd.dwFileAttributes;
    pfd->time_create = __time_t_from_ft(&wfd.ftCreationTime);
    pfd->time_access = __time_t_from_ft(&wfd.ftLastAccessTime);
    pfd->time_write  = __time_t_from_ft(&wfd.ftLastWriteTime);
#ifdef __FSIZE_T_64
    pfd->size = (long long)wfd.nFileSizeHigh * 0x100000000LL + (long long)wfd.nFileSizeLow;
#else
    pfd->size = wfd.nFileSizeLow;
#endif
    strcpy(pfd->name, (char *)wfd.cFileName);

    return 0;
}


/* release resources of find */
int __cdecl (_findclose)(long handle)
{
    if (!FindClose((HANDLE)handle))
    {
        errno = EINVAL;
        return -1;
    }
    return 0;
}

