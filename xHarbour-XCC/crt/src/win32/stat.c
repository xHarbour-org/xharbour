/****************************************************************************
 *                                                                          *
 * File    : stat.c                                                         *
 *                                                                          *
 * Purpose : _stat function -- win32 version.                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           02-10-16  Created                                              *
 *                                                                          *
 ****************************************************************************/

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <errno.h>
#include <direct.h>
#include <sys\stat.h>

#define ISSLASH(c)  ((c) == '\\' || (c) == '/')

/* helper function in findfile.c */
extern time_t __cdecl __time_t_from_ft(FILETIME *);


/* return true if the argument is a UNC name specifying a root share */
static int isrootshare(const char *path)
{
    /* if a root UNC name, path will start with 2 (but not 3) slashes */
    if (strlen(path) >= 5 && ISSLASH(path[0]) && ISSLASH(path[1]))
    {
        const char *p = path + 2;

        /*
         * find slash between the server name and share name.
         */
        while (*++p)
            if (ISSLASH(*p))
                break;

        if (p[0] && p[1])
        {
            /*
             * is there a further slash?
             */
            while (*++p)
                if (ISSLASH(*p))
                    break;

            /*
             * just final slash (or no final slash).
             */
            if (!p[0] || !p[1])
                return 1;
        }
    }

    return 0;
}


/* convert Windows file attributes to Unix attributes */
static unsigned short touxmode(unsigned int attr, const char *name)
{
    unsigned short uxmode;
    unsigned int dosmode;
    const char *p;

    dosmode = attr & 0xff;
    if ((p = name)[1] == ':')
        p += 2;

    /* check to see if this is a directory - we must make a special check for the root */
    uxmode = (unsigned short)(((ISSLASH(p[0]) && p[1] == '\0') ||
        (dosmode & FILE_ATTRIBUTE_DIRECTORY) || p[0] == '\0') ? _S_IFDIR|_S_IEXEC : _S_IFREG);

    /* if attribute byte does not have read-only bit, it is read-write */
    uxmode |= (dosmode & FILE_ATTRIBUTE_READONLY) ? _S_IREAD : (_S_IREAD|_S_IWRITE);

    /* see if file appears to be executable - check extension of name */
    if ((p = strrchr(name, '.')) != 0)
    {
        if (_stricmp(p, ".exe") == 0 ||
            _stricmp(p, ".cmd") == 0 ||
            _stricmp(p, ".bat") == 0 ||
            _stricmp(p, ".com") == 0)
            uxmode |= _S_IEXEC;
    }

    /* propagate user read/write/execute bits to group/other fields */
    uxmode |= (uxmode & 0700) >> 3;
    uxmode |= (uxmode & 0700) >> 6;

    return uxmode;
}


/* get file status info */
int __cdecl _stat(const char *name, struct _stat *buf)
{
    char *path;
    char pathbuf[FILENAME_MAX];
    int drive;
    HANDLE handle;
    WIN32_FIND_DATA wfd;

    /* don't allow wildcards to be interpreted by system */
    if (strpbrk(name, "?*") != 0)
    {
        errno = ENOENT;
        return -1;
    }

    /* try to get disk from name */
    if (name[1] == ':')
    {
        if (name[0] != '\0' && name[2] == '\0')
        {
            errno = ENOENT;
            return -1;
        }
        drive = tolower(*name) - 'a' + 1;  /* A: = 1, B: = 2, etc. */
    }
    else
    {
        drive = _getdrive();
    }

    if ((handle = FindFirstFileA(name, &wfd)) == INVALID_HANDLE_VALUE)
    {
        if (!(strpbrk(name, "./\\") &&
             (path = _fullpath(pathbuf, name, FILENAME_MAX)) != 0 &&
             /* root dir ('C:\') or UNC root dir ('\\server\share\') */
             ((strlen(path) == 3) || isrootshare(path)) &&
             (GetDriveType(path) > 1)))
        {
            errno = ENOENT;
            return -1;
        }

        /* root directories are fabricated */
        wfd.dwFileAttributes = FILE_ATTRIBUTE_DIRECTORY;
        wfd.nFileSizeHigh = 0;
        wfd.nFileSizeLow = 0;
        wfd.cFileName[0] = '\0';

        buf->st_mtime = (time_t)-1;
        buf->st_atime = (time_t)-1;
        buf->st_ctime = (time_t)-1;
    }
    else
    {
        buf->st_mtime = __time_t_from_ft(&wfd.ftLastWriteTime);
        buf->st_atime = __time_t_from_ft(&wfd.ftLastAccessTime);
        buf->st_ctime = __time_t_from_ft(&wfd.ftCreationTime);

        FindClose(handle);
    }

    /* fill in buf */
    buf->st_mode = touxmode(wfd.dwFileAttributes, name);
    buf->st_nlink = 1;
    buf->st_size = wfd.nFileSizeLow;
    buf->st_uid = buf->st_gid = buf->st_ino = 0;
    buf->st_rdev = buf->st_dev = (_dev_t)(drive - 1);  /* A=0, B=1, etc. */

    return 0;
}

