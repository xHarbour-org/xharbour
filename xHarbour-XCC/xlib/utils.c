/****************************************************************************
 *                                                                          *
 * File    : utils.c                                                        *
 *                                                                          *
 * Purpose : Win32 Library Manager; support functions.                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "lib.h"

/****************************************************************************
 *                                                                          *
 * Function: my_alloc                                                       *
 *                                                                          *
 * Purpose : Allocate a memory block.                                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *           99-01-12  Removed unnecessary clearing of block (see lib.h).   *
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
 *           97-11-18  Created                                              *
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
 *           97-11-18  Created                                              *
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
 * Function: my_openmap                                                     *
 *                                                                          *
 * Purpose : Create a read-only file mapping.                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR my_openmap(FILEINFO *file)
{
    /*
     * Open the given file.
     */
    file->hf = CreateFileA(file->name, GENERIC_READ, FILE_SHARE_READ,
        NULL, OPEN_EXISTING, 0, 0);

    if (file->hf == INVALID_HANDLE_VALUE)
        return GetLastError();

    /*
     * Create a file mapping of the opened file.
     */
    file->hmap = CreateFileMappingA(file->hf, NULL, PAGE_READONLY, 0, 0, NULL);

    if (file->hmap == NULL)
        return GetLastError();

    /*
     * Map a view of the whole file.
     */
    file->base = MapViewOfFile(file->hmap, FILE_MAP_READ, 0, 0, 0);

    if (file->base == NULL)
        return GetLastError();

    return ERROR_SUCCESS;
}

/****************************************************************************
 *                                                                          *
 * Function: my_createmap                                                   *
 *                                                                          *
 * Purpose : Create a read-write file mapping.                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR my_createmap(FILEINFO *file, ulong_t maxsize)
{
    /*
     * Create the given file.
     */
    file->hf = CreateFileA(file->name, GENERIC_READ|GENERIC_WRITE, 0,
        NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);

    if (file->hf == INVALID_HANDLE_VALUE)
        return GetLastError();

    /*
     * Create a file mapping of the opened file.
     */
    file->hmap = CreateFileMappingA(file->hf, NULL, PAGE_READWRITE, 0, maxsize, NULL);

    if (file->hmap == NULL)
        return GetLastError();

    /*
     * Map a view of the whole file.
     */
    file->base = MapViewOfFile(file->hmap, FILE_MAP_WRITE, 0, 0, 0);

    if (file->base == NULL)
        return GetLastError();

    return ERROR_SUCCESS;
}

/****************************************************************************
 *                                                                          *
 * Function: my_closemap                                                    *
 *                                                                          *
 * Purpose : Close a file mapping.                                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR my_closemap(FILEINFO *file, bool_t force_eof)
{
    if (file->base)
    {
        if (!UnmapViewOfFile(file->base))
            return GetLastError();

        file->base = NULL;
    }

    if (file->hmap)
    {
        if (!CloseHandle(file->hmap))
            return GetLastError();

        file->hmap = NULL;
    }

    if (file->hf)
    {
        if (force_eof)
        {
            if (SetFilePointer(file->hf, file->size, 0, FILE_BEGIN) == 0xFFFFFFFF)
                return GetLastError();

            if (!SetEndOfFile(file->hf))
                return GetLastError();
        }

        if (!CloseHandle(file->hf))
            return GetLastError();

        file->hf = NULL;
    }

    return ERROR_SUCCESS;
}

/****************************************************************************
 *                                                                          *
 * Function: my_create_binfile                                              *
 *                                                                          *
 * Purpose : Write a binary file to disk.                                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

WINERR my_create_binfile(const char *filename, const void *buf, ulong_t nbuf)
{
    HANDLE hf;
    ulong_t nwritten;
    WINERR err;

    /*
     * Create the given file.
     */
    hf = CreateFileA(filename, GENERIC_WRITE, 0,
        NULL, CREATE_NEW, FILE_ATTRIBUTE_NORMAL, 0);

    if (hf == INVALID_HANDLE_VALUE)
        return GetLastError();

    /*
     * Write the data & check for errors.
     */
    if (WriteFile(hf, buf, nbuf, &nwritten, NULL))
        err = (nwritten < nbuf) ? ERROR_WRITE_FILE : ERROR_SUCCESS;
    else
        err = GetLastError();

    /*
     * Close the file. Delete it, if we failed.
     */
    if (!CloseHandle(hf) && err == ERROR_SUCCESS)
        err = GetLastError();

    if (err != ERROR_SUCCESS)
        my_deletefile(filename);

    return err;
}

/****************************************************************************
 *                                                                          *
 * Function: my_deletefile                                                  *
 *                                                                          *
 * Purpose : Delete the given file.                                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
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
 *           97-11-18  Created                                              *
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
 * Function: read_respfile                                                  *
 *                                                                          *
 * Purpose : Read a response file from disk.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           99-01-29  Created                                              *
 *           04-01-13  Added filler argument ('\t' for args; '\n' for DEF). *
 *                                                                          *
 ****************************************************************************/

#define CTRLZ  26
#define CR  13
#define LF  10
char *read_respfile(const char *filename, char filler)
{
    char *buf;
    ulong_t size;
    HANDLE hf;

    hf = CreateFileA(filename, GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, 0);
    if (hf == INVALID_HANDLE_VALUE)
        return NULL;

    size = GetFileSize(hf, NULL);

    buf = (char *)malloc(size+1);
    if (buf != NULL)
    {
        ulong_t nread;

        if (!ReadFile(hf, buf, size, &nread, NULL) || nread < size)
        {
            free(buf);
            buf = NULL;
        }
        else
        {
            char *s;

            for (s = buf; s < buf + size; s++)
            {
                if (*s == CR || *s == LF)
                    *s = filler;
                else if (*s == CTRLZ)
                    break;
            }
            *s = '\0';
        }
    }

    CloseHandle(hf);
    return buf;
}
#undef CTRLZ
#undef CR
#undef LF

/****************************************************************************
 *                                                                          *
 * Function: tokenize                                                       *
 *                                                                          *
 * Purpose : chop up a string into an array of pointers (funky).            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *           99-01-29  Rewritten; now also returns "argc".                  *
 *                                                                          *
 ****************************************************************************/

#define DELIMITERS " \t"
void tokenize(char *input, int *argcp, char ***argvp)
{
    int alen = 0;  /* initial guess */
    int argc = 0;

    *argvp = NULL;

    for (input = strtok(input, DELIMITERS);
         input != NULL;
         input = strtok(NULL, DELIMITERS))
    {
        if (argc == alen)
        {
            *argvp = (char **)my_realloc((char **)(*argvp),
                (alen += 10) * sizeof(char *));
        }

        (*argvp)[argc++] = input;
    }

    if (argcp) *argcp = argc;
}
#undef DELIMITERS

/****************************************************************************
 *                                                                          *
 * Function: basename                                                       *
 *                                                                          *
 * Purpose : Return filename part of a pathname.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *           03-05-16  Added scan for forward slash.                        *
 *                                                                          *
 ****************************************************************************/

const char *basename(const char *filename)
{
    if (filename != NULL && *filename != '\0')
    {
        const char *s;

        s = strrchr(filename, '\\');
        if (s) return ++s;

        s = strrchr(filename, '/');
        if (s) return ++s;

        /* No path, return whole name */
        return filename;
    }

    return "";
}

/****************************************************************************
 *                                                                          *
 * Function: my_exception_filter                                            *
 *                                                                          *
 * Purpose : Generic exception filter.                                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           99-05-29  Created                                              *
 *                                                                          *
 ****************************************************************************/

int my_exception_filter(DWORD sehcode, WINERR *errp)
{
    UNREFERENCED_PARAMETER(sehcode);

#ifdef PRERELEASE
    UNREFERENCED_PARAMETER(errp);
    return EXCEPTION_CONTINUE_SEARCH;
#else
    *errp = ERROR_INTERNAL;
    return EXCEPTION_EXECUTE_HANDLER;
#endif
}

