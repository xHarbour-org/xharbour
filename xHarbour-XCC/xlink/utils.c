/****************************************************************************
 *                                                                          *
 * File    : utils.c                                                        *
 *                                                                          *
 * Purpose : Win32 Linker; support functions.                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "link.h"

#ifdef PRERELEASE
#ifndef __POCC__
#include <malloc.h>
#endif
#endif /* PRERELEASE */

/****************************************************************************
 *                                                                          *
 * Function: my_alloc                                                       *
 *                                                                          *
 * Purpose : Allocate a memory block.                                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

#ifndef PODEBUG
void *my_alloc(size_t size)
{
    void *vp = malloc(size);

    if (vp == NULL)
        apperror(RCFATAL(ERROR_OUTOFMEMORY));

#ifdef PRERELEASE
    cur_size += my_size(vp);
    tot_size = max(tot_size, cur_size);
#endif

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
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

#ifndef PODEBUG
void *my_realloc(void *vp, size_t newsize)
{
#ifdef PRERELEASE
    if (vp) cur_size -= my_size(vp);
#endif

    vp = realloc(vp, newsize);

    if (vp == NULL)
        apperror(RCFATAL(ERROR_OUTOFMEMORY));

#ifdef PRERELEASE
    cur_size += my_size(vp);
    tot_size = max(tot_size, cur_size);
#endif

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
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

#ifndef PODEBUG
void my_free(void *vp)
{
#ifdef PRERELEASE
    if (vp) cur_size -= my_size(vp);
#endif

    if (vp) free(vp);
}
#endif

/****************************************************************************
 *                                                                          *
 * Function: my_size                                                        *
 *                                                                          *
 * Purpose : Return the size of a memory block.                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

#ifdef PRERELEASE
size_t my_size(void *vp)
{
    return _msize(vp);
}
#endif

/****************************************************************************
 *                                                                          *
 * Function: my_openmap                                                     *
 *                                                                          *
 * Purpose : Create a read-only file mapping.                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
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
 *           98-01-28  Created                                              *
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
 *           98-01-28  Created                                              *
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
 * Function: calculate_executable_checksum                                  *
 *                                                                          *
 * Purpose : Use IMAGEHLP.DLL to calculate the checksum for a executable.   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

bool_t calculate_executable_checksum(FILEINFO *file, long *checksum)
{
    bool_t success = FALSE;
    HANDLE hmod;

    hmod = LoadLibraryA("IMAGEHLP.DLL");
    if (hmod != NULL)
    {
        CHECKSUMMAPPEDFILE pfnCheckSumMappedFile;

        pfnCheckSumMappedFile = (CHECKSUMMAPPEDFILE)GetProcAddress(hmod, "CheckSumMappedFile");
        if (pfnCheckSumMappedFile != NULL)
        {
            long header_checksum;

            success = (pfnCheckSumMappedFile(file->base, file->size,
                &header_checksum, checksum) != NULL);
        }

        FreeLibrary(hmod);
    }

    return success;
}

/****************************************************************************
 *                                                                          *
 * Function: my_deletefile                                                  *
 *                                                                          *
 * Purpose : Delete the given file.                                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
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
 *           98-01-28  Created                                              *
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
 *                                                                          *
 ****************************************************************************/

#define CTRLZ 26
#define CR 13
#define LF 10
char *read_respfile(const char *filename)
{
    char *buf;
    ulong_t size;
    HANDLE hf;

    hf = CreateFileA(filename, GENERIC_READ, FILE_SHARE_READ,
        NULL, OPEN_EXISTING, 0, 0);

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
                    *s = '\t';
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
 *           98-01-28  Created                                              *
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
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

const char *basename(const char *filename)
{
    if (filename != NULL && *filename != '\0')
    {
        const char *s;

        s = strrchr(filename, '\\');
        if (s) return s+1;

        /* No path, return whole name */
        return filename;
    }

    return "";
}

/****************************************************************************
 *                                                                          *
 * Function: add_extension_to_file                                          *
 *                                                                          *
 * Purpose : Add extension to a filename without it.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

void add_extension_to_file(char *filename, const char *ext)
{
    char *s;

    s = strrchr(filename, '\\');
    if (!strchr((s) ? s : filename, '.'))
        strcat(filename, ext);
}

/****************************************************************************
 *                                                                          *
 * Function: update_extension_in_file                                       *
 *                                                                          *
 * Purpose : Update extension in a filename.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

void update_extension_in_file(char *filename, const char *ext)
{
    char *s;

    s = strrchr(filename, '\\');
    if ((s = strchr((s) ? s : filename, '.')) != NULL)
        strcpy(s, ext);
    else
        strcat(filename, ext);
}

/****************************************************************************
 *                                                                          *
 * Function: my_exception_filter                                            *
 *                                                                          *
 * Purpose : Generic exception filter.                                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
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

