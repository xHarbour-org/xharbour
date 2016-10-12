/****************************************************************************
 *                                                                          *
 * File    : archive.c                                                      *
 *                                                                          *
 * Purpose : Win32 Library Manager; existing archive management.            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "lib.h"

/* Static function prototypes */
static int __stdcall list_archive_member(FILEINFO *, const char *, size_t, const void *, void *);
static int __stdcall extract_archive_member(FILEINFO *, const char *, size_t, const void *, void *);
static int __stdcall explode_archive_member(FILEINFO *, const char *, size_t, const void *, void *);
static void dump_archive_member(const char *, size_t, const void *);
static COFF_ARHDR *next_archive_member(const COFF_ARHDR *);
static COFF_ARHDR *open_archive_map(FILEINFO *);
static void close_archive_map(FILEINFO *);

/****************************************************************************
 *                                                                          *
 * Function: list_archive_file                                              *
 *                                                                          *
 * Purpose : Display information about all archive members.                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

void list_archive_file(void)
{
    enum_archive_members(lib_file, list_archive_member, NULL);
}

/****************************************************************************
 *                                                                          *
 * Function: list_archive_member                                            *
 *                                                                          *
 * Purpose : Display information about a single archive member.             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *           97-12-27  Check for EXE extension added.                       *
 *                                                                          *
 ****************************************************************************/

static int __stdcall list_archive_member(FILEINFO *lib_file,
    const char *member_name, size_t member_size,
    const void *member_ptr, void *arg)
{
    long member_offset = file_offset(lib_file, member_ptr);
    char *s;

    UNREFERENCED_PARAMETER(arg);

    s = strrchr(member_name, '.');
    if (s && (_stricmp(s, EXT_DLL) == 0 || _stricmp(s, EXT_EXE) == 0))
    {
        printf("%s:%s\n", member_name,
            import_archive_member_name(member_ptr));
    }
    else
    {
        if (options.verbose)
        {
            printmsg(MSG_LISTING_MEMBER,
                member_offset, member_size, member_name);
        }
        else
        {
            printf("%s\n", member_name);
        }
    }

    return 1;
}

/****************************************************************************
 *                                                                          *
 * Function: extract_archive_members                                        *
 *                                                                          *
 * Purpose : Extract specified archive members.                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

void extract_archive_members(void)
{
    NAMENTRY *nam;

    enum_archive_members(lib_file, extract_archive_member, NULL);

    /* If we have any entries left, they were not found... */
    for (nam = extract_list; nam != NULL; nam = nam->next)
        apperror(RCWARNING(ERROR_CANT_FIND_MEMBER), nam->name);
}

/****************************************************************************
 *                                                                          *
 * Function: extract_archive_member                                         *
 *                                                                          *
 * Purpose : Extract a single archive member.                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *           04-01-13  Also try members without explicit path (03-08-12).   *
 *                                                                          *
 ****************************************************************************/

static int __stdcall extract_archive_member(FILEINFO *lib_file,
    const char *member_name, size_t member_size,
    const void *member_ptr, void *arg)
{
    NAMENTRY *nam;

    UNREFERENCED_PARAMETER(lib_file);
    UNREFERENCED_PARAMETER(arg);

    for (nam = extract_list; nam != NULL; nam = nam->next)
    {
        if (_stricmp(nam->name, member_name) == 0)
        {
            /* Write member to disk; remove it from the list */
            dump_archive_member(member_name, member_size, member_ptr);
            remove_name_from_list(&extract_list, nam);

            /* Continue if more to extract... */
            return (extract_list != NULL);
        }
    }

    /* No success; try without explicit path */
    for (nam = extract_list; nam != NULL; nam = nam->next)
    {
        if (_stricmp(nam->name, basename(member_name)) == 0)
        {
            /* Write member to disk; remove it from the list */
            dump_archive_member(member_name, member_size, member_ptr);
            remove_name_from_list(&extract_list, nam);

            /* Continue if more to extract... */
            return (extract_list != NULL);
        }
    }

    return 1;
}

/****************************************************************************
 *                                                                          *
 * Function: explode_archive_file                                           *
 *                                                                          *
 * Purpose : Create separate files of all archive members.                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

void explode_archive_file(void)
{
    enum_archive_members(lib_file, explode_archive_member, NULL);
}

/****************************************************************************
 *                                                                          *
 * Function: explode_archive_member                                         *
 *                                                                          *
 * Purpose : Create a file of a single archive member.                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int __stdcall explode_archive_member(FILEINFO *lib_file,
    const char *member_name, size_t member_size,
    const void *member_ptr, void *arg)
{
    UNREFERENCED_PARAMETER(lib_file);
    UNREFERENCED_PARAMETER(arg);

    dump_archive_member(member_name, member_size, member_ptr);
    return 1;
}

/****************************************************************************
 *                                                                          *
 * Function: dump_archive_member                                            *
 *                                                                          *
 * Purpose : Write a single archive member to disk.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void dump_archive_member(const char *member_name,
    size_t member_size, const void *member_ptr)
{
    const char *fname;
    WINERR err;

    fname = basename(member_name);

    if (options.verbose)
        printmsg(MSG_WRITING_MEMBER, fname);

    err = my_create_binfile(fname, member_ptr, member_size);
    if (err) apperror(RCFATAL(err), fname);
}

/****************************************************************************
 *                                                                          *
 * Function: enum_archive_members                                           *
 *                                                                          *
 * Purpose : Open archive and walk through all members.                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *           97-12-12  Accept archives without linker member 2.             *
 *           98-01-18  Changed to ERROR_NO_LONGNAMES_MEMBER message.        *
 *                                                                          *
 ****************************************************************************/

void enum_archive_members(FILEINFO *lib_file, int (__stdcall *callback)
    (FILEINFO *, const char *, size_t, const void *, void *), void *arg)
{
    COFF_ARHDR *arhdr;
    COFF_ARHDR *arhdr_linker1;
    COFF_ARHDR *arhdr_linker2;
    COFF_ARHDR *arhdr_longnames;

    arhdr = open_archive_map(lib_file);

    arhdr_linker1 = arhdr_linker2 = arhdr_longnames = NULL;

    /*
     * Look for the special members (first three).
     */
    if (arhdr->ar_name[0] == '/' && arhdr->ar_name[1] == ' ')
    {
        arhdr_linker1 = arhdr;
        arhdr = next_archive_member(arhdr);
    }

    if (arhdr->ar_name[0] == '/' && arhdr->ar_name[1] == ' ')
    {
        arhdr_linker2 = arhdr;
        arhdr = next_archive_member(arhdr);
    }

    if (arhdr->ar_name[0] == '/' && arhdr->ar_name[1] == '/')
    {
        arhdr_longnames = arhdr;
        arhdr = next_archive_member(arhdr);
    }

    if (!arhdr_linker1)
        apperror(RCFATAL(ERROR_INVALID_ARCHIVE), lib_file->name);

    /*
     * Walk through the archive members.
     */
    while (!end_of_file(lib_file, arhdr))
    {
        char *member_name;
        char namebuf[17];
        long member_size;

        if (arhdr->ar_name[0] == '/')
        {
            long offset;

            if (!arhdr_longnames)  /* Suddenly we need this one! */
                apperror(RCFATAL(ERROR_NO_LONGNAMES_MEMBER), lib_file->name);

            sscanf(arhdr->ar_name, "/%15ld", &offset);
            member_name = (char *)(arhdr_longnames+1) + offset;
        }
        else
        {
            sscanf(arhdr->ar_name, "%16[^/]", namebuf);
            member_name = namebuf;
        }

        sscanf(arhdr->ar_size, "%10ld", &member_size);

        if (!callback(lib_file, member_name, member_size, arhdr+1, arg))
            break;

        arhdr = next_archive_member(arhdr);
    }

    close_archive_map(lib_file);
}

/****************************************************************************
 *                                                                          *
 * Function: next_archive_member                                            *
 *                                                                          *
 * Purpose : Walk past the current archive member.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

static COFF_ARHDR *next_archive_member(const COFF_ARHDR *arhdr)
{
    long size;

    sscanf(arhdr->ar_size, "%10ld", &size);
    if (size & 1) size++;

    return (COFF_ARHDR *)((char *)arhdr + COFF_ARHSZ + size);
}

/****************************************************************************
 *                                                                          *
 * Function: open_archive_map                                               *
 *                                                                          *
 * Purpose : Create a file-mapping of a archive file.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

static COFF_ARHDR *open_archive_map(FILEINFO *lib_file)
{
    WINERR err;

    err = my_openmap(lib_file);
    if (err) apperror(MYOPENERROR(RCFATAL(err)), lib_file->name);

    if (memcmp(lib_file->base, COFF_ARMAG, COFF_ARMSZ) != 0)
        apperror(RCFATAL(ERROR_INVALID_ARCHIVE), lib_file->name);

    return (COFF_ARHDR *)((char *)lib_file->base + COFF_ARMSZ);
}

/****************************************************************************
 *                                                                          *
 * Function: close_archive_map                                              *
 *                                                                          *
 * Purpose : Close a file-mapping of a archive file.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void close_archive_map(FILEINFO *lib_file)
{
    WINERR err;

    err = my_closemap(lib_file, FALSE);
    if (err) apperror(RCFATAL(err));
}

