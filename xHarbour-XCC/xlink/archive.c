/****************************************************************************
 *                                                                          *
 * File    : archive.c                                                      *
 *                                                                          *
 * Purpose : Win32 Linker; archive file management.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "link.h"

typedef struct _DIRENTRY
{
    char *symbol_name;      /* symbol name */
    long member_offset;     /* archive member offset */
} DIRENTRY;

#define MAX_IMPORT_OBJECT_SIZE  0x0020000   /* 128 KB */

void process_exports_and_import(void);

void process_archive(FILEINFO *, bool_t *);

/* Static function prototypes */
static int __cdecl comp_symbol_entries(const void *, const void *);
static void process_archive_member(const char *, FILEINFO *, const void *);
static void process_delay_import_archive_member(const char *, FILEINFO *, const void *);
static bool_t extract_import_symbol(char *, bool_t *, const void *);
static COFF_ARHDR *next_archive_member(const COFF_ARHDR *);
static COFF_ARHDR *open_archive_map(FILEINFO *);
static void close_archive_map(FILEINFO *);

/****************************************************************************
 *                                                                          *
 * Function: process_archives                                               *
 *                                                                          *
 * Purpose : Look for unresolved symbols in all archives.                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           99-08-21  Break out of loop, if unresolved count hits zero.    *
 *                                                                          *
 ****************************************************************************/

void process_archives(void)
{
    while (unresolved_count != 0)
    {
        bool_t found_symbol = FALSE;
        FILEINFO *lib_file;

        for (lib_file = lib_file_list;
             lib_file != NULL;
             lib_file = lib_file->next)
        {
            process_archive(lib_file, &found_symbol);
            if (unresolved_count == 0) break;
        }

        if (unresolved_count != 0 && !found_symbol)
        {
            bool_t weak_symbols = FALSE;
            SYMENTRY *pub;

            /*
             * The unresolved symbols that we now have can either be
             * 'unresolved', which is an error, or 'unresolved weak',
             * which means that we should try to find them using
             * there alternate name.
             */
            for (pub = public_list; pub != NULL; pub = pub->next)
            {
                if (pub->type == unresolved_weak)
                {
                    rename_weak_symbol(pub);
                    weak_symbols = TRUE;
                }
            }

            /*
             * If we found any 'unresolved weak' symbols, do another
             * sweep through the archives. Otherwise report the errors.
             */
            if (!weak_symbols)
            {
                // Ron pinkas
                {
                    for (pub = public_list; pub != NULL; pub = pub->next)
                    {
                        if (pub->type == unresolved)
                        {
                           process_exports_and_import();
                           break;
                        }
                    }
                }

                for (pub = public_list; pub != NULL; pub = pub->next)
                {
                    char msg[512];

                    if (pub->type == unresolved)
                    {
                        sprintf( msg, "%s referenced from %s(%s)",
                                      pub->name,
                                      ( pub->dec_mod->lib_file ? basename( pub->dec_mod->lib_file->name ) : "" ),
                                      pub->dec_mod->obj_file->name );

                        apperror(RCERROR(ERROR_UNRESOLVED_SYMBOL), msg);
                    }
                }
                break;
            }
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: process_archive                                                *
 *                                                                          *
 * Purpose : Look for unresolved symbols in the given archive.              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           98-11-08  Support for new import archives added.               *
 *           99-02-14  Support for delayed imports added.                   *
 *           99-07-03  Verbose messages added.                              *
 *           03-07-30  Call to unmangle_name() added.                       *
 *           03-08-27  Bugfix: Unmangling can make the symbols "unsorted".  *
 *                                                                          *
 ****************************************************************************/

void process_archive(FILEINFO *lib_file, bool_t *found_symbol)
{
    COFF_ARHDR *arhdr;
    COFF_ARHDR *arhdr_linker1;
    COFF_ARHDR *arhdr_linker2;
    COFF_ARHDR *arhdr_longnames;
    long member_count;
    long symbol_count;
    long *member_offsets;
    ushort_t *member_indexes;
    DIRENTRY *symbol_entries;
    bool_t delay_check;
    long i;
    void *ip;
    SYMENTRY *pub;

    if (options.verbose)
        printmsg(MSG_PROCESSING_ARCHIVE, lib_file->name);

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

    if (!arhdr_linker1 || !arhdr_linker2)  /* long names not required */
        apperror(RCFATAL(ERROR_INVALID_ARCHIVE), lib_file->name);

    /*
     * Process linker member 2.
     */
    ip = (void *)(arhdr_linker2+1);

    member_count = *(long *)ip;
    ip = ((long *)ip + 1);

    member_offsets = (long *)ip;
    ip = ((long *)ip + member_count);

    symbol_count = *(long *)ip;
    ip = ((long *)ip + 1);

    member_indexes = (ushort_t *)ip;
    ip = ((ushort_t *)ip + symbol_count);

    /*
     * Allocate a temporary array for symbols.
     */
    symbol_entries = (DIRENTRY *)my_alloc(symbol_count * sizeof(DIRENTRY));
    if (options.unmangle_names)
    {
        /*
         * Fill the temporary array with unmangled symbols.
         */
        for (i = 0; i < symbol_count; i++)
        {
            symbol_entries[i].symbol_name = tstrcpy(unmangle_name(ip));
            symbol_entries[i].member_offset = member_offsets[member_indexes[i]-1];
            ip = (char *)ip + strlen(ip)+1;
        }

        /* Unmangling might make the array "unsorted" - sort it! */
        qsort(symbol_entries, symbol_count, sizeof(DIRENTRY), comp_symbol_entries);
    }
    else
    {
        /*
         * Fill the temporary array with symbols.
         */
        for (i = 0; i < symbol_count; i++)
        {
            symbol_entries[i].symbol_name = ip;
            symbol_entries[i].member_offset = member_offsets[member_indexes[i]-1];
            ip = (char *)ip + strlen(ip)+1;
        }
    }

    /*
     * Walk through all (unresolved) symbols.  Since we append new
     * symbols at the end of the list, this will always work...
     */
    delay_check = TRUE;
    for (pub = public_list; pub != NULL; pub = pub->next)
    {
        int lo = 0;
        int hi = symbol_count-1;

        if (pub->type != unresolved &&
            pub->type != unresolved_weak)
            continue;

        if (pub->flags.nosearch)
            continue;

        while (hi >= lo)  /* binary search */
        {
            int this = (lo + hi) / 2;
            int cond = strcmp(pub->name, symbol_entries[this].symbol_name);

            if (cond < 0)
                hi = this-1;
            else if (cond > 0)
                lo = this+1;
            else
            {
                char *filename;
                char name[17];

                *found_symbol = TRUE;

                if (options.verbose)
                    printmsg(MSG_ARCHIVE_SYMBOL_FOUND, pub->name);

                arhdr = (COFF_ARHDR *)((char *)lib_file->base + symbol_entries[this].member_offset);

                if (arhdr->ar_name[0] == '/')  /* loooong name */
                {
                    long offset;

                    if (!arhdr_longnames)
                        apperror(RCFATAL(ERROR_NO_LONGNAMES_MEMBER), lib_file->name);

                    sscanf(arhdr->ar_name, "/%15ld", &offset);
                    filename = (char *)(arhdr_longnames+1) + offset;
                }
                else
                {
                    sscanf(arhdr->ar_name, "%16[^/]", name);
                    filename = name;
                }

                if (delay_check && !lib_file->delay_dll)
                {
                    lib_file->delay_dll = lookup_delay_import_module(filename, FALSE);
                    delay_check = FALSE;  /* try once, ignore many */
                }

                if (!lib_file->delay_dll)
                {
                    /* Process a normal/import archive member */
                    process_archive_member(filename, lib_file, arhdr+1);
                }
                else
                {
                    /* Process a delay loaded import archive member */
                    process_delay_import_archive_member(filename, lib_file, arhdr+1);
                }

                if (pub->type == unresolved)  /* accept unresolved_weak */
                {
                    apperror(RCFATAL(ERROR_BAD_ARCHIVE_DIRECTORY),
                        lib_file->name, pub->name, filename);
                }
                break;
            }
        }
    }

    /* Free the binary search array */
    if (options.unmangle_names)
    {
        for (i = 0; i < symbol_count; i++)
            my_free(symbol_entries[i].symbol_name);
    }
    my_free(symbol_entries);

    close_archive_map(lib_file);
}

/****************************************************************************
 *                                                                          *
 * Function: comp_symbol_entries                                            *
 *                                                                          *
 * Purpose : Compare two symbol entries for the qsort() function.           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           03-08-27  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int __cdecl comp_symbol_entries(const void *item1, const void *item2)
{
    DIRENTRY entry1 = *(DIRENTRY *)item1;
    DIRENTRY entry2 = *(DIRENTRY *)item2;

    return strcmp(entry1.symbol_name, entry2.symbol_name);
}

/****************************************************************************
 *                                                                          *
 * Function: process_archive_member                                         *
 *                                                                          *
 * Purpose : Process one archive member for a normal/import archive.        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           99-02-14  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void process_archive_member(const char *filename, FILEINFO *lib_file, const void *base)
{
    COFF_NEWIMP *ni;

    ni = (COFF_NEWIMP *)base;
    if (ni->ni_sig1 == COFF_F_MAG_UNKNOWN &&
        ni->ni_sig2 == COFF_I_MAG_SIG2)
    {
        char *expname = (char *)(ni+1);
        char *modname = expname + strlen(expname)+1;
        void *ip;

        /*
         * This is a new style import archive member which only
         * contains a definition, not a real object module.
         * We must build a virtual object module to pass on.
         */
        ip = my_alloc(MAX_IMPORT_OBJECT_SIZE);
        build_new_import_object(ip, ni, expname, modname);
        process_object_module(filename, lib_file, ip);
        my_free(ip);
    }
    else
    {
        /* Process module straight from the archive */
        process_object_module(filename, lib_file, base);
    }
}

/****************************************************************************
 *                                                                          *
 * Function: process_delay_import_archive_member                            *
 *                                                                          *
 * Purpose : Process one archive member for a delay loaded import archive.  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           99-02-14  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void process_delay_import_archive_member(const char *filename, FILEINFO *lib_file, const void *base)
{
    void *ip = my_alloc(MAX_IMPORT_OBJECT_SIZE);
    COFF_NEWIMP *ni;

    ni = (COFF_NEWIMP *)base;
    if (ni->ni_sig1 == COFF_F_MAG_UNKNOWN &&
        ni->ni_sig2 == COFF_I_MAG_SIG2)
    {
        char *expname = (char *)(ni+1);  /* _symbol@nn */
        char *modname = expname + strlen(expname)+1;  /* module.DLL */
        char symname[MAX_PATH];

        /*
         * This is a new style import archive member. Since we can only
         * delay-load functions, make sure this isn't a data symbol.
         */
        if (COFF_I_NEWTYPE(ni->ni_flags) != COFF_I_CODE)
            apperror(RCFATAL(ERROR_CANT_DELAY_DATA), expname, filename);

        /*
         * Step 1: Build a linker-generated virtual object module to
         *         pass on instead of the original archive member.
         */
        symbol_from_export_name(symname, expname, ni->ni_flags);
        build_delay_import_object(ip, modname, expname, symname);
        process_object_module(filename, lib_file, ip);
    }
    else
    {
        char expname[MAX_PATH];
        char symname[MAX_PATH];
        bool_t is_func;

        /*
         * This is a old style import archive member. Extract the name
         * of the symbol from the object module. Since we can only
         * delay-load functions, make sure this isn't a data symbol.
         */
        if (!extract_import_symbol(expname, &is_func, base))
            apperror(RCFATAL(ERROR_INVALID_OBJECT), filename);

        if (!is_func)
            apperror(RCFATAL(ERROR_CANT_DELAY_DATA), expname, filename);

        /*
         * Step 1: Build a linker-generated virtual object module to
         *         pass on instead of the original archive member.
         */
        symbol_from_export_name(symname, expname, COFF_I_NAME_UNDEC);
        build_delay_import_object(ip, filename, expname, symname);
        process_object_module(filename, lib_file, ip);
    }

    if (!lib_file->delay_dll->flags.referenced)
    {
        static bool_t emitted_null_thunk = FALSE;

        /*
         * Step 2: Since this is the first symbol imported from this
         *         module we need to pull in the per-module extra stuff.
         *         This includes, among other things, an entry for the
         *         Delay Import Directory.
         */
        build_delay_import_module_object(ip, filename);
        process_object_module(filename, lib_file, ip);

        if (!emitted_null_thunk)
        {
            /*
             * Step 3: We need to include exactly one 'null thunk data'
             *         for the entire executable file, to terminate
             *         the Delay Import Directory. This is normally
             *         done by referencing a symbol in the archive,
             *         but since we are building the delayed import
             *         objects on-the-fly, the archive directory does
             *         not contain the correct symbol names.
             */
            build_delay_import_null_thunk_object(ip);
            process_object_module(filename, lib_file, ip);
            emitted_null_thunk = TRUE;
        }

        lib_file->delay_dll->flags.referenced = TRUE;
    }

    my_free(ip);
}

/****************************************************************************
 *                                                                          *
 * Function: extract_import_symbol                                          *
 *                                                                          *
 * Purpose : Extract the name and type of a imported symbol, from an old    *
 *           style import archive object module.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           99-02-14  Created                                              *
 *                                                                          *
 ****************************************************************************/

static bool_t extract_import_symbol(char *expname, bool_t *is_func, const void *base)
{
    COFF_FILHDR *fhdr;
    COFF_SCNHDR *scnhdr;
    COFF_SYMENT *symptr;
    char *strptr;
    long i;

    fhdr = (COFF_FILHDR *)base;

    scnhdr = (COFF_SCNHDR *)((char *)base + COFF_FILHSZ + fhdr->f_opthdr);
    symptr = (COFF_SYMENT *)((char *)base + fhdr->f_symptr);
    strptr = (char *)(symptr + fhdr->f_nsyms) + sizeof(long);

    for (i = 0; i < fhdr->f_nsyms; i++)
    {
        COFF_SYMENT *se = &symptr[i];

        if (se->n_sclass == COFF_C_EXT &&
            se->n_scnum >= 1 &&
            se->n_scnum <= fhdr->f_nscns)
        {
            COFF_SCNHDR *sh = &scnhdr[se->n_scnum-1];

            /* Indicate if this is a code symbol */
            *is_func = (sh->s_flags & COFF_STYP_TEXT) != 0;

            if (se->n_zeroes != 0)
            {
                for (i = 0; i < 8 && se->n_name[i] != '\0'; i++)
                    expname[i] = se->n_name[i];
                expname[i] = '\0';
            }
            else
            {
                strcpy(expname, &strptr[se->n_offset-4]);
            }

            /* Remove the import prefix from the name, if present */
            if (strncmp(expname, "__imp_", 6) == 0)
                memmove(expname, expname+6, strlen(expname)+1-6);

            return TRUE;
        }
    }

    return FALSE;
}

/****************************************************************************
 *                                                                          *
 * Function: next_archive_member                                            *
 *                                                                          *
 * Purpose : Walk past the current archive member.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
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
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static COFF_ARHDR *open_archive_map(FILEINFO *lib_file)
{
    WINERR err;

    /*
       Ron Pinkas Oct-9-2008
       We might already have a base here, because process_archive()
       might have been called from write_and_process_import_archive()
       which has the map opened!
    */
    if( lib_file->base == NULL )
    {
       err = my_openmap(lib_file);
       if (err) apperror(MYOPENERROR(RCFATAL(err)), lib_file->name);
    }

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
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void close_archive_map(FILEINFO *lib_file)
{
    WINERR err;

    err = my_closemap(lib_file, FALSE);
    if (err) apperror(RCFATAL(err));
}

