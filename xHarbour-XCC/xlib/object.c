/****************************************************************************
 *                                                                          *
 * File    : object.c                                                       *
 *                                                                          *
 * Purpose : Win32 Library Manager; object file management.                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *           98-03-22  Changed to dynamic allocation of member array.       *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#pragma warning(push)
#pragma warning(disable: 4115 4201)
#include <imagehlp.h>
#pragma warning(pop)

#include "lib.h"

#define MAX_ARCHIVE_SIZE  0x4000000     /* 64 MB (99-05-28) */

#define MEMBER_ARRAY_INCREMENT  1024
#define LONGNAMES_BUFFER_INCREMENT  4096

static char *strptr = NULL;
static void *ip;

static long *member_offsets = NULL;
static size_t member_count = 0;
static size_t member_maxcount = 0;

static char *longnames_ptr = NULL;
static size_t longnames_maxlen = 0;
static size_t longnames_len = 0;

__inline char *align_fileptr(void *tp, size_t size)
{
    if (size & 1)
    {
        *(char *)tp = '\n';
        tp = ((char *)tp + 1);
    }
    return tp;
}

__inline size_t align_size(size_t size)
{
    return (size & 1) ? ++size : size;
}

/* Static function prototypes */
static int __stdcall process_archive_member(FILEINFO *, const char *, size_t, const void *, void *);
static void process_symbol(const char *, long, const char *);
static const char *symbol_name(COFF_SYMENT *);
static void *open_object_map(FILEINFO *);
static void close_object_map(FILEINFO *);
static size_t output_archive_member_header(COFF_ARHDR *, const char *, size_t, uint_t, time_t);
static char *output_archive_member(void *, const char *, size_t, const char *, time_t);
static void output_special_archive_members(void);
static size_t sizeof_first_linker_member(void);
static char *output_first_linker_member(void *, size_t);
static size_t sizeof_second_linker_member(void);
static char *output_second_linker_member(void *, size_t);
static size_t sizeof_longnames_member(void);
static char *output_longnames_member(void *, size_t);
static void create_output_map(void);
static void close_output_map(void);

/****************************************************************************
 *                                                                          *
 * Function: create_archive_file                                            *
 *                                                                          *
 * Purpose : Create a new archive file.                                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *           98-01-18  Added call to process_executable_image().            *
 *           99-05-29  Exception handler added (just blew the 16MB limit).  *
 *           04-01-12  Support added for command line exports.              *
 *                                                                          *
 ****************************************************************************/

void create_archive_file(void)
{
    FILEINFO *file;
    NAMENTRY *nam;
    WINERR err;

    longnames_maxlen = longnames_len = 0;
    member_count = 0;
    err = 0;

    create_output_map();

    __try
    {
        /*
         * Walk through all object/archive/executable files.
         */
        for (file = obj_list; file != NULL; file = file->next)
        {
            char *s;

            /*
             * Assume we have decent extensions, to avoid a
             * costly file identification process.
             */
            s = strrchr(file->name, '.');
            if (s != NULL && _stricmp(s, EXT_LIB) == 0)
            {
                enum_archive_members(file, process_archive_member, NULL);
            }
            else if (s != NULL && _stricmp(s, EXT_DLL) == 0 ||
                     s != NULL && _stricmp(s, EXT_EXE) == 0)
            {
                process_executable_image(file);
            }
            else
            {
                open_object_map(file);
                process_object_module(file->name, file->size, file->base, file->time);
                close_object_map(file);
            }
        }

        /*
         * Process command line exports.
         */
        if (export_list != NULL)
            process_export_list();

        /*
         * Output Linker member 1, 2 and Longnames member.
         */
        output_special_archive_members();
    }
    __except (my_exception_filter(GetExceptionCode(), &err))
    {
        apperror(RCFATAL(err), "create_archive_file");
    }

    close_output_map();

    my_free(longnames_ptr);
    longnames_ptr = NULL;

    /* If we have entries left, they were not found... */
    for (nam = remove_list; nam != NULL; nam = nam->next)
        apperror(RCWARNING(ERROR_CANT_FIND_MEMBER), nam->name);
}

/****************************************************************************
 *                                                                          *
 * Function: process_archive_member                                         *
 *                                                                          *
 * Purpose : Process a single archive member.                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int __stdcall process_archive_member(FILEINFO *lib_file,
    const char *member_name, size_t member_size,
    const void *member_ptr, void *arg)
{
    UNREFERENCED_PARAMETER(lib_file);
    UNREFERENCED_PARAMETER(arg);

    process_object_module(member_name, member_size, member_ptr, time_stamp);
    return 1;
}

/****************************************************************************
 *                                                                          *
 * Function: process_object_module                                          *
 *                                                                          *
 * Purpose : Process a single object file.                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *           98-03-22  Changed to dynamic allocation of member array.       *
 *           98-11-07  Support for new import archives added.               *
 *           98-11-11  Added some (unimportant) machine types.              *
 *           99-03-27  Use same prefix for code and data: "__imp_".         *
 *           03-10-19  Added hack for oldnames.lib.                         *
 *           00-11-16  Support for ARM machine added (new machine logic).   *
 *           04-01-12  Bugfix: only add __imp_name for data symbols.        *
 *                                                                          *
 ****************************************************************************/

void process_object_module(const char *name, size_t size, const void *base, time_t time)
{
    COFF_FILHDR *fhdr;
    COFF_NEWIMP *ni;
    COFF_SYMENT *symptr = NULL;
    NAMENTRY *nam;
    long i;
    long offset;

    /*
     * Check if we should remove this member.
     */
    for (nam = remove_list; nam != NULL; nam = nam->next)
    {
        if (_stricmp(nam->name, name) == 0)
        {
            remove_name_from_list(&remove_list, nam);
            return;
        }
    }

    if (options.verbose)
        printf("%s\n", name);

    /*
     * Copy to output file (might be garbage, we decide later).
     */
    if (member_count == member_maxcount)
    {
        member_maxcount += MEMBER_ARRAY_INCREMENT;
        member_offsets = (long *)my_realloc(member_offsets, member_maxcount * sizeof(long));
    }

    member_offsets[member_count++] = offset = file_offset(tmp_file, ip);
    ip = output_archive_member(ip, name, size, base, time);

    /*
     * Check for new style import object.
     */
    ni = (COFF_NEWIMP *)base;
    if (ni->ni_sig1 == COFF_F_MAG_UNKNOWN &&
        ni->ni_sig2 == COFF_I_MAG_SIG2)
    {
        char *impname = (char *)(ni+1);
        char namebuf[256];

        if (options.old_implib)
            apperror(RCFATAL(ERROR_CANT_CONVERT));

        if ((ni->ni_flags & COFF_I_DATA) == 0)
        {
            /* Add <impname> to the archive */
            process_symbol(impname, offset, name);
        }

        /* Add __imp_<impname> to the archive */
        sprintf(namebuf, "__imp_%s", impname);
        process_symbol(namebuf, offset, name);
        return;
    }

    /*
     * Process the object header. Make sure it's valid.
     */
    fhdr = (COFF_FILHDR *)base;
    switch (fhdr->f_magic)
    {
        case COFF_F_MAG_I386:
            if (options.machine == MACHINE_UNKNOWN)
                options.machine = MACHINE_X86;
            else if (options.machine != MACHINE_X86)
                apperror(RCWARNING(ERROR_INVALID_MACHINE), name);
            break;

        case COFF_F_MAG_ARM:
            if (options.machine == MACHINE_UNKNOWN)
                options.machine = MACHINE_ARM;
            else if (options.machine != MACHINE_ARM)
                apperror(RCWARNING(ERROR_INVALID_MACHINE), name);
            break;

        case COFF_F_MAG_UNKNOWN:  /* any machine */
            break;

        default:  /* all other machines */
            apperror(RCFATAL(ERROR_INVALID_MACHINE), name);
            break;
    }

    /*
     * Initialize the image pointers.
     */
    symptr = (COFF_SYMENT *)((char *)base + fhdr->f_symptr);
    strptr = (char *)(symptr + fhdr->f_nsyms) + sizeof(long);

    for (i = 0; i < fhdr->f_nsyms; i++)
    {
        COFF_SYMENT *se = &symptr[i];

        if (se->n_sclass == COFF_C_EXT || se->n_sclass == COFF_C_WEAKEXT)
        {
            if (se->n_scnum != COFF_N_UNDEF || se->n_value != 0)
            {
                /* Add <symname> to the archive */
                process_symbol(symbol_name(se), offset, name);
            }
            else if (options.old_names && se->n_sclass == COFF_C_WEAKEXT)
            {
                /* Add <symname> to the archive (oldnames.lib hack) */
                process_symbol(symbol_name(se), offset, name);
            }
        }

        /* Skip auxiliary symbol entries */
        i += se->n_numaux;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: process_symbol                                                 *
 *                                                                          *
 * Purpose : Add a symbol to the symbol table; warn if duplicates.          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void process_symbol(const char *symbol_name, long offset, const char *name)
{
    SYMENTRY *sym;

    sym = lookup_symbol(symbol_name);

    if (!sym->defined)
    {
        sym->defined = TRUE;
        sym->offset = offset;
    }
    else if (sym->name[0] != '?')
    {
        COFF_ARHDR *arhdr;
        char *member_name;
        char namebuf[17];

        arhdr = (COFF_ARHDR *)((char *)tmp_file->base + sym->offset);

        if (arhdr->ar_name[0] == '/')
        {
            long offset;

            sscanf(arhdr->ar_name, "/%15ld", &offset);
            member_name = longnames_ptr + offset;
        }
        else
        {
            sscanf(arhdr->ar_name, "%16[^/]", namebuf);
            member_name = namebuf;
        }

        apperror(RCWARNING(ERROR_ALREADY_DEFINED),
            sym->name, basename(member_name), basename(name));
    }
}

/****************************************************************************
 *                                                                          *
 * Function: symbol_name                                                    *
 *                                                                          *
 * Purpose : Return the name of a symbol.                                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

static const char *symbol_name(COFF_SYMENT *se)
{
    static char name[9];
    int i;

    if (se->n_zeroes)
    {
        for (i = 0; i < 8 && se->n_name[i] != '\0'; i++)
            name[i] = se->n_name[i];

        name[i] = '\0';
        return name;
    }
    else
    {
        return &strptr[se->n_offset-4];
    }
}

/****************************************************************************
 *                                                                          *
 * Function: import_archive_member_name                                     *
 *                                                                          *
 * Purpose : Return name of exported symbol in import library (hack-ish).   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *           98-11-07  Support for new import archives added.               *
 *                                                                          *
 ****************************************************************************/

const char *import_archive_member_name(const char *base)
{
    COFF_FILHDR *fhdr;
    COFF_NEWIMP *ni;
    COFF_SYMENT *symptr;
    long i;

    ni = (COFF_NEWIMP *)base;

    if (ni->ni_sig1 == COFF_F_MAG_UNKNOWN &&
        ni->ni_sig2 == COFF_I_MAG_SIG2)
    {
        /* Return new style import name */
        return (char *)(ni+1);
    }

    fhdr = (COFF_FILHDR *)base;

    symptr = (COFF_SYMENT *)(base + fhdr->f_symptr);
    strptr = (char *)(symptr + fhdr->f_nsyms) + sizeof(long);

    for (i = 0; i < fhdr->f_nsyms; i++)
    {
        COFF_SYMENT *se = &symptr[i];

        if (se->n_sclass == COFF_C_EXT && se->n_scnum > 0)
        {
            return symbol_name(se);
        }

        /* Skip auxiliary symbol entries */
        i += se->n_numaux;
    }

    return "";
}

/****************************************************************************
 *                                                                          *
 * Function: open_object_map                                                *
 *                                                                          *
 * Purpose : Create a file-mapping of a object file.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void *open_object_map(FILEINFO *obj_file)
{
    WINERR err;

    err = my_openmap(obj_file);
    if (err) apperror(MYOPENERROR(RCFATAL(err)), obj_file->name);

    return obj_file->base;
}

/****************************************************************************
 *                                                                          *
 * Function: close_object_map                                               *
 *                                                                          *
 * Purpose : Close a file-mapping of a object file.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void close_object_map(FILEINFO *obj_file)
{
    WINERR err;

    err = my_closemap(obj_file, FALSE);
    if (err) apperror(RCFATAL(err));
}

/****************************************************************************
 *                                                                          *
 * Function: output_archive_member_header                                   *
 *                                                                          *
 * Purpose : Write a archive member header to the output file.              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

static size_t output_archive_member_header(COFF_ARHDR *arhdr,
    const char *name, size_t size, uint_t mode, time_t time)
{
    char timebuf[16];
    char sizebuf[16];
    char modebuf[16];

    sprintf(timebuf, "%lu", time);
    sprintf(sizebuf, "%lu", size);
    sprintf(modebuf, "%lu", mode);

    memset(arhdr, ' ', COFF_ARHSZ);
    memcpy(arhdr->ar_name, name, strlen(name));
    memcpy(arhdr->ar_date, timebuf, strlen(timebuf));
    memcpy(arhdr->ar_mode, modebuf, strlen(modebuf));
    memcpy(arhdr->ar_size, sizebuf, strlen(sizebuf));
    memcpy(arhdr->ar_fmag, COFF_ARFMAG, strlen(COFF_ARFMAG));

    return COFF_ARHSZ;
}

/****************************************************************************
 *                                                                          *
 * Function: output_archive_member                                          *
 *                                                                          *
 * Purpose : Write a archive member to the output file.                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *           97-12-16  Short name should be < 16 rather than <= 16 to       *
 *                     make the Microsoft Lib Utility happy.                *
 *                                                                          *
 ****************************************************************************/

static char *output_archive_member(void *tp,
    const char *name, size_t size, const char *base, time_t time)
{
    char namebuf[17];

    if (strlen(name) < 16)
    {
        strcat(strcpy(namebuf, name), "/");
    }
    else
    {
        sprintf(namebuf, "/%lu", longnames_len);

        if ((longnames_len + strlen(name)+1) > longnames_maxlen)
        {
            longnames_maxlen += LONGNAMES_BUFFER_INCREMENT;
            longnames_ptr = my_realloc(longnames_ptr, longnames_maxlen);
        }

        strcpy(longnames_ptr + longnames_len, name);
        longnames_len += strlen(name)+1;
    }

    tp = (char *)tp + output_archive_member_header(
        (COFF_ARHDR *)tp, namebuf, size, 100666, time);

    memcpy(tp, base, size);
    tp = ((char *)tp + size);

    tp = align_fileptr(tp, size);

    return tp;
}

/****************************************************************************
 *                                                                          *
 * Function: output_special_archive_members                                 *
 *                                                                          *
 * Purpose : Write the special members to the output file.                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void output_special_archive_members(void)
{
    size_t sizeof_linker1;
    size_t sizeof_linker2;
    size_t sizeof_longnames;
    size_t delta;
    size_t i;
    void *tp;

    if (member_count == 0)
        apperror(RCFATAL(ERROR_NO_MEMBERS));

    /* Calculate actual size of the three special members */
    sizeof_linker1 = sizeof_first_linker_member();
    sizeof_linker2 = sizeof_second_linker_member();
    sizeof_longnames = sizeof_longnames_member();

    /* Calculate number of bytes to insert at beginning of file */
    delta = COFF_ARMSZ;
    delta += COFF_ARHSZ + align_size(sizeof_linker1);
    delta += COFF_ARHSZ + align_size(sizeof_linker2);
    delta += COFF_ARHSZ + align_size(sizeof_longnames);

    for (i = 0; i < member_count; i++)
        member_offsets[i] += delta;

    for (i = 0; i < symbol_count; i++)
        symbol_list[i]->offset += delta;

    tp = tmp_file->base;

    /* Make room to insert the three members in */
    memmove((char *)tp + delta, tp, ((char *)ip - (char *)tp));
    ip = (char *)ip + delta;

    /* Write the magic header */
    memcpy(tp, COFF_ARMAG, COFF_ARMSZ);
    tp = (char *)tp + COFF_ARMSZ;

    /* Write the special members */
    tp = output_first_linker_member(tp, sizeof_linker1);
    tp = output_second_linker_member(tp, sizeof_linker2);
    tp = output_longnames_member(tp, sizeof_longnames);
}

/****************************************************************************
 *                                                                          *
 * Function: sizeof_first_linker_member                                     *
 *                                                                          *
 * Purpose : Calculate the size of the first linker member.                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

static size_t sizeof_first_linker_member(void)
{
    size_t size;
    size_t i;

    size = sizeof(long) + symbol_count * sizeof(long);

    for (i = 0; i < symbol_count; i++)
        size += strlen(symbol_list[i]->name) + 1;  /* include '\0' */

    return size;
}

/****************************************************************************
 *                                                                          *
 * Function: output_first_linker_member                                     *
 *                                                                          *
 * Purpose : Write the first linker member to the output file.              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

static char *output_first_linker_member(void *tp, size_t size)
{
    size_t i;

    tp = (char *)tp + output_archive_member_header((COFF_ARHDR *)tp, "/", size, 0, time_stamp);

    write_big_endian(tp, symbol_count);
    tp = ((long *)tp + 1);

    for (i = 0; i < symbol_count; i++)
    {
        write_big_endian(tp, symbol_list[i]->offset);
        tp = ((long *)tp + 1);
    }

    for (i = 0; i < symbol_count; i++)
    {
        strcpy(tp, symbol_list[i]->name);
        tp = (char *)tp + strlen(symbol_list[i]->name) + 1;  /* include '\0' */
    }

    tp = align_fileptr(tp, size);

    return tp;
}

/****************************************************************************
 *                                                                          *
 * Function: sizeof_second_linker_member                                    *
 *                                                                          *
 * Purpose : Calculate size of the second linker member.                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

static size_t sizeof_second_linker_member(void)
{
    size_t size;
    size_t i;

    size = sizeof(long) + member_count * sizeof(long) +
           sizeof(long) + symbol_count * sizeof(short);

    for (i = 0; i < symbol_count; i++)
        size += strlen(symbol_list[i]->name) + 1;  /* include '\0' */

    return size;
}

/****************************************************************************
 *                                                                          *
 * Function: output_second_linker_member                                    *
 *                                                                          *
 * Purpose : Write the second linker member to the output file.             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

static char *output_second_linker_member(void *tp, size_t size)
{
    size_t i;

    tp = (char *)tp + output_archive_member_header((COFF_ARHDR *)tp, "/", size, 0, time_stamp);

    *(long *)tp = member_count;
    tp = ((long *)tp + 1);

    for (i = 0; i < member_count; i++)
    {
        *(long *)tp = member_offsets[i];
        tp = ((long *)tp + 1);
    }

    *(long *)tp = symbol_count;
    tp = ((long *)tp + 1);

    for (i = 0; i < symbol_count; i++)
    {
        size_t index;

        for (index = 0; index < member_count; index++)
            if (member_offsets[index] == symbol_list[i]->offset)
                break;

        *(short *)tp = (short)(1 + index);
        tp = ((short *)tp + 1);
    }

    for (i = 0; i < symbol_count; i++)
    {
        strcpy(tp, symbol_list[i]->name);
        tp = (char *)tp + strlen(symbol_list[i]->name) + 1;  /* include '\0' */
    }

    tp = align_fileptr(tp, size);

    return tp;
}

/****************************************************************************
 *                                                                          *
 * Function: sizeof_longnames_member                                        *
 *                                                                          *
 * Purpose : Calculate size of the longnames member.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

static size_t sizeof_longnames_member(void)
{
    return longnames_len;
}

/****************************************************************************
 *                                                                          *
 * Function: output_longnames_member                                        *
 *                                                                          *
 * Purpose : Write the longnames member to the output file.                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

static char *output_longnames_member(void *tp, size_t size)
{
    tp = (char *)tp + output_archive_member_header((COFF_ARHDR *)tp, "//", size, 0, time_stamp);

    memcpy(tp, longnames_ptr, longnames_len);
    tp = (char *)tp + longnames_len;

    tp = align_fileptr(tp, size);

    return tp;
}

/****************************************************************************
 *                                                                          *
 * Function: create_output_map                                              *
 *                                                                          *
 * Purpose : Create a read-write file mapping of the output file.           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void create_output_map(void)
{
    WINERR err;

    err = my_createmap(tmp_file, MAX_ARCHIVE_SIZE);
    if (err) apperror(RCFATAL(err), tmp_file->name);

    ip = tmp_file->base;
}

/****************************************************************************
 *                                                                          *
 * Function: close_output_map                                               *
 *                                                                          *
 * Purpose : Close a file-mapping of the output file.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void close_output_map(void)
{
    WINERR err;

    /* Set size, before we call my_closemap() */
    tmp_file->size = file_offset(tmp_file, ip);

    err = my_closemap(tmp_file, TRUE);
    if (err) apperror(RCFATAL(err));

    /* Delete the target file; it might not exist */
    err = my_deletefile(lib_file->name);
    if (err)
    {
        if (err != ERROR_FILE_NOT_FOUND)
        {
            my_deletefile(tmp_file->name);
            apperror(RCFATAL(err));
        }
    }

    /* Rename temporary file as the target file */
    err = my_renamefile(tmp_file->name, lib_file->name);
    if (err) apperror(RCFATAL(err));

    /* Set correct size for other functions */
    lib_file->size = tmp_file->size;
}

