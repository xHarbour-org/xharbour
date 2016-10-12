/****************************************************************************
 *                                                                          *
 * File    : export.c                                                       *
 *                                                                          *
 * Purpose : Win32 Linker; export management.                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           98-12-02  Support for new import archives added.               *
 *           99-02-14  Use same prefix for code and data: "__imp_".         *
 *           00-11-16  Support for ARM machine added.                       *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop
#include <limits.h>

#include "link.h"

#define MAX_EXPORT_OBJECT_SIZE   0x0100000      /*  1 MB */
#define MAX_IMPORT_ARCHIVE_SIZE  0x1000000      /* 16 MB */

#define MEMBER_ARRAY_INCREMENT  256

void process_archive(FILEINFO *, bool_t *);

/* Locals */
static char *import_descriptor_name = NULL;
static char *name_null_thunk_data = NULL;
static char *null_import_descriptor = "__NULL_IMPORT_DESCRIPTOR";
static char *member_name = NULL;
static char *strptr = NULL;

static long *member_offsets = NULL;
static size_t member_count = 0;
static size_t member_maxcount = 0;

static ushort_t ordinal_min = 0;
static ushort_t ordinal_max = 0;

__inline void *align_archive_ptr(void *ip)
{
    if ((size_t)ip & 1)
    {
        *(char *)ip = '\n';
        ip = ((char *)ip + 1);
    }
    return ip;
}

__inline size_t archive_member_size(size_t size)
{
    return (size) ? COFF_ARHSZ + ROUNDUP_WORD(size) : 0;
}

/* Static function prototypes */
static void build_special_symbols(void);
static void assign_ordinal_numbers(void);
static void write_symbol_name(COFF_SYMENT *, const char *, void **);
static const char *symbol_name(COFF_SYMENT *);
static void write_and_process_export_object(void);
static void *write_export_object_file(void *);
static void *create_export_object_map(void);
static void close_export_object_map(void *);
static void write_and_process_import_archive(void);
static void write_import_archive(void);
static void *write_import_archive_file(void *);
static void *process_archive_module(void *, void *, void *(__stdcall *)(void *, void *));
static void *write_import1_member(void *, void *);
static void *write_import2_member(void *, void *);
static void *write_import3_member(void *, void *);
static void *write_old_import_member(void *, void *);
static void *write_new_import_member(void *, void *);
static size_t write_archive_member_header(COFF_ARHDR *, const char *, size_t, uint_t, time_t);
static void *write_special_archive_members(void *);
static size_t sizeof_first_linker_member(void);
static void *write_first_linker_member(void *, size_t);
static size_t sizeof_second_linker_member(void);
static void *write_second_linker_member(void *, size_t);
static size_t sizeof_longnames_member(void);
static void *write_longnames_member(void *, size_t);
static void *create_import_archive_map(void);
static void close_import_archive_map(void *);

/****************************************************************************
 *                                                                          *
 * Function: process_exports                                                *
 *                                                                          *
 * Purpose : Create the export files (export object and import archive).    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           03-10-07  Added support for /NOEXPOBJ and /NOIMPLIB.           *
 *                                                                          *
 ****************************************************************************/

void process_exports(void)
{
    if (export_count != 0)
    {
        SCNENTRY *scn;

        /*
         * Need to create the export files?
         */
        scn = lookup_section(COFF_EDATA, COFF_EDATA_FLAGS);
        if (scn->segs == NULL && (!options.no_export_object || !options.no_import_archive))
        {
            build_special_symbols();
            assign_ordinal_numbers();

            if (!options.no_export_object)
                write_and_process_export_object();
            if (!options.no_import_archive)
                write_import_archive();
        }
    }
}

void process_exports_and_import(void)
{
    if (export_count != 0)
    {
        SCNENTRY *scn;

        /*
         * Need to create the export files?
         */
        scn = lookup_section(COFF_EDATA, COFF_EDATA_FLAGS);
        if (scn->segs == NULL)
        {
            build_special_symbols();
            assign_ordinal_numbers();

            write_and_process_export_object();
            write_and_process_import_archive();
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: build_special_symbols                                          *
 *                                                                          *
 * Purpose : Build the special export symbol names.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           99-02-14  Rewritten; simplified code.                          *
 *                                                                          *
 ****************************************************************************/

static void build_special_symbols(void)
{
    char name[MAX_PATH];
    char *dot;
    size_t len;

    /* Name of the exporting executable (DLL) */
    member_name = tstrcpy(basename(exe_file->name));

    dot = strchr(member_name, '.');
    len = (dot) ? dot-member_name : strlen(member_name);

    /* Symbol used to pull in the Import Directory Entry */
    sprintf(name, "__IMPORT_DESCRIPTOR_%.*s", len, member_name);
    import_descriptor_name = tstrcpy(name);

    /* Symbol used to terminate the Import Lookup Table */
    sprintf(name, "\177%.*s_NULL_THUNK_DATA", len, member_name);
    name_null_thunk_data = tstrcpy(name);
}

/****************************************************************************
 *                                                                          *
 * Function: assign_ordinal_numbers                                         *
 *                                                                          *
 * Purpose : Assign ordinal numbers to exports without it.                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void assign_ordinal_numbers(void)
{
    ushort_t ordinal;
    size_t i;
    size_t j;

    ordinal_min = USHRT_MAX;
    ordinal_max = 0;

    /*
     * Get ordinal range and check for duplicates.
     */
    for (i = 0; i < export_count; i++)
    {
        ordinal = export_list[i]->ordinal;
        if (ordinal != 0)
        {
            for (j = i+1; j < export_count; j++)
            {
                if (export_list[j]->ordinal == ordinal)
                    apperror(RCFATAL(ERROR_DUPLICATE_ORDINAL), ordinal);
            }

            ordinal_min = min(ordinal_min, ordinal);
            ordinal_max = max(ordinal_max, ordinal);
        }
    }

    /*
     * Assign ordinals to exports without it.
     */
    if (ordinal_max == 0)
    {
        /*
         * No user-defined ordinals; set optimal defaults.
         */
        for (i = 0; i < export_count; i++)
            export_list[i]->ordinal = (ushort_t)(1+i);

        ordinal_min = (ushort_t)1;
        ordinal_max = (ushort_t)export_count;
    }
    else
    {
        /*
         * Fill in gaps between user-defined ordinals.
         */
        ordinal = ordinal_min-1;
        for (i = 0; i < export_count; i++)
        {
            if (export_list[i]->ordinal == 0)
            {
                do
                {
                    /*
                     * Make sure we don't re-use a number.
                     */
                    ordinal++;

                    for (j = 0; j < export_count; j++)
                        if (export_list[j]->ordinal == ordinal) break;

                } while (j < export_count);

                export_list[i]->ordinal = ordinal;
            }
        }

        ordinal_max = max(ordinal_max, ordinal);
    }
}

/****************************************************************************
 *                                                                          *
 * Function: write_symbol_name                                              *
 *                                                                          *
 * Purpose : Write the name of a symbol table entry.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void write_symbol_name(COFF_SYMENT *se, const char *name, void **ip)
{
    if (strlen(name) <= 8)
    {
        strncpy(se->n_name, name, 8);
    }
    else
    {
        se->n_zeroes = 0;
        se->n_offset = ((char *)(*ip) - strptr);

        strcpy(*ip, name);
        (*ip) = (char *)(*ip) + strlen(*ip)+1;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: symbol_name                                                    *
 *                                                                          *
 * Purpose : Return the name of a symbol table entry.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static const char *symbol_name(COFF_SYMENT *se)
{
    static char name[9];
    int i;

    if (se->n_zeroes != 0)
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
 * Function: write_and_process_export_object                                *
 *                                                                          *
 * Purpose : Process the export object file.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void write_and_process_export_object(void)
{
    WINERR err = 0;
    void *ip;
    void *base;

    ip = base = create_export_object_map();

    __try
    {
        ip = write_export_object_file(ip);
    }
    __except (my_exception_filter(GetExceptionCode(), &err))
    {
        apperror(RCFATAL(err), "write_and_process_export_object");
    }

    /* Set the size of the file */
    exp_file->size = file_offset(exp_file, ip);

    /* Process the file, while it's still mapped */
    process_object_module(exp_file->name, NULL, base);

    close_export_object_map(ip);
}

/****************************************************************************
 *                                                                          *
 * Function: write_export_object_file                                       *
 *                                                                          *
 * Purpose : Write the export object file.                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void *write_export_object_file(void *ip)
{
    COFF_FILHDR *fhdr;
    COFF_SCNHDR *sh;
    COFF_EXPORT *export;
    COFF_RELOC *re;
    COFF_SYMENT *se;
    addr_t *address_table;
    long *nameptr_table;
    short *ordinal_table;
    long naddrs;
    void *base;
    size_t i;

    base = ip;

    naddrs = (ordinal_max - ordinal_min + 1);

    /*
     * Write the file header.
     */
    fhdr = (COFF_FILHDR *)ip;
    ip = ((COFF_FILHDR *)ip + 1);

    fhdr->f_magic = (options.machine == MACHINE_ARM) ? COFF_F_MAG_ARM : COFF_F_MAG_I386;
    fhdr->f_nscns = 1;
    fhdr->f_timdat = time_stamp;
    /* fhdr->f_symptr = 0; */
    fhdr->f_nsyms = (1 + export_count);
    fhdr->f_opthdr = 0;
    fhdr->f_flags = COFF_F_32BIT;

    /*
     * Reserve room for the sections header.
     */
    sh = (COFF_SCNHDR *)ip;
    ip = ((COFF_SCNHDR *)ip + 1);

    /*
     * Write the section header.
     */
    strncpy(sh->s_name, COFF_EDATA, 8);
    sh->s_vsize = 0;
    sh->s_vaddr = 0;
    /* sh->s_size = 0; */
    sh->s_scnptr = ((char *)ip - (char *)base);
    /* sh->s_relptr = 0; */
    sh->s_lnnoptr = 0;
    sh->s_nreloc = 4 + (ushort_t)export_count * 2;
    sh->s_nlnno = 0;
    sh->s_flags = COFF_RDATA_FLAGS;

    /*
     * Reserve room for the export directory.
     */
    export = (COFF_EXPORT *)ip;
    ip = ((COFF_EXPORT *)ip + 1);

    /*
     * Reserve room for the export tables.
     */
    address_table = (addr_t *)ip;
    ip = ((addr_t *)ip + naddrs);

    /* Must clear unused entries (if gap between ordinals) */
    memset(address_table, 0, naddrs * sizeof(addr_t));

    nameptr_table = (long *)ip;
    ip = ((long *)ip + export_count);

    ordinal_table = (short *)ip;
    ip = ((short *)ip + export_count);

    /*
     * Write the export directory.
     */
    export->e_flags = 0;
    export->e_timdat = time_stamp;
    export->e_majver = 0;
    export->e_minver = 0;
    export->e_modname = ((char *)ip - (char *)export);  /* name of the DLL */
    export->e_ordbase = ordinal_min;
    export->e_naddrs = naddrs;
    export->e_nnames = export_count;
    export->e_addrtab = ((char *)address_table - (char *)export);
    export->e_nametab = ((char *)nameptr_table - (char *)export);
    export->e_ordtab = ((char *)ordinal_table - (char *)export);

    /*
     * Write the export tables.
     */
    strcpy(ip, member_name);
    ip = (char *)ip + strlen(ip)+1;

    for (i = 0; i < export_count; i++)
    {
        EXPENTRY *exp = export_list[i];
        ushort_t idx = (exp->ordinal - ordinal_min);

        address_table[idx] = 0;
        nameptr_table[i] = ((char *)ip - (char *)export);
        ordinal_table[i] = idx;

        strcpy(ip, exp->name);
        ip = (char *)ip + strlen(ip)+1;
    }

    /*
     * Update the section header.
     */
    sh->s_size = ((char *)ip - (char *)export);
    ip = align_word_ptr(ip);
    sh->s_relptr = ((char *)ip - (char *)base);

    /*
     * Reserve room for the relocations.
     */
    re = (COFF_RELOC *)ip;
    ip = ((COFF_RELOC *)ip + sh->s_nreloc);

    /*
     * Write relocations for the export directory.
     */
    re->r_vaddr = offsetof(COFF_EXPORT, e_modname);
    re->r_symndx = 0;
    re->r_type = (options.machine == MACHINE_ARM) ? COFF_R_ARM_ADDR32NB : COFF_R_I386_DIR32NB;
    re++;

    re->r_vaddr = offsetof(COFF_EXPORT, e_addrtab);
    re->r_symndx = 0;
    re->r_type = (options.machine == MACHINE_ARM) ? COFF_R_ARM_ADDR32NB : COFF_R_I386_DIR32NB;
    re++;

    re->r_vaddr = offsetof(COFF_EXPORT, e_nametab);
    re->r_symndx = 0;
    re->r_type = (options.machine == MACHINE_ARM) ? COFF_R_ARM_ADDR32NB : COFF_R_I386_DIR32NB;
    re++;

    re->r_vaddr = offsetof(COFF_EXPORT, e_ordtab);
    re->r_symndx = 0;
    re->r_type = (options.machine == MACHINE_ARM) ? COFF_R_ARM_ADDR32NB : COFF_R_I386_DIR32NB;
    re++;

    /*
     * Write relocations for the export tables.
     */
    for (i = 0; i < export_count; i++)
    {
        ushort_t idx = (export_list[i]->ordinal - ordinal_min);

        re->r_vaddr = (char *)&address_table[idx] - (char *)export;
        re->r_symndx = (1+i);
        re->r_type = (options.machine == MACHINE_ARM) ? COFF_R_ARM_ADDR32NB : COFF_R_I386_DIR32NB;
        re++;

        re->r_vaddr = (char *)&nameptr_table[i] - (char *)export;
        re->r_symndx = 0;
        re->r_type = (options.machine == MACHINE_ARM) ? COFF_R_ARM_ADDR32NB : COFF_R_I386_DIR32NB;
        re++;
    }

    /*
     * Update the file header.
     */
    fhdr->f_symptr = ((char *)ip - (char *)base);

    /*
     * Reserve room for the symbol table.
     */
    se = (COFF_SYMENT *)ip;
    ip = ((COFF_SYMENT *)ip + fhdr->f_nsyms);

    /*
     * Initialize the string table.
     */
    strptr = ip;
    *(long *)strptr = 0;
    ip = ((long *)ip + 1);

    /*
     * Write section symbol (always first).
     */
    strncpy(se->n_name, COFF_EDATA, 8);
    se->n_value = 0;
    se->n_scnum = 1;
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_STAT;
    se->n_numaux = 0;
    se++;

    /*
     * Write function symbols.
     */
    for (i = 0; i < export_count; i++)
    {
        EXPENTRY *exp = export_list[i];

        write_symbol_name(se, exp->sym->name, &ip);
        se->n_value = 0;
        se->n_scnum = COFF_N_UNDEF;
        se->n_type = COFF_T_NULL;
        se->n_sclass = COFF_C_EXT;
        se->n_numaux = 0;
        se++;
    }

    /*
     * Update length of the string table.
     */
    *(long *)strptr = ((char *)ip - strptr);

    return ip;
}

/****************************************************************************
 *                                                                          *
 * Function: create_export_object_map                                       *
 *                                                                          *
 * Purpose : Create a read-write file mapping of the export object file.    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void *create_export_object_map(void)
{
    WINERR err;

    if (!exp_file)
    {
        char fname[MAX_PATH];

        strcpy(fname, exe_file->name);
        update_extension_in_file(fname, EXT_EXP);
        lookup_file(&exp_file, fname);
    }

    printmsg(MSG_CREATE_EXPORT_OBJECT, exp_file->name);

    err = my_createmap(exp_file, MAX_EXPORT_OBJECT_SIZE);
    if (err) apperror(RCFATAL(err), exp_file->name);

    return exp_file->base;
}

/****************************************************************************
 *                                                                          *
 * Function: close_export_object_map                                        *
 *                                                                          *
 * Purpose : Close a file-mapping of the export object file.                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void close_export_object_map(void *next_byte)
{
    WINERR err;

    /* Set size, before we call my_closemap() */
    exp_file->size = file_offset(exp_file, next_byte);

    err = my_closemap(exp_file, TRUE);
    if (err) apperror(RCFATAL(err));
}

static void write_and_process_import_archive(void)
{
    WINERR err = 0;
    void *ip;
    BOOL bDummy = FALSE;

    ip = create_import_archive_map();

    __try
    {
        ip = write_import_archive_file(ip);
    }
    __except (my_exception_filter(GetExceptionCode(), &err))
    {
        apperror(RCFATAL(err), "write_import_archive");
    }

    /* Process the file, while it's still mapped */
    process_archive(lib_file, &bDummy );

    close_import_archive_map(ip);
}

/****************************************************************************
 *                                                                          *
 * Function: write_import_archive                                           *
 *                                                                          *
 * Purpose : Write the import archive file; main function.                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void write_import_archive(void)
{
    WINERR err = 0;
    void *ip;

    ip = create_import_archive_map();

    __try
    {
        ip = write_import_archive_file(ip);
    }
    __except (my_exception_filter(GetExceptionCode(), &err))
    {
        apperror(RCFATAL(err), "write_import_archive");
    }

    close_import_archive_map(ip);
}

/****************************************************************************
 *                                                                          *
 * Function: write_import_archive_file                                      *
 *                                                                          *
 * Purpose : Write the import archive file.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           98-12-02  Support for new import archives added.               *
 *                                                                          *
 ****************************************************************************/

static void *write_import_archive_file(void *ip)
{
    size_t i;

    member_count = member_maxcount = 0;

    /* Write the special import archive members */
    ip = process_archive_module(ip, NULL, write_import1_member);
    ip = process_archive_module(ip, NULL, write_import2_member);
    ip = process_archive_module(ip, NULL, write_import3_member);

    /* Write the symbol import archive members */
    for (i = 0; i < export_count; i++)
    {
        if (export_list[i]->sym != NULL) ip = process_archive_module(
            ip, export_list[i], (options.old_implib) ?
            write_old_import_member : write_new_import_member);
    }

    /* Write Linker member 1, 2 and Longnames member */
    ip = write_special_archive_members(ip);

    my_free(member_offsets);
    member_offsets = NULL;

    return ip;
}

/****************************************************************************
 *                                                                          *
 * Function: process_archive_module                                         *
 *                                                                          *
 * Purpose : Write a single archive member.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           98-12-02  Support for new import archives added. New logic.    *
 *                                                                          *
 ****************************************************************************/

static void *process_archive_module(void *ip, void *vp, void *(__stdcall *write_member)(void *, void *))
{
    COFF_ARHDR *arhdr;
    COFF_FILHDR *fhdr;
    COFF_NEWIMP *ni;
    char name[256];  /* member or import name */
    long offset;

    if (member_count == member_maxcount)
    {
        member_maxcount += MEMBER_ARRAY_INCREMENT;
        member_offsets = (long *)my_realloc(member_offsets, member_maxcount * sizeof(long));
    }

    /* Remember the file offset to this archive member */
    member_offsets[member_count++] = offset = file_offset(lib_file, ip);

    /*
     * Write the archive member.
     */
    arhdr = (COFF_ARHDR *)ip;
    ip = ((COFF_ARHDR *)ip + 1);

    fhdr = (COFF_FILHDR *)ip;
    ip = write_member(ip, vp);

    if (strlen(member_name) < 16)
        strcat(strcpy(name, member_name), "/");
    else
        strcpy(name, "/0");

    write_archive_member_header(arhdr, name,
        (size_t)((char *)ip - (char *)fhdr), 0, time_stamp);

    /*
     * Check for new style import object.
     */
    ni = (COFF_NEWIMP *)fhdr;
    if (ni->ni_sig1 == COFF_F_MAG_UNKNOWN &&
        ni->ni_sig2 == COFF_I_MAG_SIG2)
    {
        char *impname = (char *)(ni+1);
        EXPENTRY *exp;

        exp = lookup_export(impname, NULL);
        exp->offset = offset;

        sprintf(name, "__imp_%s", impname);
        exp = lookup_export(name, NULL);
        exp->offset = offset;
    }
    else
    {
        COFF_SYMENT *symptr;
        long i;

        symptr = (COFF_SYMENT *)((char *)fhdr + fhdr->f_symptr);
        strptr = (char *)(symptr + fhdr->f_nsyms) + sizeof(long);

        for (i = 0; i < fhdr->f_nsyms; i++)
        {
            COFF_SYMENT *se = &symptr[i];

            if (se->n_sclass == COFF_C_EXT && se->n_scnum > 0)
            {
                EXPENTRY *exp;

                /* This will add "__imp_symbol" to sorted dictionary */
                exp = lookup_export(symbol_name(se), NULL);
                exp->offset = offset;
            }

            /* Skip auxiliary symbol entries */
            i += se->n_numaux;
        }
    }

    return align_archive_ptr(ip);
}

/****************************************************************************
 *                                                                          *
 * Function: write_import1_member                                           *
 *                                                                          *
 * Purpose : Write special import archive member #1.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void *write_import1_member(void *ip, void *vp)
{
    COFF_FILHDR *fhdr;
    COFF_OPTHDR *ohdr;
    COFF_SCNHDR *sh;
    COFF_RELOC *re;
    COFF_SYMENT *se;
    void *base;
    void *ip_raw;

    UNREFERENCED_PARAMETER(vp);

    base = ip;

    /*
     * Write the file header.
     */
    fhdr = (COFF_FILHDR *)ip;
    ip = ((COFF_FILHDR *)ip + 1);

    fhdr->f_magic = (options.machine == MACHINE_ARM) ? COFF_F_MAG_ARM : COFF_F_MAG_I386;
    fhdr->f_nscns = 2;
    fhdr->f_timdat = time_stamp;
    /* fhdr->f_symptr = 0; */
    fhdr->f_nsyms = 7;
    fhdr->f_opthdr = COFF_OPTHSZ;
    fhdr->f_flags = COFF_F_32BIT;

    /*
     * Write the "optional" header.
     */
    ohdr = (COFF_OPTHDR *)ip;
    ip = ((COFF_OPTHDR *)ip + 1);

    memset(ohdr, 0, sizeof(*ohdr));

    ohdr->o_magic = COFF_O_MAG_NT;
    ohdr->o_ld_majver = MAJOR_LINKER_VERSION;
    ohdr->o_ld_minver = MINOR_LINKER_VERSION;
    ohdr->o_salign = options.section_alignment;
    ohdr->o_falign = options.file_alignment;  /* FILE_ALIGNMENT_WIN32 */
    ohdr->o_os_majver = options.major_os_version;
    ohdr->o_os_minver = options.minor_os_version;
    ohdr->o_stack_reserve = options.stack_reserve;  /* see MS */
    ohdr->o_stack_commit = options.stack_commit;  /* see MS */
    ohdr->o_heap_reserve = options.heap_reserve;  /* see MS */
    ohdr->o_heap_commit = options.heap_commit;  /* see MS */

    /*
     * Reserve room for the sections headers.
     */
    sh = (COFF_SCNHDR *)ip;
    ip = ((COFF_SCNHDR *)ip + 2);

    /*
     * Write section header #1.
     */
    strncpy(sh->s_name, COFF_IDATA2, 8);
    sh->s_vsize = 0;
    sh->s_vaddr = 0;
    sh->s_size = sizeof(COFF_IMPORT);
    sh->s_scnptr = ((char *)ip - (char *)base);
    sh->s_relptr = ((char *)ip - (char *)base) + sh->s_size;
    sh->s_lnnoptr = 0;
    sh->s_nreloc = 3;
    sh->s_nlnno = 0;
    sh->s_flags = COFF_DATA_FLAGS|COFF_STYP_A1;

    /*
     * Write a Import Directory Entry.
     */
    memset(ip, 0, sizeof(COFF_IMPORT));
    ip = (char *)ip + sizeof(COFF_IMPORT);

    /*
     * Reserve room for the relocations.
     */
    re = (COFF_RELOC *)ip;
    ip = ((COFF_RELOC *)ip + 3);

    /*
     * Write relocations.
     */
    re->r_vaddr = offsetof(COFF_IMPORT, i_modname);
    re->r_symndx = 2;
    re->r_type = (options.machine == MACHINE_ARM) ? COFF_R_ARM_ADDR32NB : COFF_R_I386_DIR32NB;
    re++;

    re->r_vaddr = offsetof(COFF_IMPORT, i_oaddrtab);
    re->r_symndx = 3;
    re->r_type = (options.machine == MACHINE_ARM) ? COFF_R_ARM_ADDR32NB : COFF_R_I386_DIR32NB;
    re++;

    re->r_vaddr = offsetof(COFF_IMPORT, i_addrtab);
    re->r_symndx = 4;
    re->r_type = (options.machine == MACHINE_ARM) ? COFF_R_ARM_ADDR32NB : COFF_R_I386_DIR32NB;
    re++;

    sh++;

    /*
     * Write section header #2.
     */
    strncpy(sh->s_name, COFF_IDATA6, 8);
    sh->s_vsize = 0;
    sh->s_vaddr = 0;
    /* sh->s_size = 0; */
    sh->s_scnptr = ((char *)ip - (char *)base);
    sh->s_relptr = 0;
    sh->s_lnnoptr = 0;
    sh->s_nreloc = 0;
    sh->s_nlnno = 0;
    sh->s_flags = COFF_DATA_FLAGS|COFF_STYP_A2;

    ip_raw = ip;

    strcpy(ip, member_name);
    ip = (char *)ip + strlen(ip)+1;

    /* "Hint/Name tables are WORD aligned" */
    ip = align_word_ptr(ip);

    /*
     * Update the section header.
     */
    sh->s_size = ((char *)ip - (char *)ip_raw);

    /*
     * Update the file header.
     */
    fhdr->f_symptr = ((char *)ip - (char *)base);

    /*
     * Reserve room for the symbol table.
     */
    se = (COFF_SYMENT *)ip;
    ip = ((COFF_SYMENT *)ip + fhdr->f_nsyms);

    /*
     * Initialize the string table.
     */
    strptr = ip;
    *(long *)strptr = 0;
    ip = ((long *)ip + 1);

    /*
     * Write symbol table.
     */
    write_symbol_name(se, import_descriptor_name, &ip);
    se->n_value = 0;
    se->n_scnum = 1;
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_EXT;
    se->n_numaux = 0;
    se++;  /* 0++ */

    strncpy(se->n_name, COFF_IDATA2, 8);
    se->n_value = COFF_DATA_FLAGS;
    se->n_scnum = 1;
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_SECT;
    se->n_numaux = 0;
    se++;  /* 1++ */

    strncpy(se->n_name, COFF_IDATA6, 8);
    se->n_value = 0;
    se->n_scnum = 2;
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_STAT;
    se->n_numaux = 0;
    se++;  /* 2++ */

    strncpy(se->n_name, COFF_IDATA4, 8);
    se->n_value = COFF_DATA_FLAGS;
    se->n_scnum = COFF_N_UNDEF;
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_SECT;
    se->n_numaux = 0;
    se++;  /* 3++ */

    strncpy(se->n_name, COFF_IDATA5, 8);
    se->n_value = COFF_DATA_FLAGS;
    se->n_scnum = COFF_N_UNDEF;
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_SECT;
    se->n_numaux = 0;
    se++;  /* 4++ */

    write_symbol_name(se, null_import_descriptor, &ip);
    se->n_value = 0;
    se->n_scnum = COFF_N_UNDEF;
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_EXT;
    se->n_numaux = 0;
    se++;  /* 5++ */

    write_symbol_name(se, name_null_thunk_data, &ip);
    se->n_value = 0;
    se->n_scnum = COFF_N_UNDEF;
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_EXT;
    se->n_numaux = 0;
    se++;  /* 6++ */

    *(long *)strptr = ((char *)ip - strptr);

    return ip;
}

/****************************************************************************
 *                                                                          *
 * Function: write_import2_member                                           *
 *                                                                          *
 * Purpose : Write special import archive member #2.                        *
 *                                                                          *
 * Comment : This member contains a empty Import Directory Table entry,     *
 *           which is used to terminate the directory table.                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void *write_import2_member(void *ip, void *vp)
{
    COFF_FILHDR *fhdr;
    COFF_SCNHDR *sh;
    COFF_SYMENT *se;
    void *base;

    UNREFERENCED_PARAMETER(vp);

    base = ip;

    /*
     * Write the file header.
     */
    fhdr = (COFF_FILHDR *)ip;
    ip = ((COFF_FILHDR *)ip + 1);

    fhdr->f_magic = (options.machine == MACHINE_ARM) ? COFF_F_MAG_ARM : COFF_F_MAG_I386;
    fhdr->f_nscns = 1;
    fhdr->f_timdat = time_stamp;
    /* fhdr->f_symptr = 0; */
    fhdr->f_nsyms = 1;
    fhdr->f_opthdr = 0;
    fhdr->f_flags = COFF_F_32BIT;

    /*
     * Reserve room for the section header.
     */
    sh = (COFF_SCNHDR *)ip;
    ip = ((COFF_SCNHDR *)ip + 1);

    /*
     * Write the section header.
     */
    strncpy(sh->s_name, COFF_IDATA3, 8);
    sh->s_vsize = 0;
    sh->s_vaddr = 0;
    sh->s_size = sizeof(COFF_IMPORT);
    sh->s_scnptr = ((char *)ip - (char *)base);
    sh->s_relptr = 0;
    sh->s_lnnoptr = 0;
    sh->s_nreloc = 0;
    sh->s_nlnno = 0;
    sh->s_flags = COFF_DATA_FLAGS|COFF_STYP_A1;

    /*
     * Write the terminating (null) Import Directory Entry.
     */
    memset(ip, 0, sizeof(COFF_IMPORT));
    ip = (char *)ip + sizeof(COFF_IMPORT);

    /*
     * Update the file header.
     */
    fhdr->f_symptr = ((char *)ip - (char *)base);

    /*
     * Reserve room for the symbol table.
     */
    se = (COFF_SYMENT *)ip;
    ip = ((COFF_SYMENT *)ip + fhdr->f_nsyms);

    /*
     * Initialize the string table.
     */
    strptr = ip;
    *(long *)strptr = 0;
    ip = ((long *)ip + 1);

    /*
     * Write the symbol table.
     */
    write_symbol_name(se, null_import_descriptor, &ip);
    se->n_value = 0;
    se->n_scnum = 1;
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_EXT;
    se->n_numaux = 0;

    *(long *)strptr = ((char *)ip - strptr);

    return ip;
}

/****************************************************************************
 *                                                                          *
 * Function: write_import3_member                                           *
 *                                                                          *
 * Purpose : Write special import archive member #3.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void *write_import3_member(void *ip, void *vp)
{
    COFF_FILHDR *fhdr;
    COFF_SCNHDR *sh;
    COFF_SYMENT *se;
    void *base;

    UNREFERENCED_PARAMETER(vp);

    base = ip;

    /*
     * Write the file header.
     */
    fhdr = (COFF_FILHDR *)ip;
    ip = ((COFF_FILHDR *)ip + 1);

    fhdr->f_magic = (options.machine == MACHINE_ARM) ? COFF_F_MAG_ARM : COFF_F_MAG_I386;
    fhdr->f_nscns = 2;
    fhdr->f_timdat = time_stamp;
    /* fhdr->f_symptr = 0; */
    fhdr->f_nsyms = 1;
    fhdr->f_opthdr = 0;
    fhdr->f_flags = COFF_F_32BIT;

    /*
     * Reserve room for the section headers.
     */
    sh = (COFF_SCNHDR *)ip;
    ip = ((COFF_SCNHDR *)ip + 2);

    /*
     * Write section header #1.
     */
    strncpy(sh->s_name, COFF_IDATA5, 8);
    sh->s_vsize = 0;
    sh->s_vaddr = 0;
    sh->s_size = sizeof(long);
    sh->s_scnptr = ((char *)ip - (char *)base);
    sh->s_relptr = 0;
    sh->s_lnnoptr = 0;
    sh->s_nreloc = 0;
    sh->s_nlnno = 0;
    sh->s_flags = COFF_DATA_FLAGS|COFF_STYP_A4;

    *(long *)ip = 0;
    ip = ((long *)ip + 1);

    sh++;

    /*
     * Write section header #2.
     */
    strncpy(sh->s_name, COFF_IDATA4, 8);
    sh->s_vsize = 0;
    sh->s_vaddr = 0;
    sh->s_size = sizeof(long);
    sh->s_scnptr = ((char *)ip - (char *)base);
    sh->s_relptr = 0;
    sh->s_lnnoptr = 0;
    sh->s_nreloc = 0;
    sh->s_nlnno = 0;
    sh->s_flags = COFF_DATA_FLAGS|COFF_STYP_A4;

    *(long *)ip = 0;
    ip = ((long *)ip + 1);

    /*
     * Update the file header.
     */
    fhdr->f_symptr = ((char *)ip - (char *)base);

    /*
     * Reserve room for the symbol table.
     */
    se = (COFF_SYMENT *)ip;
    ip = ((COFF_SYMENT *)ip + fhdr->f_nsyms);

    /*
     * Initialize the string table.
     */
    strptr = ip;
    *(long *)strptr = 0;
    ip = ((long *)ip + 1);

    /*
     * Write the symbol table.
     */
    write_symbol_name(se, name_null_thunk_data, &ip);
    se->n_value = 0;
    se->n_scnum = 1;
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_EXT;
    se->n_numaux = 0;

    *(long *)strptr = ((char *)ip - strptr);

    return ip;
}

/****************************************************************************
 *                                                                          *
 * Function: write_old_import_member                                        *
 *                                                                          *
 * Purpose : Write old style import archive member.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void *write_old_import_member(void *ip, void *vp)
{
    EXPENTRY *exp = (EXPENTRY *)vp;
    COFF_FILHDR *fhdr;
    COFF_SCNHDR *sh;
    COFF_RELOC *re;
    COFF_SYMENT *se;
    COFF_AUXENT *ae;
    bool_t is_func;
    bool_t is_ord;
    void *base;
    void *ip_raw;
    char name[MAX_PATH];

    base = ip;

    is_func = exp->sym->flags.function;
    is_ord = FALSE;

    /*
     * Write the file header.
     */
    fhdr = (COFF_FILHDR *)ip;
    ip = ((COFF_FILHDR *)ip + 1);

    fhdr->f_magic = (options.machine == MACHINE_ARM) ? COFF_F_MAG_ARM : COFF_F_MAG_I386;
    fhdr->f_nscns = (is_func) ? 4 : 3;
    fhdr->f_timdat = time_stamp;
    /* fhdr->f_symptr = 0; */
    fhdr->f_nsyms = (is_func) ? 11 : 8;
    fhdr->f_opthdr = 0;
    fhdr->f_flags = COFF_F_32BIT;

    /*
     * Reserve room for the sections headers.
     */
    sh = (COFF_SCNHDR *)ip;
    ip = ((COFF_SCNHDR *)ip + fhdr->f_nscns);

    if (is_func)
    {
        /*
         * Write section header #1.
         */
        strncpy(sh->s_name, COFF_TEXT, 8);
        sh->s_vsize = 0;
        sh->s_vaddr = 0;
        sh->s_size = (options.machine == MACHINE_ARM) ? 12 : 6;
        sh->s_scnptr = ((char *)ip - (char *)base);
        sh->s_relptr = ((char *)ip - (char *)base) + sh->s_size;
        sh->s_lnnoptr = 0;
        sh->s_nreloc = 1;
        sh->s_nlnno = 0;
        sh->s_flags = COFF_TEXT_FLAGS|COFF_STYP_COMDAT|COFF_STYP_A2;

        if (options.machine == MACHINE_ARM)
        {
            *(long *)ip = 0xE59FC000, ip = ((long *)ip + 1);  /* ldr ip,[pc,#0] */
            *(long *)ip = 0xE59CF000, ip = ((long *)ip + 1);  /* ldr pc,[ip,#0] */
            *(long *)ip = 0, ip = ((long *)ip + 1);  /* dwd 0x00000000 */
        }
        else
        {
            /* jmp dword ptr ds:[0x00000000] */
#define IMPORT_CODE  "\xFF\x25\0\0\0\0"
#define IMPORT_SIZE  sizeof(IMPORT_CODE)-1
            memcpy(ip, IMPORT_CODE, IMPORT_SIZE);
            ip = (char *)ip + IMPORT_SIZE;
#undef IMPORT_CODE
#undef IMPORT_SIZE
        }

        /*
         * Reserve room for the relocation.
         */
        re = (COFF_RELOC *)ip;
        ip = ((COFF_RELOC *)ip + 1);

        /*
         * Write relocation.
         */
        re->r_vaddr = (options.machine == MACHINE_ARM) ? 8 : 2;
        re->r_symndx = 5;  /* __imp_symbol */
        re->r_type = (options.machine == MACHINE_ARM) ? COFF_R_ARM_ADDR32 : COFF_R_I386_DIR32;

        sh++;
    }

    /*
     * Write section header #2/1.
     */
    strncpy(sh->s_name, COFF_IDATA5, 8);
    sh->s_vsize = 0;
    sh->s_vaddr = 0;
    sh->s_size = sizeof(long);
    sh->s_scnptr = ((char *)ip - (char *)base);
    sh->s_relptr = ((char *)ip - (char *)base) + sh->s_size;
    sh->s_lnnoptr = 0;
    sh->s_nreloc = (is_ord) ? 0 : 1;
    sh->s_nlnno = 0;
    sh->s_flags = COFF_DATA_FLAGS|COFF_STYP_COMDAT|COFF_STYP_A4;

    *(long *)ip = (is_ord) ? exp->ordinal | COFF_IMP_ORDINAL : 0;
    ip = ((long *)ip + 1);

    if (!is_ord)
    {
        /*
         * Reserve room for the relocation.
         */
        re = (COFF_RELOC *)ip;
        ip = ((COFF_RELOC *)ip + 1);

        /*
         * Write relocation.
         */
        re->r_vaddr = 0;
        re->r_symndx = (is_func) ? 8 : 5;  /* ".idata$6" */
        re->r_type = (options.machine == MACHINE_ARM) ? COFF_R_ARM_ADDR32NB : COFF_R_I386_DIR32NB;
    }

    sh++;

    /*
     * Write section header #3/2.
     */
    strncpy(sh->s_name, COFF_IDATA4, 8);
    sh->s_vsize = 0;
    sh->s_vaddr = 0;
    sh->s_size = sizeof(long);
    sh->s_scnptr = ((char *)ip - (char *)base);
    sh->s_relptr = ((char *)ip - (char *)base) + sh->s_size;
    sh->s_lnnoptr = 0;
    sh->s_nreloc = (is_ord) ? 0 : 1;
    sh->s_nlnno = 0;
    sh->s_flags = COFF_DATA_FLAGS|COFF_STYP_COMDAT|COFF_STYP_A4;

    *(long *)ip = (is_ord) ? exp->ordinal | COFF_IMP_ORDINAL : 0;
    ip = ((long *)ip + 1);

    if (!is_ord)
    {
        /*
         * Reserve room for the relocation.
         */
        re = (COFF_RELOC *)ip;
        ip = ((COFF_RELOC *)ip + 1);

        /*
         * Write relocation.
         */
        re->r_vaddr = 0;
        re->r_symndx = (is_func) ? 8 : 5;  /* ".idata$6" */
        re->r_type = (options.machine == MACHINE_ARM) ? COFF_R_ARM_ADDR32NB : COFF_R_I386_DIR32NB;
    }

    sh++;

    /*
     * Write section header #4/3.
     */
    strncpy(sh->s_name, COFF_IDATA6, 8);
    sh->s_vsize = 0;
    sh->s_vaddr = 0;
    /* sh->s_size = 0; */
    sh->s_scnptr = ((char *)ip - (char *)base);
    sh->s_relptr = 0;
    sh->s_lnnoptr = 0;
    sh->s_nreloc = 0;
    sh->s_nlnno = 0;
    sh->s_flags = COFF_DATA_FLAGS|COFF_STYP_COMDAT|COFF_STYP_A2;

    ip_raw = ip;

    *(short *)ip = 0;  /* hint */
    ip = ((short *)ip + 1);

    strcpy(ip, exp->name);
    ip = (char *)ip + strlen(ip)+1;

    /* "Hint/Name tables are WORD aligned" */
    ip = align_word_ptr(ip);

    /*
     * Update the section header.
     */
    sh->s_size = ((char *)ip - (char *)ip_raw);

    /*
     * Update the file header.
     */
    fhdr->f_symptr = ((char *)ip - (char *)base);

    /*
     * Reserve room for the symbol table.
     */
    se = (COFF_SYMENT *)ip;
    ip = ((COFF_SYMENT *)ip + fhdr->f_nsyms);

    /*
     * Initialize the string table.
     */
    strptr = ip;
    *(long *)strptr = 0;
    ip = ((long *)ip + 1);

    /*
     * Write the symbol table.
     */
    if (is_func)
    {
        strncpy(se->n_name, COFF_TEXT, 8);
        se->n_value = 0;
        se->n_scnum = 1;
        se->n_type = COFF_T_NULL;
        se->n_sclass = COFF_C_STAT;
        se->n_numaux = 1;
        se++;  /* 0++ */
        /**/
        ae = (COFF_AUXENT *)se;
        ae->x_ssize = 6;
        ae->x_nreloc = 1;
        ae->x_nlnno = 0;
        ae->x_chksum = 0;
        ae->x_scnum = 0;
        ae->x_sel = COFF_COMDAT_NODUPS;
        se++;  /* 1++ */

        write_symbol_name(se, exp->name, &ip);
        se->n_value = 0;
        se->n_scnum = 1;
        se->n_type = COFF_T_NULL;
        se->n_sclass = COFF_C_EXT;
        se->n_numaux = 0;
        se++;  /* 2++ */
    }

    strncpy(se->n_name, COFF_IDATA5, 8);
    se->n_value = 0;
    se->n_scnum = (is_func) ? 2 : 1;
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_STAT;
    se->n_numaux = 1;
    se++;  /* 0++ / 3++ */
    /**/
    ae = (COFF_AUXENT *)se;
    ae->x_ssize = sizeof(long);
    ae->x_nreloc = (is_ord) ? 0 : 1;
    ae->x_nlnno = 0;
    ae->x_chksum = 0;
    ae->x_scnum = 0;
    ae->x_sel = COFF_COMDAT_NODUPS;
    se++;  /* 1++ / 4++ */

    sprintf(name, "__imp_%s", exp->name);
    write_symbol_name(se, name, &ip);
    se->n_value = 0;
    se->n_scnum = (is_func) ? 2 : 1;
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_EXT;
    se->n_numaux = 0;
    se++;  /* 2++ / 5++ */

    strncpy(se->n_name, COFF_IDATA4, 8);
    se->n_value = 0;
    se->n_scnum = (is_func) ? 3 : 2;
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_STAT;
    se->n_numaux = 1;
    se++;  /* 3++ / 6++ */
    /**/
    ae = (COFF_AUXENT *)se;
    ae->x_ssize = sizeof(long);
    ae->x_nreloc = (is_ord) ? 0 : 1;
    ae->x_nlnno = 0;
    ae->x_chksum = 0;
    ae->x_scnum = (is_func) ? 2 : 1;
    ae->x_sel = COFF_COMDAT_ASSOC;
    se++;  /* 4++ / 7++ */

    strncpy(se->n_name, COFF_IDATA6, 8);
    se->n_value = 0;
    se->n_scnum = (is_func) ? 4 : 3;
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_STAT;
    se->n_numaux = 1;
    se++;  /* 5++ / 8++ */
    /**/
    ae = (COFF_AUXENT *)se;
    ae->x_ssize = sh->s_size;  /* oops! */
    ae->x_nreloc = 0;
    ae->x_nlnno = 0;
    ae->x_chksum = 0;
    ae->x_scnum = (is_func) ? 2 : 1;
    ae->x_sel = COFF_COMDAT_ASSOC;
    se++;  /* 6++ / 9++ */

    write_symbol_name(se, import_descriptor_name, &ip);
    se->n_value = 0;
    se->n_scnum = COFF_N_UNDEF;
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_EXT;
    se->n_numaux = 0;
    se++;  /* 7++ / 10++ */

    *(long *)strptr = ((char *)ip - strptr);

    return ip;
}

/****************************************************************************
 *                                                                          *
 * Function: write_new_import_member                                        *
 *                                                                          *
 * Purpose : Write new style import archive member.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-12-02  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void *write_new_import_member(void *ip, void *vp)
{
    EXPENTRY *exp = (EXPENTRY *)vp;
    COFF_NEWIMP *ni;

    /*
     * Write the import header.
     */
    ni = (COFF_NEWIMP *)ip;
    ip = ((COFF_NEWIMP *)ip + 1);

    ni->ni_sig1 = COFF_F_MAG_UNKNOWN;
    ni->ni_sig2 = COFF_I_MAG_SIG2;
    ni->ni_vstamp = 0;
    ni->ni_magic = (options.machine == MACHINE_ARM) ? COFF_F_MAG_ARM : COFF_F_MAG_I386;
    ni->ni_timdat = time_stamp;
    ni->ni_dsize = strlen(exp->name)+1 + strlen(member_name)+1;
    ni->ni_hint = 0;
    ni->ni_flags = (exp->sym->flags.function ? COFF_I_CODE : COFF_I_DATA)|COFF_I_NAME;

    /*
     * Write the symbol name.
     */
    strcpy(ip, exp->name);
    ip = (char *)ip + strlen(ip)+1;

    /*
     * Write the module name.
     */
    strcpy(ip, member_name);
    ip = (char *)ip + strlen(ip)+1;

    return ip;
}

/****************************************************************************
 *                                                                          *
 * Function: build_new_import_object                                        *
 *                                                                          *
 * Purpose : Build import object from new style member.                     *
 *                                                                          *
 * Comment : Do the same stuff as in write_old_import_member().             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-12-02  Created                                              *
 *                                                                          *
 ****************************************************************************/

void build_new_import_object(void *ip, COFF_NEWIMP *ni, const char *expname, const char *modname)
{
    COFF_FILHDR *fhdr;
    COFF_SCNHDR *sh;
    COFF_RELOC *re;
    COFF_SYMENT *se;
    COFF_AUXENT *ae;
    bool_t is_func;
    bool_t is_ord;
    void *base;
    void *ip_raw;
    char name[MAX_PATH];
    char *dot = strchr(modname, '.');

    base = ip;

    is_func = (COFF_I_NEWTYPE(ni->ni_flags) == COFF_I_CODE);
    is_ord  = (COFF_I_NEWNAME(ni->ni_flags) == COFF_I_ORDINAL);

    /*
     * Write the file header.
     */
    fhdr = (COFF_FILHDR *)ip;
    ip = ((COFF_FILHDR *)ip + 1);

    fhdr->f_magic = (options.machine == MACHINE_ARM) ? COFF_F_MAG_ARM : COFF_F_MAG_I386;
    fhdr->f_nscns = (is_func) ? 4 : 3;
    fhdr->f_timdat = time_stamp;
    /* fhdr->f_symptr = 0; */
    fhdr->f_nsyms = (is_func) ? 11 : 8;
    fhdr->f_opthdr = 0;
    fhdr->f_flags = COFF_F_32BIT;

    /*
     * Reserve room for the sections headers.
     */
    sh = (COFF_SCNHDR *)ip;
    ip = ((COFF_SCNHDR *)ip + fhdr->f_nscns);

    if (is_func)
    {
        /*
         * Write section header #1.
         */
        strncpy(sh->s_name, COFF_TEXT, 8);
        sh->s_vsize = 0;
        sh->s_vaddr = 0;
        sh->s_size = (options.machine == MACHINE_ARM) ? 12 : 6;
        sh->s_scnptr = ((char *)ip - (char *)base);
        sh->s_relptr = ((char *)ip - (char *)base) + sh->s_size;
        sh->s_lnnoptr = 0;
        sh->s_nreloc = 1;
        sh->s_nlnno = 0;
        sh->s_flags = COFF_TEXT_FLAGS|COFF_STYP_COMDAT|COFF_STYP_A2;

        if (options.machine == MACHINE_ARM)
        {
            *(long *)ip = 0xE59FC000, ip = ((long *)ip + 1);  /* ldr ip,[pc,#0] */
            *(long *)ip = 0xE59CF000, ip = ((long *)ip + 1);  /* ldr pc,[ip,#0] */
            *(long *)ip = 0, ip = ((long *)ip + 1);  /* dwd 0x00000000 */
        }
        else
        {
            /* jmp dword ptr ds:[0x00000000] */
#define IMPORT_CODE  "\xFF\x25\0\0\0\0"
#define IMPORT_SIZE  sizeof(IMPORT_CODE)-1
            memcpy(ip, IMPORT_CODE, IMPORT_SIZE);
            ip = (char *)ip + IMPORT_SIZE;
#undef IMPORT_CODE
#undef IMPORT_SIZE
        }

        /*
         * Reserve room for the relocation.
         */
        re = (COFF_RELOC *)ip;
        ip = ((COFF_RELOC *)ip + 1);

        /*
         * Write relocation.
         */
        re->r_vaddr = (options.machine == MACHINE_ARM) ? 8 : 2;
        re->r_symndx = 5;  /* __imp_symbol */
        re->r_type = (options.machine == MACHINE_ARM) ? COFF_R_ARM_ADDR32 : COFF_R_I386_DIR32;

        sh++;
    }

    /*
     * Write section header #2/1.
     */
    strncpy(sh->s_name, COFF_IDATA5, 8);
    sh->s_vsize = 0;
    sh->s_vaddr = 0;
    sh->s_size = sizeof(long);
    sh->s_scnptr = ((char *)ip - (char *)base);
    sh->s_relptr = ((char *)ip - (char *)base) + sh->s_size;
    sh->s_lnnoptr = 0;
    sh->s_nreloc = (is_ord) ? 0 : 1;
    sh->s_nlnno = 0;
    sh->s_flags = COFF_DATA_FLAGS|COFF_STYP_COMDAT|COFF_STYP_A4;

    *(long *)ip = (is_ord) ? ni->ni_ordinal | COFF_IMP_ORDINAL : 0;
    ip = ((long *)ip + 1);

    if (!is_ord)
    {
        /*
         * Reserve room for the relocation.
         */
        re = (COFF_RELOC *)ip;
        ip = ((COFF_RELOC *)ip + 1);

        /*
         * Write relocation.
         */
        re->r_vaddr = 0;
        re->r_symndx = (is_func) ? 8 : 5;  /* ".idata$6" */
        re->r_type = (options.machine == MACHINE_ARM) ? COFF_R_ARM_ADDR32NB : COFF_R_I386_DIR32NB;
    }

    sh++;

    /*
     * Write section header #3/2.
     */
    strncpy(sh->s_name, COFF_IDATA4, 8);
    sh->s_vsize = 0;
    sh->s_vaddr = 0;
    sh->s_size = sizeof(long);
    sh->s_scnptr = ((char *)ip - (char *)base);
    sh->s_relptr = ((char *)ip - (char *)base) + sh->s_size;
    sh->s_lnnoptr = 0;
    sh->s_nreloc = (is_ord) ? 0 : 1;
    sh->s_nlnno = 0;
    sh->s_flags = COFF_DATA_FLAGS|COFF_STYP_COMDAT|COFF_STYP_A4;

    *(long *)ip = (is_ord) ? ni->ni_ordinal | COFF_IMP_ORDINAL : 0;
    ip = ((long *)ip + 1);

    if (!is_ord)
    {
        /*
         * Reserve room for the relocation.
         */
        re = (COFF_RELOC *)ip;
        ip = ((COFF_RELOC *)ip + 1);

        /*
         * Write relocation.
         */
        re->r_vaddr = 0;
        re->r_symndx = (is_func) ? 8 : 5;  /* ".idata$6" */
        re->r_type = (options.machine == MACHINE_ARM) ? COFF_R_ARM_ADDR32NB : COFF_R_I386_DIR32NB;
    }

    sh++;

    /*
     * Write section header #4/3.
     */
    strncpy(sh->s_name, COFF_IDATA6, 8);
    sh->s_vsize = 0;
    sh->s_vaddr = 0;
    /* sh->s_size = 0; */
    sh->s_scnptr = ((char *)ip - (char *)base);
    sh->s_relptr = 0;
    sh->s_lnnoptr = 0;
    sh->s_nreloc = 0;
    sh->s_nlnno = 0;
    sh->s_flags = COFF_DATA_FLAGS|COFF_STYP_COMDAT|COFF_STYP_A2;

    ip_raw = ip;

    *(short *)ip = ni->ni_hint;
    ip = ((short *)ip + 1);

    symbol_from_export_name(ip, expname, ni->ni_flags);
    ip = (char *)ip + strlen(ip)+1;

    /* "Hint/Name tables are WORD aligned" */
    ip = align_word_ptr(ip);

    /*
     * Update the section header.
     */
    sh->s_size = ((char *)ip - (char *)ip_raw);

    /*
     * Update the file header.
     */
    fhdr->f_symptr = ((char *)ip - (char *)base);

    /*
     * Reserve room for the symbol table.
     */
    se = (COFF_SYMENT *)ip;
    ip = ((COFF_SYMENT *)ip + fhdr->f_nsyms);

    /*
     * Initialize the string table.
     */
    strptr = ip;
    *(long *)strptr = 0;
    ip = ((long *)ip + 1);

    /*
     * Write the symbol table.
     */
    if (is_func)
    {
        strncpy(se->n_name, COFF_TEXT, 8);
        se->n_value = 0;
        se->n_scnum = 1;
        se->n_type = COFF_T_NULL;
        se->n_sclass = COFF_C_STAT;
        se->n_numaux = 1;
        se++;  /* 0++ */
        /**/
        ae = (COFF_AUXENT *)se;
        ae->x_ssize = 6;
        ae->x_nreloc = 1;
        ae->x_nlnno = 0;
        ae->x_chksum = 0;
        ae->x_scnum = 0;
        ae->x_sel = COFF_COMDAT_NODUPS;
        se++;  /* 1++ */

        write_symbol_name(se, expname, &ip);
        se->n_value = 0;
        se->n_scnum = 1;
        se->n_type = COFF_T_NULL;
        se->n_sclass = COFF_C_EXT;
        se->n_numaux = 0;
        se++;  /* 2++ */
    }

    strncpy(se->n_name, COFF_IDATA5, 8);
    se->n_value = 0;
    se->n_scnum = (is_func) ? 2 : 1;
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_STAT;
    se->n_numaux = 1;
    se++;  /* 0++ / 3++ */
    /**/
    ae = (COFF_AUXENT *)se;
    ae->x_ssize = sizeof(long);
    ae->x_nreloc = (is_ord) ? 0 : 1;
    ae->x_nlnno = 0;
    ae->x_chksum = 0;
    ae->x_scnum = 0;
    ae->x_sel = COFF_COMDAT_NODUPS;
    se++;  /* 1++ / 4++ */

    sprintf(name, "__imp_%s", expname);
    write_symbol_name(se, name, &ip);
    se->n_value = 0;
    se->n_scnum = (is_func) ? 2 : 1;
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_EXT;
    se->n_numaux = 0;
    se++;  /* 2++ / 5++ */

    strncpy(se->n_name, COFF_IDATA4, 8);
    se->n_value = 0;
    se->n_scnum = (is_func) ? 3 : 2;
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_STAT;
    se->n_numaux = 1;
    se++;  /* 3++ / 6++ */
    /**/
    ae = (COFF_AUXENT *)se;
    ae->x_ssize = sizeof(long);
    ae->x_nreloc = (is_ord) ? 0 : 1;
    ae->x_nlnno = 0;
    ae->x_chksum = 0;
    ae->x_scnum = (is_func) ? 2 : 1;
    ae->x_sel = COFF_COMDAT_ASSOC;
    se++;  /* 4++ / 7++ */

    strncpy(se->n_name, COFF_IDATA6, 8);
    se->n_value = 0;
    se->n_scnum = (is_func) ? 4 : 3;
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_STAT;
    se->n_numaux = 1;
    se++;  /* 5++ / 8++ */
    /**/
    ae = (COFF_AUXENT *)se;
    ae->x_ssize = sh->s_size;  /* oops! */
    ae->x_nreloc = 0;
    ae->x_nlnno = 0;
    ae->x_chksum = 0;
    ae->x_scnum = (is_func) ? 2 : 1;
    ae->x_sel = COFF_COMDAT_ASSOC;
    se++;  /* 6++ / 9++ */

    /* Symbol used to pull in the Import Directory Entry */
    sprintf(name, "__IMPORT_DESCRIPTOR_%.*s",
        (dot) ? dot-modname : strlen(modname), modname);

    write_symbol_name(se, name, &ip);
    se->n_value = 0;
    se->n_scnum = COFF_N_UNDEF;
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_EXT;
    se->n_numaux = 0;
    se++;  /* 7++ / 10++ */

    *(long *)strptr = ((char *)ip - strptr);
}

/****************************************************************************
 *                                                                          *
 * Function: symbol_from_export_name                                        *
 *                                                                          *
 * Purpose : Build symbol name out of new export symbol name.               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-12-02  Created                                              *
 *                                                                          *
 ****************************************************************************/

#define is_export_prefix(c)  ((c) == '?' || (c) == '@' || (c) == '_')
#define is_export_suffix(c)  ((c) == '@' || (c) == '\0')

void symbol_from_export_name(char *symname, const char *expname, ushort_t flags)
{
    switch (flags & COFF_I_NMASK)
    {
        case COFF_I_NAME:
            strcpy(symname, expname);
            break;

        case COFF_I_NAME_NOPREF:
            if (is_export_prefix(*expname)) expname++;
            strcpy(symname, expname);
            break;

        case COFF_I_NAME_UNDEC:
            if (is_export_prefix(*expname)) expname++;
            while (!is_export_suffix(*expname))
                *symname++ = *expname++;
            *symname = '\0';
            break;

        default:
            *symname = '\0';
            break;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: write_archive_member_header                                    *
 *                                                                          *
 * Purpose : Write a member header to the archive file.                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static size_t write_archive_member_header(COFF_ARHDR *arhdr,
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
 * Function: write_special_archive_members                                  *
 *                                                                          *
 * Purpose : Write the special members to the archive file.                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void *write_special_archive_members(void *ip)
{
    size_t sizeof_linker1;
    size_t sizeof_linker2;
    size_t sizeof_longnames;
    size_t delta;
    size_t i;
    void *tp;

    /* Calculate actual size of the three special members */
    sizeof_linker1 = sizeof_first_linker_member();
    sizeof_linker2 = sizeof_second_linker_member();
    sizeof_longnames = sizeof_longnames_member();

    /* Calculate number of bytes to insert at beginning of file */
    delta = COFF_ARMSZ;
    delta += archive_member_size(sizeof_linker1);
    delta += archive_member_size(sizeof_linker2);
    delta += archive_member_size(sizeof_longnames);

    for (i = 0; i < member_count; i++)
        member_offsets[i] += delta;

    for (i = 0; i < export_count; i++)
        export_list[i]->offset += delta;

    tp = lib_file->base;

    /* Make room to insert the three members in */
    memmove((char *)tp + delta, tp, (char *)ip - (char *)tp);
    ip = (char *)ip + delta;

    /* Write the magic header */
    memcpy(tp, COFF_ARMAG, COFF_ARMSZ);
    tp = (char *)tp + COFF_ARMSZ;

    /* Write the special archive members */
    tp = write_first_linker_member(tp, sizeof_linker1);
    tp = write_second_linker_member(tp, sizeof_linker2);
    tp = write_longnames_member(tp, sizeof_longnames);

    return ip;
}

/****************************************************************************
 *                                                                          *
 * Function: sizeof_first_linker_member                                     *
 *                                                                          *
 * Purpose : Calculate the size of the first linker member.                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static size_t sizeof_first_linker_member(void)
{
    size_t size;
    size_t i;

    size = sizeof(long) + export_count * sizeof(long);

    for (i = 0; i < export_count; i++)
        size += strlen(export_list[i]->name) + 1;  /* include '\0' */

    return size;
}

/****************************************************************************
 *                                                                          *
 * Function: write_first_linker_member                                      *
 *                                                                          *
 * Purpose : Write the first linker member to the archive file.             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void *write_first_linker_member(void *ip, size_t size)
{
    size_t i;

    ip = (char *)ip + write_archive_member_header((COFF_ARHDR *)ip, "/", size, 0, time_stamp);

    write_big_endian(ip, (long)export_count);
    ip = ((long *)ip + 1);

    for (i = 0; i < export_count; i++)
    {
        write_big_endian(ip, export_list[i]->offset);
        ip = ((long *)ip + 1);
    }

    for (i = 0; i < export_count; i++)
    {
        strcpy(ip, export_list[i]->name);
        ip = (char *)ip + strlen(ip)+1;
    }

    return align_archive_ptr(ip);
}

/****************************************************************************
 *                                                                          *
 * Function: sizeof_second_linker_member                                    *
 *                                                                          *
 * Purpose : Calculate size of the second linker member.                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static size_t sizeof_second_linker_member(void)
{
    size_t size;
    size_t i;

    size = sizeof(long) + member_count * sizeof(long) +
           sizeof(long) + export_count * sizeof(short);

    for (i = 0; i < export_count; i++)
        size += strlen(export_list[i]->name) + 1;  /* include '\0' */

    return size;
}

/****************************************************************************
 *                                                                          *
 * Function: write_second_linker_member                                     *
 *                                                                          *
 * Purpose : Write the second linker member to the archive file.            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void *write_second_linker_member(void *ip, size_t size)
{
    size_t i;

    ip = (char *)ip + write_archive_member_header((COFF_ARHDR *)ip, "/", size, 0, time_stamp);

    *(long *)ip = member_count;
    ip = ((long *)ip + 1);

    for (i = 0; i < member_count; i++)
    {
        *(long *)ip = member_offsets[i];
        ip = ((long *)ip + 1);
    }

    *(long *)ip = export_count;
    ip = ((long *)ip + 1);

    for (i = 0; i < export_count; i++)
    {
        size_t index;

        for (index = 0; index < member_count; index++)
            if (member_offsets[index] == export_list[i]->offset)
                break;

        *(short *)ip = (short)(1 + index);
        ip = ((short *)ip + 1);
    }

    for (i = 0; i < export_count; i++)
    {
        strcpy(ip, export_list[i]->name);
        ip = (char *)ip + strlen(ip)+1;
    }

    return align_archive_ptr(ip);
}

/****************************************************************************
 *                                                                          *
 * Function: sizeof_longnames_member                                        *
 *                                                                          *
 * Purpose : Calculate size of the longnames member.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static size_t sizeof_longnames_member(void)
{
    size_t len;

    len = strlen(member_name);
    return (len < 16) ? 0 : len+1;
}

/****************************************************************************
 *                                                                          *
 * Function: write_longnames_member                                         *
 *                                                                          *
 * Purpose : Write the longnames member to the archive file.                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void *write_longnames_member(void *ip, size_t size)
{
    if (size == 0)
        return ip;

    ip = (char *)ip + write_archive_member_header((COFF_ARHDR *)ip, "//", size, 0, time_stamp);

    memcpy(ip, member_name, size);
    ip = (char *)ip + size;

    return align_archive_ptr(ip);
}

/****************************************************************************
 *                                                                          *
 * Function: create_import_archive_map                                      *
 *                                                                          *
 * Purpose : Create a read-write file mapping of the import archive file.   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void *create_import_archive_map(void)
{
    WINERR err;

    if (!lib_file)
    {
        char fname[MAX_PATH];

        strcpy(fname, exe_file->name);
        update_extension_in_file(fname, EXT_LIB);
        lookup_file(&lib_file, fname);
    }

    printmsg(MSG_CREATE_EXPORT_ARCHIVE, lib_file->name);

    err = my_createmap(lib_file, MAX_IMPORT_ARCHIVE_SIZE);
    if (err) apperror(RCFATAL(err), lib_file->name);

    return lib_file->base;
}

/****************************************************************************
 *                                                                          *
 * Function: close_import_archive_map                                       *
 *                                                                          *
 * Purpose : Close a file-mapping of the import archive file.               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void close_import_archive_map(void *next_byte)
{
    WINERR err;

    /* Set size, before we call my_closemap() */
    lib_file->size = file_offset(lib_file, next_byte);

    err = my_closemap(lib_file, TRUE);
    if (err) apperror(RCFATAL(err));
}

