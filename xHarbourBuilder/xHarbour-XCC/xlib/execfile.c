/****************************************************************************
 *                                                                          *
 * File    : execfile.c                                                     *
 *                                                                          *
 * Purpose : Win32 Library Manager; executable file management.             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-18  Created                                              *
 *           98-11-07  Support for new import archives added.               *
 *           99-03-27  Use same prefix for code and data: "__imp_".         *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "lib.h"

#define MAX_OBJECT_SIZE     0x10000   /* 64 KB */

#define DATA_SECTION_FLAGS  (COFF_STYP_DATA|COFF_STYP_READ|COFF_STYP_WRITE)
#define TEXT_SECTION_FLAGS  (COFF_STYP_TEXT|COFF_STYP_READ|COFF_STYP_EXEC)

/* Locals */
static char *import_descriptor_name = NULL;
static char *name_null_thunk_data = NULL;
static char *null_import_descriptor = "__NULL_IMPORT_DESCRIPTOR";
static char *strptr = NULL;

/* Static function prototypes */
static COFF_SCNHDR *get_section_header_by_name(const char *, COFF_PEHDRS *);
static COFF_SCNHDR *get_section_header_by_address(addr_t, COFF_PEHDRS *);
static void build_special_symbols(const char *);
static void free_special_symbols(void);
static size_t write_import1_member(void *, const char *);
static size_t write_import2_member(void *);
static size_t write_import3_member(void *);
static size_t write_old_import_member(void *, char *, bool_t);
static size_t write_new_import_member(void *, char *, bool_t, bool_t, char *);
static void write_symbol_name(COFF_SYMENT *, const char *, void **);
static COFF_PEHDRS *open_executable_map(FILEINFO *);
static void close_executable_map(FILEINFO *);

__inline char *align_word_ptr(char *ip)
{
    if ((size_t)ip & 1) *ip++ = 0;
    return ip;
}

/****************************************************************************
 *                                                                          *
 * Function: process_executable_image                                       *
 *                                                                          *
 * Purpose : Open executable image and process all exports.                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-18  Created                                              *
 *           98-11-07  Support for new import archives added.               *
 *           03-07-16  Always prefix functions with an underscore.          *
 *           03-10-02  Bugfix: always prefix names, check forwarded syms.   *
 *                                                                          *
 ****************************************************************************/

void process_executable_image(FILEINFO *exe_file)
{
    COFF_PEHDRS *pehdrs;
    COFF_EXPORT *export;
    COFF_SCNHDR *sh;
    char *member_name;
    addr_t *address_table;
    short *ordinal_table;
    long *nameptr_table;
    void *base;
    void *bp;
    long scnptr = 0;
    long scnsz = 0;
    long i;

    pehdrs = open_executable_map(exe_file);

    /*
     * Look for the export directory.
     */
    sh = get_section_header_by_name(COFF_EDATA, pehdrs);
    if (sh)
    {
        scnptr = sh->s_scnptr;
        scnsz = sh->s_size;
    }
    else
    {
        addr_t vaddr = pehdrs->pe_ohdr.o_pedir[COFF_PE_EXPORT].pd_vaddr;
        sh = get_section_header_by_address(vaddr, pehdrs);
        if (sh)
        {
            scnptr = sh->s_scnptr + (vaddr - sh->s_vaddr);
            scnsz = pehdrs->pe_ohdr.o_pedir[COFF_PE_EXPORT].pd_size;
        }
    }

    if (sh == NULL || scnptr == 0)
        apperror(RCFATAL(ERROR_CANT_FIND_EXPDIR), exe_file->name);

    /*
     * Parse the export directory.
     */
    export = (COFF_EXPORT *)((char *)exe_file->base + scnptr);
    base = (char *)exe_file->base - (sh->s_vaddr - sh->s_scnptr);

    member_name = (char *)(export->e_modname + (char *)base);
    build_special_symbols(member_name);

    address_table = (addr_t *)(export->e_addrtab + (char *)base);
    ordinal_table = (short *)(export->e_ordtab + (char *)base);
    nameptr_table = (long *)(export->e_nametab + (char *)base);

    /* Temporary member buffer */
    bp = my_alloc(MAX_OBJECT_SIZE);

    /*
     * Write the special import archive members.
     */
    process_object_module(member_name, write_import1_member(bp, member_name), bp, time_stamp);
    process_object_module(member_name, write_import2_member(bp), bp, time_stamp);
    process_object_module(member_name, write_import3_member(bp), bp, time_stamp);

    /*
     * Write the import archive members.
     */
    for (i = 0; i < export->e_naddrs; i++)
    {
        char decname[MAX_PATH], *name;
        bool_t is_func;
        long fwdptr;

        name = (i < export->e_nnames) ? nameptr_table[i] + (char *)base : "noname";
        sh = get_section_header_by_address(address_table[i], pehdrs);

        /* Assume function, if located in code segment... */
        is_func = (sh->s_flags & COFF_STYP_TEXT) != 0;

        fwdptr = sh->s_scnptr + (address_table[i] - sh->s_vaddr);
        if (fwdptr >= scnptr && fwdptr < scnptr + scnsz)
        {
            /* Assume function, if forwarded to another DLL... */
            apperror(RCWARNING(ERROR_FORWARDED_SYMBOL), name, (char *)exe_file->base + fwdptr);
            is_func = TRUE;
        }

        if (*name != '_')
        {
            sprintf(decname, "_%s", name);
            name = decname;
        }

        if (options.old_implib)
        {
            /* Write import object in the old, verbose, format */
            process_object_module(member_name, write_old_import_member(
                bp, name, is_func), bp, time_stamp);
        }
        else
        {
            /* Write import object in the new, terse, format */
            process_object_module(member_name, write_new_import_member(
                bp, name, is_func, name == decname, member_name), bp, time_stamp);
        }
    }

    my_free(bp);

    free_special_symbols();

    close_executable_map(exe_file);
}

/****************************************************************************
 *                                                                          *
 * Function: get_section_header_by_name                                     *
 *                                                                          *
 * Purpose : Return section header by name.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

static COFF_SCNHDR *get_section_header_by_name(
    const char *name, COFF_PEHDRS *pehdrs)
{
    COFF_SCNHDR *sh;
    int i;

    sh = COFF_PEHDRS_FIRST_SCNHDR(pehdrs);
    for (i = 0; i < pehdrs->pe_fhdr.f_nscns; i++, sh++)
    {
        if (strcmp(sh->s_name, name) == 0)
            return sh;
    }

    return NULL;
}

/****************************************************************************
 *                                                                          *
 * Function: get_section_header_by_address                                  *
 *                                                                          *
 * Purpose : Return section header by virtual address.                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

static COFF_SCNHDR *get_section_header_by_address(
    addr_t vaddr, COFF_PEHDRS *pehdrs)
{
    COFF_SCNHDR *sh;
    int i;

    sh = COFF_PEHDRS_FIRST_SCNHDR(pehdrs);
    for (i = 0; i < pehdrs->pe_fhdr.f_nscns; i++, sh++)
    {
        if (sh->s_vaddr <= vaddr && vaddr < sh->s_vaddr + sh->s_vsize)
            return sh;
    }

    return NULL;
}

/****************************************************************************
 *                                                                          *
 * Function: build_special_symbols                                          *
 *                                                                          *
 * Purpose : Build the special export symbol names (old implib's).          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void build_special_symbols(const char *member_name)
{
    char expname[MAX_PATH];
    char name[MAX_PATH];

    strcpy(expname, member_name);
    *strchr(expname, '.') = '\0';

    /* Symbol used to pull in the Import Directory Entry */
    sprintf(name, "__IMPORT_DESCRIPTOR_%s", expname);
    import_descriptor_name = tstrcpy(name);

    /* Symbol used to terminate the Import Lookup Table */
    sprintf(name, "\177%s_NULL_THUNK_DATA", expname);
    name_null_thunk_data = tstrcpy(name);
}

/****************************************************************************
 *                                                                          *
 * Function: free_special_symbols                                           *
 *                                                                          *
 * Purpose : Free the special export symbol names (old implib's).           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void free_special_symbols(void)
{
    my_free(import_descriptor_name);
    import_descriptor_name = NULL;

    my_free(name_null_thunk_data);
    name_null_thunk_data = NULL;
}

/****************************************************************************
 *                                                                          *
 * Function: write_import1_member                                           *
 *                                                                          *
 * Purpose : Write special import archive member #1 (old implib's).         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

static size_t write_import1_member(void *ip, const char *member_name)
{
    COFF_FILHDR *fhdr;
    COFF_OPTHDR *ohdr;
    COFF_SCNHDR *sh;
    COFF_RELOC *re;
    COFF_SYMENT *se;
    void *base;
    void *ip_raw;

    base = ip;

    /*
     * Write the file header.
     */
    fhdr = (COFF_FILHDR *)ip;
    ip = ((COFF_FILHDR *)ip + 1);

    fhdr->f_magic = COFF_F_MAG_I386;
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

    /*
     * Microsoft define these. Since we don't know why, we do the same...
     */
    ohdr->o_magic = COFF_O_MAG_NT;
    ohdr->o_ld_majver = MAJOR_LINKER_VERSION;
    ohdr->o_ld_minver = MINOR_LINKER_VERSION;
    ohdr->o_salign = SECTION_ALIGNMENT;
    ohdr->o_falign = FILE_ALIGNMENT;
    ohdr->o_os_majver = MAJOR_OS_VERSION;
    ohdr->o_os_minver = MINOR_OS_VERSION;
    ohdr->o_stack_reserve = DEFAULT_STACK_RESERVE;
    ohdr->o_stack_commit = DEFAULT_STACK_COMMIT;
    ohdr->o_heap_reserve = DEFAULT_HEAP_RESERVE;
    ohdr->o_heap_commit = DEFAULT_HEAP_COMMIT;

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
    sh->s_relptr = ((char *)ip - (char *)base) + sizeof(COFF_IMPORT);
    sh->s_lnnoptr = 0;
    sh->s_nreloc = 3;
    sh->s_nlnno = 0;
    sh->s_flags = DATA_SECTION_FLAGS|COFF_STYP_A1;

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
    re->r_type = COFF_R_I386_DIR32NB;
    re++;

    re->r_vaddr = offsetof(COFF_IMPORT, i_oaddrtab);
    re->r_symndx = 3;
    re->r_type = COFF_R_I386_DIR32NB;
    re++;

    re->r_vaddr = offsetof(COFF_IMPORT, i_addrtab);
    re->r_symndx = 4;
    re->r_type = COFF_R_I386_DIR32NB;
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
    sh->s_flags = DATA_SECTION_FLAGS|COFF_STYP_A2;

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
    se->n_value = DATA_SECTION_FLAGS;
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
    se->n_value = DATA_SECTION_FLAGS;
    se->n_scnum = COFF_N_UNDEF;
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_SECT;
    se->n_numaux = 0;
    se++;  /* 3++ */

    strncpy(se->n_name, COFF_IDATA5, 8);
    se->n_value = DATA_SECTION_FLAGS;
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

    return (char *)ip - (char *)base;
}

/****************************************************************************
 *                                                                          *
 * Function: write_import2_member                                           *
 *                                                                          *
 * Purpose : Write special import archive member #2 (old implib's).         *
 *                                                                          *
 * Comment : This member contains a empty Import Directory Table entry,     *
 *           which is used to terminate the directory table.                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

static size_t write_import2_member(void *ip)
{
    COFF_FILHDR *fhdr;
    COFF_SCNHDR *sh;
    COFF_SYMENT *se;
    void *base;

    base = ip;

    /*
     * Write the file header.
     */
    fhdr = (COFF_FILHDR *)ip;
    ip = ((COFF_FILHDR *)ip + 1);

    fhdr->f_magic = COFF_F_MAG_I386;
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
    sh->s_flags = DATA_SECTION_FLAGS|COFF_STYP_A1;

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

    return (char *)ip - (char *)base;
}

/****************************************************************************
 *                                                                          *
 * Function: write_import3_member                                           *
 *                                                                          *
 * Purpose : Write special import archive member #3 (old implib's).         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

static size_t write_import3_member(void *ip)
{
    COFF_FILHDR *fhdr;
    COFF_SCNHDR *sh;
    COFF_SYMENT *se;
    void *base;

    base = ip;

    /*
     * Write the file header.
     */
    fhdr = (COFF_FILHDR *)ip;
    ip = ((COFF_FILHDR *)ip + 1);

    fhdr->f_magic = COFF_F_MAG_I386;
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
    sh->s_flags = DATA_SECTION_FLAGS|COFF_STYP_A4;

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
    sh->s_flags = DATA_SECTION_FLAGS|COFF_STYP_A4;

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

    return (char *)ip - (char *)base;
}

/****************************************************************************
 *                                                                          *
 * Function: write_old_import_member                                        *
 *                                                                          *
 * Purpose : Write old style import archive member.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

static size_t write_old_import_member(void *ip, char *expname, bool_t is_func)
{
    COFF_FILHDR *fhdr;
    COFF_SCNHDR *sh;
    COFF_RELOC *re;
    COFF_SYMENT *se;
    COFF_AUXENT *ae;
    bool_t is_ord = FALSE;
    ulong_t ordinal = 0;
    void *base;
    void *ip_raw;
    char name[MAX_PATH];

    base = ip;

    /*
     * Write the file header.
     */
    fhdr = (COFF_FILHDR *)ip;
    ip = ((COFF_FILHDR *)ip + 1);

    fhdr->f_magic = COFF_F_MAG_I386;
    fhdr->f_nscns = (ushort_t)(is_func ? 4 : 3);
    fhdr->f_timdat = time_stamp;
    /* fhdr->f_symptr = 0; */
    fhdr->f_nsyms = (is_func) ? 11 : 8;
    fhdr->f_opthdr = 0;
    fhdr->f_flags = COFF_F_32BIT;

    /*
     * Reserve room for the sections headers.
     */
    sh = (COFF_SCNHDR *)ip;
    ip = ((COFF_SCNHDR *)ip + fhdr->f_nsyms);

    if (is_func)
    {
        /*
         * Write section header #1.
         */
        strncpy(sh->s_name, COFF_TEXT, 8);
        sh->s_vsize = 0;
        sh->s_vaddr = 0;
        sh->s_size = 6;
        sh->s_scnptr = ((char *)ip - (char *)base);
        sh->s_relptr = ((char *)ip - (char *)base) + 6;
        sh->s_lnnoptr = 0;
        sh->s_nreloc = 1;
        sh->s_nlnno = 0;
        sh->s_flags = TEXT_SECTION_FLAGS|COFF_STYP_COMDAT|COFF_STYP_A2;

        /* jmp dword ptr ds:[0x00000000] */
#define IMPORT_CODE  "\xFF\x25\0\0\0\0"
#define IMPORT_SIZE  sizeof(IMPORT_CODE)-1
        memcpy(ip, IMPORT_CODE, IMPORT_SIZE);
        ip = (char *)ip + IMPORT_SIZE;
#undef IMPORT_CODE
#undef IMPORT_SIZE

        /*
         * Reserve room for the relocation.
         */
        re = (COFF_RELOC *)ip;
        ip = ((COFF_RELOC *)ip + 1);

        /*
         * Write relocation.
         */
        re->r_vaddr = 2;
        re->r_symndx = 5;  /* __imp_symbol */
        re->r_type = COFF_R_I386_DIR32;

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
    sh->s_relptr = ((char *)ip - (char *)base) + sizeof(long);
    sh->s_lnnoptr = 0;
    sh->s_nreloc = (ushort_t)(is_ord ? 0 : 1);
    sh->s_nlnno = 0;
    sh->s_flags = DATA_SECTION_FLAGS|COFF_STYP_COMDAT|COFF_STYP_A4;

    *(long *)ip = (is_ord) ? ordinal|COFF_IMP_ORDINAL : 0;
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
        re->r_type = COFF_R_I386_DIR32NB;
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
    sh->s_relptr = ((char *)ip - (char *)base) + sizeof(long);
    sh->s_lnnoptr = 0;
    sh->s_nreloc = (ushort_t)(is_ord ? 0 : 1);
    sh->s_nlnno = 0;
    sh->s_flags = DATA_SECTION_FLAGS|COFF_STYP_COMDAT|COFF_STYP_A4;

    *(long *)ip = (is_ord) ? ordinal|COFF_IMP_ORDINAL : 0;
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
        re->r_type = COFF_R_I386_DIR32NB;
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
    sh->s_flags = DATA_SECTION_FLAGS|COFF_STYP_COMDAT|COFF_STYP_A2;

    ip_raw = ip;

    *(short *)ip = 0;  /* hint */
    ip = ((short *)ip + 1);

    strcpy(ip, expname);
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
    se->n_scnum = (short)(is_func ? 2 : 1);
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_STAT;
    se->n_numaux = 1;
    se++;  /* 0++ / 3++ */
    /**/
    ae = (COFF_AUXENT *)se;
    ae->x_ssize = sizeof(long);
    ae->x_nreloc = (ushort_t)(is_ord ? 0 : 1);
    ae->x_nlnno = 0;
    ae->x_chksum = 0;
    ae->x_scnum = 0;
    ae->x_sel = COFF_COMDAT_NODUPS;
    se++;  /* 1++ / 4++ */

    sprintf(name, "__imp_%s", expname);
    write_symbol_name(se, name, &ip);
    se->n_value = 0;
    se->n_scnum = (short)(is_func ? 2 : 1);
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_EXT;
    se->n_numaux = 0;
    se++;  /* 2++ / 5++ */

    strncpy(se->n_name, COFF_IDATA4, 8);
    se->n_value = 0;
    se->n_scnum = (short)(is_func ? 3 : 2);
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_STAT;
    se->n_numaux = 1;
    se++;  /* 3++ / 6++ */
    /**/
    ae = (COFF_AUXENT *)se;
    ae->x_ssize = sizeof(long);
    ae->x_nreloc = (ushort_t)(is_ord ? 0 : 1);
    ae->x_nlnno = 0;
    ae->x_chksum = 0;
    ae->x_scnum = (short)(is_func ? 2 : 1);
    ae->x_sel = COFF_COMDAT_ASSOC;
    se++;  /* 4++ / 7++ */

    strncpy(se->n_name, COFF_IDATA6, 8);
    se->n_value = 0;
    se->n_scnum = (short)(is_func ? 4 : 3);
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
    ae->x_scnum = (short)(is_func ? 2 : 1);
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

    return (char *)ip - (char *)base;
}

/****************************************************************************
 *                                                                          *
 * Function: write_new_import_member                                        *
 *                                                                          *
 * Purpose : Write new style import archive member.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-11-07  Created                                              *
 *           03-07-16  Changed COFF_I_NAME to COFF_I_NAME_NOPREF.           *
 *           03-10-02  Bugfix: Added argument is_pref.                      *
 *                                                                          *
 ****************************************************************************/

static size_t write_new_import_member(void *ip, char *expname, bool_t is_func, bool_t is_pref, char *member_name)
{
    COFF_NEWIMP *ni;
    void *base;

    base = ip;

    /*
     * Write the import header.
     */
    ni = (COFF_NEWIMP *)ip;
    ip = ((COFF_NEWIMP *)ip + 1);

    ni->ni_sig1 = COFF_F_MAG_UNKNOWN;
    ni->ni_sig2 = COFF_I_MAG_SIG2;
    ni->ni_vstamp = 0;
    ni->ni_magic = COFF_F_MAG_I386;
    ni->ni_timdat = time_stamp;
    ni->ni_dsize = strlen(expname)+1 + strlen(member_name)+1;
    ni->ni_hint = 0;
    ni->ni_flags = (ushort_t)((is_func ? COFF_I_CODE : COFF_I_DATA)|(is_pref ? COFF_I_NAME_NOPREF : COFF_I_NAME));

    /*
     * Write the symbol name.
     */
    strcpy(ip, expname);
    ip = (char *)ip + strlen(ip)+1;

    /*
     * Write the module name.
     */
    strcpy(ip, member_name);
    ip = (char *)ip + strlen(ip)+1;

    return (char *)ip - (char *)base;
}

/****************************************************************************
 *                                                                          *
 * Function: write_symbol_name                                              *
 *                                                                          *
 * Purpose : Write the name of a symbol table entry.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-18  Created                                              *
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
 * Function: open_executable_map                                            *
 *                                                                          *
 * Purpose : Create a file-mapping of a executable file.                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

static COFF_PEHDRS *open_executable_map(FILEINFO *exe_file)
{
    EXE_DOSHDR *mzhdr;
    COFF_PEHDRS *pehdrs;
    WINERR err;

    err = my_openmap(exe_file);
    if (err) apperror(MYOPENERROR(RCFATAL(err)), exe_file->name);

    mzhdr = (EXE_DOSHDR *)exe_file->base;
    pehdrs = (COFF_PEHDRS *)((char *)mzhdr + mzhdr->mz_lfanew);

    if (mzhdr->mz_magic != EXE_DOS_MAGIC ||
        mzhdr->mz_lfanew + sizeof(COFF_PEHDRS) > exe_file->size ||
        pehdrs->pe_magic != EXE_NT_MAGIC)
    {
        apperror(RCFATAL(ERROR_INVALID_EXECFILE), exe_file->name);
    }

    return pehdrs;
}

/****************************************************************************
 *                                                                          *
 * Function: close_executable_map                                           *
 *                                                                          *
 * Purpose : Close a file-mapping of a executable file.                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-18  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void close_executable_map(FILEINFO *exe_file)
{
    WINERR err;

    err = my_closemap(exe_file, FALSE);
    if (err) apperror(RCFATAL(err));
}

