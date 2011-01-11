/****************************************************************************
 *                                                                          *
 * File    : debug.c                                                        *
 *                                                                          *
 * Purpose : Win32 Linker; debug information management.                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-12-05  Created                                              *
 *           03-06-15  Replaced CVPACK with own code (compact_types).       *
 *           03-07-20  Bugfix: must group all CodeView types (group_types). *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#pragma warning(push)
#pragma warning(disable: 4200)  /* zero-sized array in struct/union */
#pragma warning(disable: 4214)  /* bit field types other than int */
#include "codeview.h"
#pragma warning(pop)

#include "link.h"

#define IS_DEBUG_SYMBOL(sym) \
    (!(sym)->flags.internal && (sym)->name[0] != '.')

#define SYMBOL_SECTION(sym) \
    (((sym)->flags.absolute) ? COFF_N_ABS : sym->scnum)

#define SYMBOL_TYPE(sym) \
    (COFF_T_NULL | (((sym)->flags.function) ? \
    (COFF_DT_FCN << COFF_N_BTSHFT) : 0))

/* Locals */
static char *strptr = NULL;

/* Static function prototypes */
static void *write_misc_debug_info(void *, void *, COFF_DBGDIR *, COFF_FILHDR *);
static void *write_fpo_debug_info(void *, void *, COFF_DBGDIR *, COFF_FILHDR *);
static void *write_coff_debug_info(void *, void *, COFF_DBGDIR *, COFF_FILHDR *);
static __inline uchar_t number_of_aux_file_entries(char *);
static void write_symbol_name(COFF_SYMENT *, const char *, void **);
static void *write_codeview_debug_info(void *, void *, COFF_DBGDIR *, COFF_FILHDR *);
static short number_of_linenumber_segments(MODENTRY *);
static __inline void *write_length_prefixed_name(uchar_t *, char *);
static short library_index(MODENTRY *);
static void group_types(void);
static void compact_types(void);
static void relocate_symbol_type(MODENTRY *, cv_typ16_t, cv_typ16_t);
static void relocate_type_type(MODENTRY *, cv_typ16_t, cv_typ16_t);
static uchar_t *skip_value(void *);

/****************************************************************************
 *                                                                          *
 * Function: alloc_debug_directory                                          *
 *                                                                          *
 * Purpose : Allocate a empty debug directory segment.                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-12-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

void alloc_debug_directory(void)
{
    if (options.debug)
    {
        int entries = 1;  /* misc debug info */
        SCNENTRY *scn;
        SEGENTRY *seg;

        if (options.debug_coff) entries++;
        if (options.debug_cv) entries++;
        if (scn_fpo) entries++;

        /*
         * Allocate a section buffer.
         */
        scn = lookup_section(COFF_RDATA, COFF_RDATA_FLAGS);
        seg = seg_dbgdir = add_segment_to_section(scn);

        seg->alignment = sizeof(long);
        seg->size = COFF_DBGDSZ * entries;
        seg->data = my_alloc(seg->size);
    }
}

/****************************************************************************
 *                                                                          *
 * Function: write_debug_information                                        *
 *                                                                          *
 * Purpose : Write debug information to the executable file.                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-12-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

void *write_debug_information(void *ip, void *base, COFF_DBGDIR *dbgdir, COFF_FILHDR *fhdr)
{
    ip = write_misc_debug_info(ip, base, dbgdir++, fhdr);

    if (scn_fpo != NULL)
        ip = write_fpo_debug_info(ip, base, dbgdir++, fhdr);

    if (options.debug_coff)
        ip = write_coff_debug_info(ip, base, dbgdir++, fhdr);

    if (options.debug_cv)
        ip = write_codeview_debug_info(ip, base, dbgdir++, fhdr);

    return ip;
}

/****************************************************************************
 *                                                                          *
 * Function: write_misc_debug_info                                          *
 *                                                                          *
 * Purpose : Write MISC debug information to the executable file.           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-12-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void *write_misc_debug_info(void *ip, void *base, COFF_DBGDIR *dbgdir, COFF_FILHDR *fhdr)
{
    COFF_DBGMISC *dbgmisc;
    const char *fname;
    size_t size;

    UNREFERENCED_PARAMETER(fhdr);

    /*
     * Use MAX_PATH here, since SplitSymbols() will overwrite
     * the image name with the path/name of the DBG file.
     */
    fname = basename(exe_file->name);
    size = sizeof(COFF_DBGMISC) + MAX_PATH;

    /*
     * Write the debug directory entry.
     */
    dbgdir->dd_flags = 0;
    dbgdir->dd_timdat = time_stamp;
    dbgdir->dd_majver = 0;
    dbgdir->dd_minver = 0;
    dbgdir->dd_type = COFF_DTYP_MISC;
    dbgdir->dd_size = size;
    dbgdir->dd_addr = 0;
    dbgdir->dd_dbgptr = ((char *)ip - (char *)base);

    /*
     * Write the debug data.
     */
    dbgmisc = (COFF_DBGMISC *)ip;
    dbgmisc->dm_type = COFF_DM_EXENAME;
    dbgmisc->dm_size = ROUNDUP_DWORD(size);  /* alignment */
    dbgmisc->dm_unicode = FALSE;
    dbgmisc->dm_reserved[0] = dbgmisc->dm_reserved[1] = dbgmisc->dm_reserved[2] = 0;
    memset(dbgmisc->dm_data, 0, MAX_PATH);
    strncpy(dbgmisc->dm_data, fname, MAX_PATH);

    ip = (char *)ip + dbgmisc->dm_size;

    return ip;
}

/****************************************************************************
 *                                                                          *
 * Function: write_fpo_debug_info                                           *
 *                                                                          *
 * Purpose : Write FPO debug information to the executable file.            *
 *                                                                          *
 * Comment : FPO records should be sorted in RVA order, according to        *
 *           Microsoft. The way the linker works, this will be done         *
 *           automatically (we hope).                                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-12-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void *write_fpo_debug_info(void *ip, void *base, COFF_DBGDIR *dbgdir, COFF_FILHDR *fhdr)
{
    SEGENTRY *seg;
    void *ip_base;

    UNREFERENCED_PARAMETER(fhdr);

    /*
     * Write the debug directory entry.
     */
    dbgdir->dd_flags = 0;
    dbgdir->dd_timdat = time_stamp;
    dbgdir->dd_majver = 0;
    dbgdir->dd_minver = 0;
    dbgdir->dd_type = COFF_DTYP_FPO;
    /* dbgdir->dd_size = 0; */
    dbgdir->dd_addr = 0;
    dbgdir->dd_dbgptr = ((char *)ip - (char *)base);

    ip_base = ip;

    /*
     * Write the debug data.
     */
    for (seg = scn_fpo->segs; seg != NULL; seg = seg->next)
    {
        /* Write FPO_DATA records */
        memcpy(ip, seg->data, seg->size);
        ip = (char *)ip + seg->size;
    }

    /* Update size of debug data */
    dbgdir->dd_size = ((char *)ip - (char *)ip_base);

    ip = align_dword_ptr(ip);

    return ip;
}

/****************************************************************************
 *                                                                          *
 * Function: write_coff_debug_info                                          *
 *                                                                          *
 * Purpose : Write COFF debug information to the executable file.           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-12-05  Created                                              *
 *           02-04-04  Use function symbol in linenumber info (bugfix).     *
 *                                                                          *
 ****************************************************************************/

static void *write_coff_debug_info(void *ip, void *base, COFF_DBGDIR *dbgdir, COFF_FILHDR *fhdr)
{
    COFF_OPTHDR *ohdr;
    COFF_DBGCOFF *dbgcoff;
    COFF_SCNHDR *scnhdr;
    COFF_SYMENT *symptr;
    COFF_SYMENT *se;
    COFF_SYMENT *se_file;
    MODENTRY *mod;
    GRPENTRY *grp;
    long symbol_count;
    void *ip_base;

    /*
     * First, calculate the number of symbol table entries.
     */
    symbol_count = 0; mod = NULL;
    for (grp = group_list; grp != NULL; grp = grp->next)
    {
        SCNENTRY *scn;

        for (scn = grp->scngrp; scn != NULL; scn = scn->scngrp)
        {
            SEGENTRY *seg;

            for (seg = scn->segs; seg != NULL; seg = seg->next)
            {
                SYMENTRY *sym;

                if (seg->mod != NULL && seg->mod != mod && seg->mod->src_file != NULL)
                    symbol_count += (1 + number_of_aux_file_entries(seg->mod->src_file->name)), mod = seg->mod;

                for (sym = seg->pubs; sym != NULL; sym = sym->syms)
                    if (IS_DEBUG_SYMBOL(sym)) symbol_count++;

                for (sym = seg->stcs; sym != NULL; sym = sym->syms)
                    if (IS_DEBUG_SYMBOL(sym)) symbol_count++;
            }
        }
    }

    ohdr = (COFF_OPTHDR *)(fhdr + 1);

    /*
     * Write the debug directory entry.
     */
    dbgdir->dd_flags = 0;
    dbgdir->dd_timdat = time_stamp;
    dbgdir->dd_majver = 0;
    dbgdir->dd_minver = 0;
    dbgdir->dd_type = COFF_DTYP_COFF;
    dbgdir->dd_size = 0;
    dbgdir->dd_addr = 0;
    dbgdir->dd_dbgptr = ((char *)ip - (char *)base);

    ip_base = ip;

    /*
     * Write the debug header.
     */
    dbgcoff = (COFF_DBGCOFF *)ip;
    ip = ((COFF_DBGCOFF *)ip + 1);

    dbgcoff->dc_nsyms = symbol_count;
    dbgcoff->dc_symptr = ((char *)ip - (char *)ip_base);
    dbgcoff->dc_nlnno = 0;
    dbgcoff->dc_lnnoptr = 0;
    dbgcoff->dc_text_start = ohdr->o_text_start;
    dbgcoff->dc_text_end = ohdr->o_text_start + ohdr->o_tsize;
    dbgcoff->dc_data_start = ohdr->o_data_start;
    dbgcoff->dc_data_end = ohdr->o_data_start + ohdr->o_dsize;

    /* Update the file header */
    fhdr->f_symptr = ((char *)ip - (char *)base);
    fhdr->f_nsyms = symbol_count;

    /* Reserve room for the symbol table */
    symptr = se = (COFF_SYMENT *)ip;
    ip = ((COFF_SYMENT *)ip + symbol_count);

    /* Initialize the string table */
    strptr = ip;
    *(long *)strptr = 0;
    ip = ((long *)ip + 1);

    /*
     * Write the symbol table.
     */
    se_file = NULL; mod = NULL;
    for (grp = group_list; grp != NULL; grp = grp->next)
    {
        SCNENTRY *scn;

        for (scn = grp->scngrp; scn != NULL; scn = scn->scngrp)
        {
            SEGENTRY *seg;

            for (seg = scn->segs; seg != NULL; seg = seg->next)
            {
                SYMENTRY *sym;

                if (seg->mod != NULL && seg->mod != mod && seg->mod->src_file != NULL)
                {
                    uchar_t num_entries = number_of_aux_file_entries(seg->mod->src_file->name);

                    /* Chain together all ".file" entries */
                    if (se_file != NULL) se_file->n_value = (se - symptr);
                    se_file = se; mod = seg->mod;

                    write_symbol_name(se, ".file", &ip);
                    se->n_value = 0;
                    se->n_scnum = COFF_N_DEBUG;
                    se->n_type = COFF_T_NULL;
                    se->n_sclass = COFF_C_FILE;
                    se->n_numaux = num_entries;
                    se++;

                    strcpy(((COFF_AUXENT *)se)->x_fname, seg->mod->src_file->name);
                    se += num_entries;
                }

                /* Process public symbols defined here... */
                for (sym = seg->pubs; sym != NULL; sym = sym->syms)
                {
                    if (!IS_DEBUG_SYMBOL(sym)) continue;

                    sym->index = (se - symptr);

                    write_symbol_name(se, sym->name, &ip);
                    se->n_value = sym->value;
                    se->n_scnum = SYMBOL_SECTION(sym);
                    se->n_type = SYMBOL_TYPE(sym);
                    se->n_sclass = sym->class;
                    se->n_numaux = 0;
                    se++;
                }

                /* Process static symbols defined here... */
                for (sym = seg->stcs; sym != NULL; sym = sym->syms)
                {
                    if (!IS_DEBUG_SYMBOL(sym)) continue;

                    sym->index = (se - symptr);

                    write_symbol_name(se, sym->name, &ip);
                    se->n_value = sym->value;
                    se->n_scnum = SYMBOL_SECTION(sym);
                    se->n_type = SYMBOL_TYPE(sym);
                    se->n_sclass = sym->class;
                    se->n_numaux = 0;
                    se++;
                }
            }
        }
    }

    /* Update length of the string table */
    *(long *)strptr = ((char *)ip - (char *)strptr);

    ip = align_dword_ptr(ip);

    /*
     * Write linenumber information.
     */
    scnhdr = (COFF_SCNHDR *)(ohdr + 1);
    for (grp = group_list; grp != NULL; grp = grp->next)
    {
        void *ip_grp = ip;
        ushort_t line_count;
        SCNENTRY *scn;

        line_count = 0;
        for (scn = grp->scngrp; scn != NULL; scn = scn->scngrp)
        {
            SEGENTRY *seg;

            for (seg = scn->segs; seg != NULL; seg = seg->next)
            {
                LINENTRY *lines;

                for (lines = seg->lines;
                     lines != NULL && lines < seg->lines + seg->nlines;
                     lines++)
                {
                    COFF_LINENO *le;

                    le = (COFF_LINENO *)ip;
                    ip = ((COFF_LINENO *)ip + 1);

                    line_count++;

                    if (lines->sym != NULL)
                    {
                        le->l_symndx = lines->sym->index;
                        le->l_lnno = 0;
                    }
                    else
                    {
                        le->l_paddr = seg->vaddr + lines->vaddr;
                        le->l_lnno = lines->lineno;
                    }
                }
            }
        }

        if (line_count != 0)
        {
            scnhdr->s_lnnoptr = ((char *)ip_grp - (char *)base);
            scnhdr->s_nlnno = line_count;

            dbgcoff->dc_nlnno += line_count;

            if (dbgcoff->dc_lnnoptr == 0)
                dbgcoff->dc_lnnoptr = ((char *)ip_grp - (char *)ip_base);
        }
    }

    /* Update size of debug data */
    dbgdir->dd_size = ((char *)ip - (char *)ip_base);

    ip = align_dword_ptr(ip);

    return ip;
}

/****************************************************************************
 *                                                                          *
 * Function: number_of_aux_file_entries                                     *
 *                                                                          *
 * Purpose : Return number of *auxiliary* symbol entries required to        *
 *           hold the given filename.                                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-12-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

static __inline uchar_t number_of_aux_file_entries(char *name)
{
    size_t len = strlen(name)+1;  /* remember '\0' */
    return (len + COFF_SYMESZ - 1) / COFF_SYMESZ;
}

/****************************************************************************
 *                                                                          *
 * Function: write_symbol_name                                              *
 *                                                                          *
 * Purpose : Write the name of a symbol table entry.  see export.c          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-12-05  Created                                              *
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
 * Function: write_codeview_debug_info                                      *
 *                                                                          *
 * Purpose : Write CodeView debug information to the executable file.       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-12-05  Created                                              *
 *           02-04-04  Use function symbol in linenumber info (bugfix).     *
 *           03-04-26  Wrong vaddr for linenumbers (bugfix).                *
 *           03-06-15  Modified to emit packed info (NB11) directly.        *
 *           03-07-20  Bugfix: sstFileIndex information wrong.              *
 *           03-07-20  Bugfix: added call to group_types().                 *
 *                                                                          *
 ****************************************************************************/

static void *write_codeview_debug_info(void *ip, void *base, COFF_DBGDIR *dbgdir, COFF_FILHDR *fhdr)
{
    CODEVIEW_SIGNATURE *cvs;
    CODEVIEW_DIRECTORY_HEADER *cvdh;
    CODEVIEW_DIRECTORY_ENTRY *cvde;
    FILEINFO *lib_file;
    MODENTRY *mod;
    GRPENTRY *grp;
    SEGENTRY *seg;
    void *ip_base;
    long total_count;
    bool_t ref;

    UNREFERENCED_PARAMETER(fhdr);

    /*
     * Calculate the number of subsections.
     */
    total_count = 2;  /* sstGlobalPub + sstSegMap */

    ref = FALSE;
    for (mod = module_list; mod != NULL; mod = mod->next)
    {
        total_count++;  /* sstModule */

        if (number_of_linenumber_segments(mod) != 0)
            ref = TRUE, total_count++;  /* sstSrcModule */
    }

    if (ref) total_count++;  /* sstFileIndex */

    if (scn_type != NULL)
    {
        total_count++;  /* sstGlobalTypes */

        mod = NULL;
        for (seg = scn_type->segs; seg != NULL; seg = seg->next)
        {
            if (seg->mod != mod)
            {
                /* remove type version */
                if (seg->size >= sizeof(long) && *(long *)seg->data == CODEVIEW_TYPES_VERSION)
                {
                    seg->size -= sizeof(long);
                    memmove(seg->data, (char *)seg->data + sizeof(long), seg->size);
                }

                mod = seg->mod;
            }
        }
    }

    if (scn_sym != NULL)
    {
        mod = NULL;
        for (seg = scn_sym->segs; seg != NULL; seg = seg->next)
        {
            if (seg->mod != mod)
            {
                total_count++;  /* sstAlignSym */

                /* remove symbol version */
                if (seg->size >= sizeof(long) && *(long *)seg->data == CODEVIEW_SYMBOLS_VERSION)
                {
                    seg->size -= sizeof(long);
                    memmove(seg->data, (char *)seg->data + sizeof(long), seg->size);
                }

                mod = seg->mod;
            }
        }
    }

    if (lib_file_list != NULL)
        total_count++;  /* sstLibraries */

    /*
     * Write debug directory entry.
     */
    dbgdir->dd_flags = 0;
    dbgdir->dd_timdat = time_stamp;
    dbgdir->dd_majver = 0;
    dbgdir->dd_minver = 0;
    dbgdir->dd_type = COFF_DTYP_CODEVIEW;
    dbgdir->dd_size = 0;
    dbgdir->dd_addr = 0;
    dbgdir->dd_dbgptr = ((char *)ip - (char *)base);

    ip_base = ip;

    /*
     * Write debug header.
     */
    cvs = (CODEVIEW_SIGNATURE *)ip;
    ip = ((CODEVIEW_SIGNATURE *)ip + 1);

    memcpy(cvs->signature, CODEVIEW_SIGNATURE_NB11, 4);
    cvs->offset = ((char *)ip - (char *)ip_base);

    cvdh = (CODEVIEW_DIRECTORY_HEADER *)ip;
    ip = ((CODEVIEW_DIRECTORY_HEADER *)ip + 1);

    cvdh->sizeof_dirhdr = sizeof(*cvdh);
    cvdh->sizeof_dirent = sizeof(*cvde);
    cvdh->ndirents = total_count;
    cvdh->offset_nextdir = 0;
    cvdh->flags = 0;

    cvde = (CODEVIEW_DIRECTORY_ENTRY *)ip;
    ip = ((CODEVIEW_DIRECTORY_ENTRY *)ip + total_count);

    /*
     * Write sstModule records.
     */
    for (mod = module_list; mod != NULL; mod = mod->next)
    {
        CODEVIEW_MODULE *cvm;
        void *ip_start = ip;

        cvde->subsect_type = CODEVIEW_SUBSECTION_MODULE;
        cvde->modindex = mod->modnum;
        cvde->offset_subsect = ((char *)ip - (char *)ip_base);

        cvm = (CODEVIEW_MODULE *)ip;
        cvm->overlay_number = 0;
        cvm->libindex = (mod->lib_file) ? library_index(mod) : 0;
        cvm->nsegs = 0;
        memcpy(cvm->style, CODEVIEW_MODULE_DEBUG_STYLE, 2);
        ip = (void *)cvm->segment;

        for (grp = group_list; grp != NULL; grp = grp->next)
        {
            SCNENTRY *scn;

            for (scn = grp->scngrp; scn != NULL; scn = scn->scngrp)
            {
                SEGENTRY *seg;

                for (seg = scn->segs; seg != NULL; seg = seg->next)
                {
                    CODEVIEW_SEGMENT *cvs;

                    if (seg->mod != mod) continue;

                    cvs = (CODEVIEW_SEGMENT *)ip;
                    ip = ((CODEVIEW_SEGMENT *)ip + 1);

                    cvs->segindex = grp->scnum;
                    cvs->pad = 0;
                    cvs->offset_code = seg->vaddr - grp->vaddr;
                    cvs->sizeof_code = seg->size;

                    cvm->nsegs++;
                }
            }
        }

        /* Write the module filename */
        ip = write_length_prefixed_name(ip, mod->obj_file->name);

        cvde->sizeof_subsect = ((char *)ip - (char *)ip_start);
        cvde++;

        ip = align_dword_ptr(ip);
    }

    /*
     * Compress global types, modify symbols.
     */
    group_types();
    compact_types();

    /*
     * Write sstAlignSym records.
     */
    if (scn_sym != NULL)
    {
        CODEVIEW_SYMBOL_SSEARCH *cvss = NULL;
        CODEVIEW_SYMBOL_GPROC32_16T *cvsgp = NULL;
        void *ip_start = NULL;

        mod = NULL;
        for (seg = scn_sym->segs; seg != NULL; seg = seg->next)
        {
            void *sp;

            if (seg->mod != mod)
            {
                cvde->subsect_type = CODEVIEW_SUBSECTION_ALIGNSYM;
                cvde->modindex = seg->mod->modnum;
                cvde->offset_subsect = ((char *)ip - (char *)ip_base);

                mod = seg->mod;
                ip_start = ip;

                *(long *)ip = CODEVIEW_PUBLIC_SYMBOL_VERSION;
                ip = ((long *)ip + 1);

                /* write search record */
                cvss = (CODEVIEW_SYMBOL_SSEARCH *)ip;
                cvss->length = ROUNDUP_DWORD(sizeof(*cvss)) - sizeof(cvss->length);
                cvss->type = CODEVIEW_SYMTYPE_SSEARCH;
                cvss->symoffset = 0;
                cvss->segment = 0;

                ip = align_dword_ptr((char *)ip + sizeof(*cvss));
            }

            sp = seg->data;
            while (sp < (void *)((char *)seg->data + seg->size))
            {
                CODEVIEW_SYMBOL_GENERIC *cvsg = (CODEVIEW_SYMBOL_GENERIC *)sp;
                long length = sizeof(cvsg->length) + cvsg->length;

                memcpy(ip, sp, length);
                sp = ((char *)sp + length);

                switch (cvsg->type)
                {
                    case CODEVIEW_SYMTYPE_LPROC32_16T:
                    case CODEVIEW_SYMTYPE_GPROC32_16T:
                    {
                        /* update next-procedure pointer */
                        if (cvsgp != NULL)
                            cvsgp->nextptr = (char *)ip - (char *)ip_start;

                        cvsgp = (CODEVIEW_SYMBOL_GPROC32_16T *)ip;

                        /* update first-procedure pointer */
                        if (cvss->symoffset == 0)
                        {
                            cvss->symoffset = (char *)ip - (char *)ip_start;
                            cvss->segment = cvsgp->segment;
                        }
                        break;
                    }

                    case CODEVIEW_SYMTYPE_END:
                    {
                        /* update end-of-procedure pointer */
                        if (cvsgp != NULL)
                            cvsgp->endptr = (char *)ip - (char *)ip_start;
                        break;
                    }
                }

                cvsg = (CODEVIEW_SYMBOL_GENERIC *)ip;
                ip = align_dword_ptr((char *)ip + length);
                cvsg->length = ROUNDUP_DWORD(length) - sizeof(cvsg->length);
            }

            if (seg->next == NULL || seg->next->mod != mod)
            {
                cvde->sizeof_subsect = ((char *)ip - (char *)ip_start);
                cvde++;

                ip = align_dword_ptr(ip);
            }
        }
    }

    /*
     * Write sstSrcModule records.
     */
    for (mod = module_list; mod != NULL; mod = mod->next)
    {
        CODEVIEW_SOURCEMODULE *cvsm;
        CODEVIEW_START_END *cvsem;
        CODEVIEW_SOURCEFILE *cvsf;
        CODEVIEW_START_END *cvsef;
        void *ip_start = ip;
        short segidx;
        short *cvseg;

        if (mod->src_file == NULL)
            continue;

        segidx = number_of_linenumber_segments(mod);
        if (segidx == 0) continue;

        cvde->subsect_type = CODEVIEW_SUBSECTION_SRCMODULE;
        cvde->modindex = mod->modnum;
        cvde->offset_subsect = ((char *)ip - (char *)ip_base);

        cvsm = (CODEVIEW_SOURCEMODULE *)ip;
        cvsm->nfiles = 1;
        cvsm->nsegs = segidx;
        cvsem = (CODEVIEW_START_END *)&cvsm->base_source_file[1];
        cvseg = (short *)&cvsem[segidx];
        ip = (void *)&cvseg[segidx];

        ip = align_dword_ptr(ip);

        cvsm->base_source_file[0] = ((char *)ip - (char *)ip_start);
        cvsf = (CODEVIEW_SOURCEFILE *)ip;
        cvsf->nsegs = segidx;
        cvsf->pad = 0;
        cvsef = (CODEVIEW_START_END *)&cvsf->base_source_line[segidx];
        ip = (void *)&cvsef[segidx];

        /* Write the source filename */
        ip = write_length_prefixed_name(ip, mod->src_file->name);

        ip = align_dword_ptr(ip);

        segidx = 0;
        for (grp = group_list; grp != NULL; grp = grp->next)
        {
            SCNENTRY *scn;

            for (scn = grp->scngrp; scn != NULL; scn = scn->scngrp)
            {
                SEGENTRY *seg;

                for (seg = scn->segs; seg != NULL; seg = seg->next)
                {
                    CODEVIEW_SOURCELINE *cvsl;
                    LINENTRY *lines;
                    short linidx;
                    short *linnum;
                    ushort_t fcnline;

                    if (seg->mod != mod || seg->lines == NULL)
                        continue;

                    for (lines = seg->lines, linidx = 0;
                         lines != NULL && lines < seg->lines + seg->nlines;
                         lines++, linidx++)
                        ;

                    cvsf->base_source_line[segidx] = ((char *)ip - (char *)ip_start);
                    cvsl = (CODEVIEW_SOURCELINE *)ip;
                    cvsl->segindex = grp->scnum;
                    cvsl->nlines = linidx;
                    linnum = (short *)&cvsl->offset[linidx];
                    ip = (void *)&linnum[linidx];

                    ip = align_dword_ptr(ip);

                    fcnline = 0;
                    for (lines = seg->lines, linidx = 0;
                         lines != NULL && lines < seg->lines + seg->nlines;
                         lines++, linidx++)
                    {
                        if (lines->sym != NULL)  /* function symbol */
                        {
                            cvsl->offset[linidx] = lines->sym->value - grp->vaddr;
                            linnum[linidx] = fcnline = lines->lineno;
                        }
                        else
                        {
                            cvsl->offset[linidx] = lines->vaddr + seg->vaddr - grp->vaddr;  /* 03-04-26 */
                            linnum[linidx] = fcnline + lines->lineno;

                        }
                    }

                    cvsem[segidx].offset_start = cvsef[segidx].offset_start = seg->vaddr - grp->vaddr;
                    cvsem[segidx].offset_end = cvsef[segidx].offset_end = seg->vaddr - grp->vaddr + seg->size-1;
                    cvseg[segidx] = grp->scnum;
                    segidx++;
                }
            }
        }

        cvde->sizeof_subsect = ((char *)ip - (char *)ip_start);
        cvde++;
    }

    /*
     * Write sstGlobalPub record.
     */
    {
        CODEVIEW_SYMBOL_HASH *cvsh;
        void *ip_start = ip;
        void *ip_syms;

        cvde->subsect_type = CODEVIEW_SUBSECTION_GLOBALPUB;
        cvde->modindex = CODEVIEW_MODULE_INDEX_NONE;
        cvde->offset_subsect = ((char *)ip - (char *)ip_base);

        cvsh = (CODEVIEW_SYMBOL_HASH *)ip;
        ip = ((CODEVIEW_SYMBOL_HASH *)ip + 1);

        cvsh->sym_hash_index = 0;  /* no hash function */
        cvsh->addr_hash_index = 0;  /* no hash function */
        /* cvsh->sizeof_syminfo = 0; */
        cvsh->sizeof_sym_hash_data = 0;
        cvsh->sizeof_addr_hash_data = 0;

        ip_syms = ip;

        for (mod = module_list; mod != NULL; mod = mod->next)
        {
            for (grp = group_list; grp != NULL; grp = grp->next)
            {
                SCNENTRY *scn;

                for (scn = grp->scngrp; scn != NULL; scn = scn->scngrp)
                {
                    SEGENTRY *seg;

                    for (seg = scn->segs; seg != NULL; seg = seg->next)
                    {
                        SYMENTRY *pub;

                        if (seg->mod != mod) continue;

                        for (pub = seg->pubs; pub != NULL; pub = pub->syms)
                        {
                            CODEVIEW_SYMBOL_PUB32_16T *cvsp;
                            int length;

                            if (pub->flags.internal || pub->flags.absolute)
                                continue;

                            length = sizeof(*cvsp) + strlen(pub->name);

                            cvsp = (CODEVIEW_SYMBOL_PUB32_16T *)ip;
                            cvsp->length = ROUNDUP_DWORD(length) - sizeof(cvsp->length);
                            cvsp->type = CODEVIEW_SYMTYPE_PUB32_16T;
                            cvsp->type_index = CODEVIEW_TYPE_NOTYPE;  /* MS doesn't seem to bother */
                            cvsp->pad = 0;
                            cvsp->offset = pub->value - grp->vaddr;
                            cvsp->segment = pub->scnum;
                            write_length_prefixed_name(cvsp->name, pub->name);

                            ip = align_dword_ptr((char *)ip + length);
                        }
                    }
                }
            }
        }

        cvsh->sizeof_syminfo = (char *)ip - (char *)ip_syms;

        cvde->sizeof_subsect = ((char *)ip - (char *)ip_start);
        cvde++;
    }

    // ? sstGlobalSym
    // ? sstStaticSym

    /*
     * Write sstLibraries record.
     */
    if (lib_file_list != NULL)
    {
        void *ip_start = ip;

        cvde->subsect_type = CODEVIEW_SUBSECTION_LIBRARIES;
        cvde->modindex = (short)CODEVIEW_MODULE_INDEX_NONE;
        cvde->offset_subsect = ((char *)ip - (char *)ip_base);

        *(char *)ip = 0;  /* reserve index 0 */
        ip = ((char *)ip + 1);

        for (lib_file = lib_file_list;
             lib_file != NULL;
             lib_file = lib_file->next)
        {
            ip = write_length_prefixed_name(ip, lib_file->name);
        }

        cvde->sizeof_subsect = ((char *)ip - (char *)ip_start);
        cvde++;

        ip = align_dword_ptr(ip);
    }

    /*
     * Write sstGlobalTypes record.
     */
    if (scn_type != NULL)
    {
        CODEVIEW_GLOBAL_TYPES *cvgt;
        void *ip_start = ip;
        long offset;

        cvde->subsect_type = CODEVIEW_SUBSECTION_GLOBALTYPES;
        cvde->modindex = CODEVIEW_MODULE_INDEX_NONE;
        cvde->offset_subsect = ((char *)ip - (char *)ip_base);

        cvgt = (CODEVIEW_GLOBAL_TYPES *)ip;
        ip = ((CODEVIEW_GLOBAL_TYPES *)ip + 1);

        cvgt->flags.sig = CODEVIEW_TYPES_VERSION;
        cvgt->flags.unused = 0;
        cvgt->ntypes = 0;

        offset = 0;
        for (seg = scn_type->segs; seg != NULL; seg = seg->next)
        {
            char *tp = seg->data;

            while (tp < (char *)seg->data + seg->size)
            {
                CODEVIEW_TYPE_RECORD *cvtr = (CODEVIEW_TYPE_RECORD *)tp;

                cvgt->offset[cvgt->ntypes++] = offset;
                offset += sizeof(cvtr->length) + cvtr->length;
                tp += sizeof(cvtr->length) + cvtr->length;
            }
        }

        ip = &cvgt->offset[cvgt->ntypes];

        for (seg = scn_type->segs; seg != NULL; seg = seg->next)
        {
            memcpy(ip, seg->data, seg->size);
            ip = (char *)ip + seg->size;
        }

        cvde->sizeof_subsect = ((char *)ip - (char *)ip_start);
        cvde++;

        ip = align_dword_ptr(ip);
    }

    /*
     * Write sstSegMap record.
     */
    {
        CODEVIEW_SEGMENT_MAP *cvsm;
        void *ip_start = ip;
        short segidx;

        cvde->subsect_type = CODEVIEW_SUBSECTION_SEGMAP;
        cvde->modindex = CODEVIEW_MODULE_INDEX_NONE;
        cvde->offset_subsect = ((char *)ip - (char *)ip_base);

        segidx = 1;
        for (grp = group_list; grp != NULL; grp = grp->next)
            segidx++;

        cvsm = (CODEVIEW_SEGMENT_MAP *)ip;
        cvsm->nsegs = segidx;
        cvsm->nlogsegs = segidx;
        ip = &cvsm->segment[segidx];

        segidx = 0;
        for (grp = group_list; grp != NULL; grp = grp->next)
        {
            long length = 0;
            SCNENTRY *scn;

            for (scn = grp->scngrp; scn != NULL; scn = scn->scngrp)
            {
                if (scn->segs != NULL)
                {
                    SEGENTRY *seg;
                    addr_t vaddr;

                    seg = scn->segs;
                    vaddr = seg->vaddr;

                    while (seg->next != NULL)
                        seg = seg->next;

                    length += (seg->vaddr + seg->size) - vaddr;
                }
            }

            cvsm->segment[segidx].read = (grp->flags & COFF_STYP_READ) != 0;
            cvsm->segment[segidx].write = (grp->flags & COFF_STYP_WRITE) != 0;
            cvsm->segment[segidx].execute = (grp->flags & COFF_STYP_EXEC) != 0;
            cvsm->segment[segidx].bits32 = TRUE;
            cvsm->segment[segidx].reserved1 = 0;
            cvsm->segment[segidx].selector = TRUE;
            cvsm->segment[segidx].absolute = 0;
            cvsm->segment[segidx].reserved2 = 0;
            cvsm->segment[segidx].group = 0;
            cvsm->segment[segidx].reserved3 = 0;
            cvsm->segment[segidx].overlay_number = 0;
            cvsm->segment[segidx].group_number = 0;
            cvsm->segment[segidx].frame = grp->scnum;
            cvsm->segment[segidx].segname_index = (short)0xFFFFU;
            cvsm->segment[segidx].clsname_index = (short)0xFFFFU;
            cvsm->segment[segidx].offset_segment = 0;
            cvsm->segment[segidx].sizeof_segment = length;
            segidx++;
        }

        cvsm->segment[segidx].read = 0;
        cvsm->segment[segidx].write = 0;
        cvsm->segment[segidx].execute = 0;
        cvsm->segment[segidx].bits32 = TRUE;
        cvsm->segment[segidx].reserved1 = 0;
        cvsm->segment[segidx].selector = 0;
        cvsm->segment[segidx].absolute = TRUE;
        cvsm->segment[segidx].reserved2 = 0;
        cvsm->segment[segidx].group = 0;
        cvsm->segment[segidx].reserved3 = 0;
        cvsm->segment[segidx].overlay_number = 0;
        cvsm->segment[segidx].group_number = 0;
        cvsm->segment[segidx].frame = 0;
        cvsm->segment[segidx].segname_index = (short)0xFFFFU;
        cvsm->segment[segidx].clsname_index = (short)0xFFFFU;
        cvsm->segment[segidx].offset_segment = 0;
        cvsm->segment[segidx].sizeof_segment = 0xFFFFFFFF;

        cvde->sizeof_subsect = ((char *)ip - (char *)ip_start);
        cvde++;
    }

    /*
     * Write sstFileIndex record.
     */
    {
        long modcnt;
        long refcnt;

        modcnt = refcnt = 0;
        for (mod = module_list; mod != NULL; mod = mod->next)
        {
            modcnt++;

            if (mod->src_file != NULL && number_of_linenumber_segments(mod) != 0)
                refcnt++;
        }

        if (refcnt > 0)
        {
            CODEVIEW_FILEINDEX *cvfi;
            void *ip_start = ip;
            short *indexes;
            short *refcnts;
            long *offsets;
            char *names;

            cvde->subsect_type = CODEVIEW_SUBSECTION_FILEINDEX;
            cvde->modindex = CODEVIEW_MODULE_INDEX_NONE;
            cvde->offset_subsect = ((char *)ip - (char *)ip_base);

            cvfi = (CODEVIEW_FILEINDEX *)ip;
            cvfi->nfiles = modcnt;
            cvfi->nrefs = refcnt;

            indexes = (short *)cvfi->name_indexes;
            refcnts = (short *)&indexes[modcnt];
            offsets = (long *)&refcnts[modcnt];
            names = (char *)&offsets[refcnt];
            ip = (void *)names;

            modcnt = refcnt = 0;
            for (mod = module_list; mod != NULL; mod = mod->next)
            {
                /* Source file that contributes to the modules code? */
                if (mod->src_file != NULL && number_of_linenumber_segments(mod) != 0)
                {
                    indexes[modcnt] = refcnt;
                    refcnts[modcnt] = 1;
                    offsets[refcnt] = ((char *)ip - (char *)names);  /* bugfix 03-07-20 */

                    /* Write the source filename */
                    ip = write_length_prefixed_name(ip, mod->src_file->name);

                    refcnt++;
                }
                else
                {
                    indexes[modcnt] = 0;
                    refcnts[modcnt] = 0;
                }

                modcnt++;
            }

            ip = align_dword_ptr(ip);

            cvde->sizeof_subsect = ((char *)ip - (char *)ip_start);
            cvde++;
        }
    }

    /* Update size of debug data */
    dbgdir->dd_size = ((char *)ip - (char *)ip_base);

    return ip;
}

/****************************************************************************
 *                                                                          *
 * Function: number_of_linenumber_segments                                  *
 *                                                                          *
 * Purpose : Return number of segments containing linenumber info.          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-12-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

static short number_of_linenumber_segments(MODENTRY *mod)
{
    GRPENTRY *grp;
    short numseg;

    numseg = 0;
    for (grp = group_list; grp != NULL; grp = grp->next)
    {
        SCNENTRY *scn;

        for (scn = grp->scngrp; scn != NULL; scn = scn->scngrp)
        {
            SEGENTRY *seg;

            for (seg = scn->segs; seg != NULL; seg = seg->next)
                if (seg->mod == mod && seg->lines) numseg++;
        }
    }

    return numseg;
}

/****************************************************************************
 *                                                                          *
 * Function: write_length_prefixed_name                                     *
 *                                                                          *
 * Purpose : Write a length-prefixed CodeView name.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-12-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

static __inline void *write_length_prefixed_name(uchar_t *ip, char *name)
{
    *ip++ = strlen(name);
    memcpy(ip, name, ip[-1]);
    return (ip + ip[-1]);
}

/****************************************************************************
 *                                                                          *
 * Function: library_index                                                  *
 *                                                                          *
 * Purpose : Return CodeView library index for the given module.            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-12-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

static short library_index(MODENTRY *mod)
{
    FILEINFO *lib_file;
    short index;

    for (lib_file = lib_file_list, index = 1;
         lib_file != NULL;
         lib_file = lib_file->next, index++)
    {
        if (lib_file == mod->lib_file)
            return index;
    }

    return 0;
}

/****************************************************************************
 *                                                                          *
 * Function: group_types                                                    *
 *                                                                          *
 * Purpose : Group CodeView types from all modules.                         *
 *                                                                          *
 * Comment : We assume one segment per module, but this should be OK!       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           03-07-20  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void group_types(void)
{
    if (scn_type != NULL)
    {
        cv_typ16_t index = 0x1000;
        SEGENTRY *seg;

        for (seg = scn_type->segs; seg != NULL; seg = seg->next)
        {
            void *tpend = (char *)seg->data + seg->size;
            void *tp = seg->data;
            cv_typ16_t seg_index = 0x1000;

            while (tp < tpend)
            {
                CODEVIEW_TYPE_RECORD *cvtr = (CODEVIEW_TYPE_RECORD *)tp;

                tp = (char *)tp + sizeof(cvtr->length) + cvtr->length;

                if (index != seg_index)
                {
                    // Change module relative index into executable index.
                    relocate_symbol_type(seg->mod, index, seg_index);
                    relocate_type_type(seg->mod, index, seg_index);
                }

                seg_index++;
                index++;
            }
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: compact_types                                                  *
 *                                                                          *
 * Purpose : Compact CodeView types (much like CVPACK).                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           03-06-15  Created                                              *
 *           03-07-20  Bugfix: several problems with duplicates.            *
 *                                                                          *
 ****************************************************************************/

static void compact_types(void)
{
    if (scn_type != NULL)
    {
        cv_typ16_t index = 0x1000;
        SEGENTRY *seg;

        for (seg = scn_type->segs; seg != NULL; seg = seg->next)
        {
            void *tpend = (char *)seg->data + seg->size;
            void *tp = seg->data;

            while (tp < tpend)
            {
                CODEVIEW_TYPE_RECORD *cvtr1 = (CODEVIEW_TYPE_RECORD *)tp;
                size_t length1 = sizeof(cvtr1->length) + cvtr1->length;

                tp = (char *)tp + length1;

                /*
                 * Handle forward referenced structure.
                 */
                if (cvtr1->leaf_type == CODEVIEW_LEAFTYPE_STRUCTURE)
                {
                    CODEVIEW_TYPELEAF_STRUCTURE32 *cvtl1 = (CODEVIEW_TYPELEAF_STRUCTURE32 *)cvtr1;

                    /* Forward reference? */
                    if (cvtl1->propattr.fwdref)
                    {
                        cv_typ16_t index2 = index + 1;
                        void *fp = tp;

                        while (fp < tpend)
                        {
                            CODEVIEW_TYPE_RECORD *cvtr2 = (CODEVIEW_TYPE_RECORD *)fp;
                            size_t length2 = sizeof(cvtr2->length) + cvtr2->length;

                            fp = (char *)fp + length2;

                            if (cvtr2->leaf_type == CODEVIEW_LEAFTYPE_STRUCTURE)
                            {
                                CODEVIEW_TYPELEAF_STRUCTURE32 *cvtl2 = (CODEVIEW_TYPELEAF_STRUCTURE32 *)cvtr2;

                                /* Definition of forward reference? */
                                if (!cvtl2->propattr.fwdref)
                                {
                                    if (memcmp(skip_value(cvtl1->variant), skip_value(cvtl2->variant),
                                        skip_value(cvtl2->variant)[0] + 1) == 0)
                                    {
                                        memcpy(cvtr1, cvtr2, length1);

                                        memmove(cvtr2, fp, ((char *)tpend - (char *)fp));
                                        tpend = (char *)tpend - length2;
                                        seg->size -= length2;

                                        relocate_symbol_type(NULL, index, index2);
                                        relocate_type_type(NULL, index, index2);
                                        break;
                                    }
                                }
                            }

                            index2++;
                        }
                    }
                }
                /*
                 * Handle forward referenced union.
                 */
                else if (cvtr1->leaf_type == CODEVIEW_LEAFTYPE_UNION)
                {
                    CODEVIEW_TYPELEAF_UNION32 *cvtl1 = (CODEVIEW_TYPELEAF_UNION32 *)cvtr1;

                    /* Forward reference? */
                    if (cvtl1->propattr.fwdref)
                    {
                        cv_typ16_t index2 = index + 1;
                        void *fp = tp;

                        while (fp < tpend)
                        {
                            CODEVIEW_TYPE_RECORD *cvtr2 = (CODEVIEW_TYPE_RECORD *)fp;
                            size_t length2 = sizeof(cvtr2->length) + cvtr2->length;

                            fp = (char *)fp + length2;

                            if (cvtr2->leaf_type == CODEVIEW_LEAFTYPE_UNION)
                            {
                                CODEVIEW_TYPELEAF_UNION32 *cvtl2 = (CODEVIEW_TYPELEAF_UNION32 *)cvtr2;

                                /* Definition of forward reference? */
                                if (!cvtl2->propattr.fwdref)
                                {
                                    if (memcmp(skip_value(cvtl1->variant), skip_value(cvtl2->variant),
                                        skip_value(cvtl2->variant)[0] + 1) == 0)
                                    {
                                        memcpy(cvtr1, cvtr2, length1);

                                        memmove(cvtr2, fp, ((char *)tpend - (char *)fp));
                                        tpend = (char *)tpend - length2;
                                        seg->size -= length2;

                                        relocate_symbol_type(NULL, index, index2);
                                        relocate_type_type(NULL, index, index2);
                                        break;
                                    }
                                }
                            }

                            index2++;
                        }
                    }
                }

                /*
                 * Handle duplicates (slow!).
                 */
                {
                    cv_typ16_t index2 = 0x1000;
                    SEGENTRY *seg2;

                    for (seg2 = scn_type->segs; seg2 != NULL; seg2 = seg2->next)
                    {
                        void *fpend = (seg2 != seg) ? (char *)seg2->data + seg2->size : (char *)tp - length1;
                        void *fp = seg2->data;

                        while (fp < fpend)
                        {
                            CODEVIEW_TYPE_RECORD *cvtr2 = (CODEVIEW_TYPE_RECORD *)fp;
                            size_t length2 = sizeof(cvtr2->length) + cvtr2->length;

                            fp = (char *)fp + length2;

                            /* Duplicate definition? */
                            if (length1 == length2 && memcmp(cvtr1, cvtr2, length2) == 0)
                            {
                                memmove(cvtr1, tp, ((char *)tpend - (char *)tp));
                                tpend = (char *)tpend - length1;
                                seg->size -= length1;

                                relocate_symbol_type(NULL, index2, index);
                                relocate_type_type(NULL, index2, index);

                                /*
                                 * Bugfix: we have just removed the record that contained length1!
                                 * Make sure to read a new length, and don't bump index!
                                 */
                                tp = (char *)tp - length1;
                                goto skipdup;
                            }

                            index2++;
                        }

                        if (seg2 == seg)
                            break;
                    }
                }

                index++;
skipdup: ;
            }
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: relocate_symbol_type                                           *
 *                                                                          *
 * Purpose : Change all references to a type index into another one.        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           03-06-15  Created                                              *
 *           03-07-20  Added mod argument to support group_types().         *
 *                                                                          *
 ****************************************************************************/

static void relocate_symbol_type(MODENTRY *mod, cv_typ16_t new_index, cv_typ16_t old_index)
{
    if (scn_sym != NULL)
    {
        SEGENTRY *seg;

        for (seg = scn_sym->segs; seg != NULL; seg = seg->next)
        {
            void *spend = (char *)seg->data + seg->size;
            void *sp = seg->data;

            if (mod != NULL && mod != seg->mod)
                continue;

            while (sp < spend)
            {
                CODEVIEW_SYMBOL_GENERIC *cvsg = (CODEVIEW_SYMBOL_GENERIC *)sp;

                sp = (char *)sp + sizeof(cvsg->length) + cvsg->length;

                switch (cvsg->type)
                {
                    case CODEVIEW_SYMTYPE_REGISTER_16T:
                    {
                        CODEVIEW_SYMBOL_REGISTER_16T *cvst = (CODEVIEW_SYMBOL_REGISTER_16T *)cvsg;

                        if (cvst->type_index == old_index)
                            cvst->type_index = new_index;
                        else if (mod == NULL && cvst->type_index > old_index)
                            cvst->type_index--;
                        break;
                    }

                    case CODEVIEW_SYMTYPE_UDT_16T:
                    {
                        CODEVIEW_SYMBOL_UDT_16T *cvst = (CODEVIEW_SYMBOL_UDT_16T *)cvsg;

                        if (cvst->type_index == old_index)
                            cvst->type_index = new_index;
                        else if (mod == NULL && cvst->type_index > old_index)
                            cvst->type_index--;
                        break;
                    }

                    case CODEVIEW_SYMTYPE_BPREL32_16T:
                    {
                        CODEVIEW_SYMBOL_BPREL32_16T *cvst = (CODEVIEW_SYMBOL_BPREL32_16T *)cvsg;

                        if (cvst->type_index == old_index)
                            cvst->type_index = new_index;
                        else if (mod == NULL && cvst->type_index > old_index)
                            cvst->type_index--;
                        break;
                    }

                    case CODEVIEW_SYMTYPE_LDATA32_16T:
                    case CODEVIEW_SYMTYPE_GDATA32_16T:
                    case CODEVIEW_SYMTYPE_PUB32_16T:
                    {
                        CODEVIEW_SYMBOL_LDATA32_16T *cvst = (CODEVIEW_SYMBOL_LDATA32_16T *)cvsg;

                        if (cvst->type_index == old_index)
                            cvst->type_index = new_index;
                        else if (mod == NULL && cvst->type_index > old_index)
                            cvst->type_index--;
                        break;
                    }

                    case CODEVIEW_SYMTYPE_LPROC32_16T:
                    case CODEVIEW_SYMTYPE_GPROC32_16T:
                    {
                        CODEVIEW_SYMBOL_LPROC32_16T *cvst = (CODEVIEW_SYMBOL_LPROC32_16T *)cvsg;

                        if (cvst->type_index == old_index)
                            cvst->type_index = new_index;
                        else if (mod == NULL && cvst->type_index > old_index)
                            cvst->type_index--;
                        break;
                    }

                    case CODEVIEW_SYMTYPE_CONSTANT_16T:  /* MS HtmlHelp */
                    {
                        CODEVIEW_SYMBOL_CONSTANT_16T *cvst = (CODEVIEW_SYMBOL_CONSTANT_16T *)cvsg;

                        if (cvst->type_index == old_index)
                            cvst->type_index = new_index;
                        else if (mod == NULL && cvst->type_index > old_index)
                            cvst->type_index--;
                        break;
                    }

                    default:
                        break;
                }
            }
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: relocate_type_type                                             *
 *                                                                          *
 * Purpose : Change all references to a type index into another one.        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           03-06-15  Created                                              *
 *           03-07-20  Added mod argument to support group_types().         *
 *                                                                          *
 ****************************************************************************/

static void relocate_type_type(MODENTRY *mod, cv_typ16_t new_index, cv_typ16_t old_index)
{
    if (scn_type != NULL)
    {
        SEGENTRY *seg;

        for (seg = scn_type->segs; seg != NULL; seg = seg->next)
        {
            void *tpend = (char *)seg->data + seg->size;
            void *tp = seg->data;

            if (mod != NULL && mod != seg->mod)
                continue;

            while (tp < tpend)
            {
                CODEVIEW_TYPE_RECORD *cvtr = (CODEVIEW_TYPE_RECORD *)tp;

                tp = (char *)tp + sizeof(cvtr->length) + cvtr->length;

                switch (cvtr->leaf_type)
                {
                    case CODEVIEW_LEAFTYPE_POINTER:
                    {
                        CODEVIEW_TYPELEAF_POINTER32 *cvtl = (CODEVIEW_TYPELEAF_POINTER32 *)cvtr;

                        if (cvtl->type_index == old_index)
                            cvtl->type_index = new_index;
                        else if (mod == NULL && cvtl->type_index > old_index)
                            cvtl->type_index--;
                        break;
                    }

                    case CODEVIEW_LEAFTYPE_ARRAY:
                    {
                        CODEVIEW_TYPELEAF_ARRAY32 *cvtl = (CODEVIEW_TYPELEAF_ARRAY32 *)cvtr;

                        if (cvtl->type_index_element == old_index)
                            cvtl->type_index_element = new_index;
                        else if (mod == NULL && cvtl->type_index_element > old_index)
                            cvtl->type_index_element--;

                        if (cvtl->type_index_index == old_index)
                            cvtl->type_index_index = new_index;
                        else if (mod == NULL && cvtl->type_index_index > old_index)
                            cvtl->type_index_index--;
                        break;
                    }

                    case CODEVIEW_LEAFTYPE_STRUCTURE:
                    {
                        CODEVIEW_TYPELEAF_STRUCTURE32 *cvtl = (CODEVIEW_TYPELEAF_STRUCTURE32 *)cvtr;

                        if (cvtl->type_index_field == old_index)
                            cvtl->type_index_field = new_index;
                        else if (mod == NULL && cvtl->type_index_field > old_index)
                            cvtl->type_index_field--;

                        cvtl->type_index_dlist = cvtl->type_index_vshape = CODEVIEW_TYPE_NOTYPE;
                        break;
                    }

                    case CODEVIEW_LEAFTYPE_UNION:
                    {
                        CODEVIEW_TYPELEAF_UNION32 *cvtl = (CODEVIEW_TYPELEAF_UNION32 *)cvtr;

                        if (cvtl->type_index_field == old_index)
                            cvtl->type_index_field = new_index;
                        else if (mod == NULL && cvtl->type_index_field > old_index)
                            cvtl->type_index_field--;
                        break;
                    }

                    case CODEVIEW_LEAFTYPE_FIELDLIST:
                    {
                        CODEVIEW_TYPELEAF_FIELDLIST32 *cvtl = (CODEVIEW_TYPELEAF_FIELDLIST32 *)cvtr;
                        void *mp = cvtl->data;

                        while (mp < tp)
                        {
                            switch (*(ushort_t *)mp)
                            {
                                case CODEVIEW_LEAFTYPE_MEMBER:
                                {
                                    CODEVIEW_TYPELEAF_MEMBER32 *cvtl = (CODEVIEW_TYPELEAF_MEMBER32 *)mp;
                                    uchar_t *name = skip_value(cvtl->variant);

                                    if (cvtl->type_index == old_index)
                                        cvtl->type_index = new_index;
                                    else if (mod == NULL && cvtl->type_index > old_index)
                                        cvtl->type_index--;

                                    mp = &name[1] + name[0];
                                    break;
                                }

                                case CODEVIEW_LEAFTYPE_ENUMERATE:
                                {
                                    CODEVIEW_TYPELEAF_ENUMERATE *cvtl = (CODEVIEW_TYPELEAF_ENUMERATE *)mp;
                                    uchar_t *name = skip_value(cvtl->variant);

                                    mp = &name[1] + name[0];
                                    break;
                                }

                                default:
                                {
                                    mp = tp;
                                    break;
                                }
                            }

                            if (mp < tp && *(uchar_t *)mp >= CODEVIEW_LEAFTYPE_PAD0)
                                mp = (char *)mp + (*(uchar_t *)mp & 0xF);
                        }
                        break;
                    }

                    case CODEVIEW_LEAFTYPE_PROCEDURE:
                    {
                        CODEVIEW_TYPELEAF_PROCEDURE32 *cvtl = (CODEVIEW_TYPELEAF_PROCEDURE32 *)cvtr;

                        if (cvtl->type_index_return == old_index)
                            cvtl->type_index_return = new_index;
                        else if (mod == NULL && cvtl->type_index_return > old_index)
                            cvtl->type_index_return--;

                        if (cvtl->type_index_arglist == old_index)
                            cvtl->type_index_arglist = new_index;
                        else if (mod == NULL && cvtl->type_index_arglist > old_index)
                            cvtl->type_index_arglist--;
                        break;
                    }

                    case CODEVIEW_LEAFTYPE_ARGLIST:
                    {
                        CODEVIEW_TYPELEAF_ARGLIST32 *cvtl = (CODEVIEW_TYPELEAF_ARGLIST32 *)cvtr;
                        unsigned int i;

                        for (i = 0; i < cvtl->nargs; i++)
                        {
                            if (cvtl->args[i] == old_index)
                                cvtl->args[i] = new_index;
                            else if (mod == NULL && cvtl->args[i] > old_index)
                                cvtl->args[i]--;
                        }
                        break;
                    }

                    case CODEVIEW_LEAFTYPE_BITFIELD:
                    {
                        CODEVIEW_TYPELEAF_BITFIELD32 *cvtl = (CODEVIEW_TYPELEAF_BITFIELD32 *)cvtr;

                        if (cvtl->type_index == old_index)
                            cvtl->type_index = new_index;
                        else if (mod == NULL && cvtl->type_index > old_index)
                            cvtl->type_index--;
                        break;
                    }

                    case CODEVIEW_LEAFTYPE_MODIFIER:
                    {
                        CODEVIEW_TYPELEAF_MODIFIER *cvtl = (CODEVIEW_TYPELEAF_MODIFIER *)cvtr;

                        if (cvtl->type_index == old_index)
                            cvtl->type_index = new_index;
                        else if (mod == NULL && cvtl->type_index > old_index)
                            cvtl->type_index--;
                        break;
                    }

                    case CODEVIEW_LEAFTYPE_ENUM:
                    {
                        CODEVIEW_TYPELEAF_ENUM32 *cvtl = (CODEVIEW_TYPELEAF_ENUM32 *)cvtr;

                        if (cvtl->type_index_enum == old_index)
                            cvtl->type_index_enum = new_index;
                        else if (mod == NULL && cvtl->type_index_enum > old_index)
                            cvtl->type_index_enum--;

                        if (cvtl->type_index_field == old_index)
                            cvtl->type_index_field = new_index;
                        else if (mod == NULL && cvtl->type_index_field > old_index)
                            cvtl->type_index_field--;
                        break;
                    }

                    case CODEVIEW_TYPE_NOTYPE:
                        break;

                    default:
                        break;
                }
            }
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: skip_value                                                     *
 *                                                                          *
 * Purpose : Skip a CodeView variant value.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           03-06-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

static uchar_t *skip_value(void *p)
{
    switch (*(ushort_t *)p)
    {
        default:
            return (uchar_t *)p + sizeof(short);

        case CODEVIEW_LEAFTYPE_CHAR:
            return (uchar_t *)p + sizeof(short) + sizeof(char);

        case CODEVIEW_LEAFTYPE_SHORT:
        case CODEVIEW_LEAFTYPE_USHORT:
            return (uchar_t *)p + sizeof(short) + sizeof(short);

        case CODEVIEW_LEAFTYPE_LONG:
        case CODEVIEW_LEAFTYPE_ULONG:
            return (uchar_t *)p + sizeof(short) + sizeof(long);
    }
}
