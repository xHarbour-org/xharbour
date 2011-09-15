/****************************************************************************
 *                                                                          *
 * File    : object.c                                                       *
 *                                                                          *
 * Purpose : Win32 Linker; object file management.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "link.h"

#define SECTION_ALIGNMENT(sh) \
  (((sh)->s_flags & COFF_STYP_NOPAD) ? 1 : \
   ((sh)->s_flags & COFF_STYP_ALIGN) ? \
   (1 << ((((sh)->s_flags & COFF_STYP_ALIGN) >> 20)-1)) : 16)

/* Locals */
static COFF_SCNHDR *scnhdr = NULL;
static COFF_SYMENT *symptr = NULL;
static char *strptr = NULL;
static int nscns = 0;
static long nsyms = 0;

static SEGENTRY *seg_array[256], **seg_list = seg_array;  /* small buffer */
static MODENTRY *cur_mod = NULL;

/* Static function prototypes */
static SEGENTRY *process_section(COFF_SCNHDR *, short, const void *);
static void process_drectve_section(COFF_SCNHDR *, const void *);
static bool_t process_comdat_section(COFF_SCNHDR *, short, SYMENTRY **);
static void process_relocations(COFF_SCNHDR *, SEGENTRY *, const void *);
static void process_linenumbers(COFF_SCNHDR *, SEGENTRY *, const void *);
static void process_defined_symbol(COFF_SYMENT *);
static void process_absolute_symbol(COFF_SYMENT *);
static void process_debug_symbol(COFF_SYMENT *);
static void process_common_symbol(COFF_SYMENT *);
static void process_declared_symbol(COFF_SYMENT *);
static void *open_object_map(FILEINFO *);
static void close_object_map(FILEINFO *);
static const char *symbol_name(COFF_SYMENT *);
static const char *section_symbol_name(COFF_SYMENT *);
static void declare_entry_point(void);
static const char *get_entry_point(void);

/****************************************************************************
 *                                                                          *
 * Function: process_object_modules                                         *
 *                                                                          *
 * Purpose : Process all primary object files (from the command line).      *
 *           Also process any resource files.                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           99-02-15  New section alias for ".idata" and ".didat"          *
 *           02-01-14  New section alias for ".xdata" (ARM)                 *
 *           03-08-01  New section alias for ".CRT".                        *
 *                                                                          *
 ****************************************************************************/

void process_object_modules(void)
{
    FILEINFO *obj_file;

    lookup_section_alias(COFF_BSS, COFF_DATA);
    lookup_section_alias(COFF_EDATA, COFF_RDATA);
    lookup_section_alias(COFF_IDATA, COFF_DATA);
    lookup_section_alias(COFF_DIDAT, COFF_DATA);
    lookup_section_alias(COFF_XDATA, COFF_RDATA);
    lookup_section_alias(COFF_CRT, COFF_RDATA);

    if (obj_file_list == NULL)
        apperror(RCWARNING(ERROR_NO_OBJECT_FILES));

    for (obj_file = obj_file_list;
         obj_file != NULL;
         obj_file = obj_file->next)
    {
        void *base = open_object_map(obj_file);

        if (is_resource_file(obj_file))
            process_resource_file(obj_file);
        else
            process_object_module(obj_file->name, NULL, base);

        close_object_map(obj_file);
    }

    /* After objects but before library search */
    declare_entry_point();
}

/****************************************************************************
 *                                                                          *
 * Function: process_object_module                                          *
 *                                                                          *
 * Purpose : Process a object file or archive member.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           98-02-05  Allocate big section buffer when needed.             *
 *           98-03-17  Check for (bogus) files without any sections.        *
 *           98-11-28  Support for debug symbols added.                     *
 *           98-11-29  Support for linenumbers added.                       *
 *           00-11-16  Support for ARM machine added (new machine logic).   *
 *                                                                          *
 ****************************************************************************/

void process_object_module(const char *filename, FILEINFO *lib_file, const void *base)
{
    COFF_FILHDR *fhdr;
    short scnum;
    long i;

    /*
     * Process the filename; set up globals.
     */
    cur_mod = add_module_to_list();
    cur_mod->lib_file = lib_file;
    cur_mod->obj_file = lookup_name(basename(filename));

    if (options.verbose)
    {
        if (lib_file != NULL)
            printmsg(MSG_ARCHIVE_MODULE, basename(lib_file->name), cur_mod->obj_file->name);
        else
            printmsg(MSG_OBJECT_MODULE, cur_mod->obj_file->name);
    }

    /*
     * Process the file header.
     */
    fhdr = (COFF_FILHDR *)base;
    switch (fhdr->f_magic)
    {
        case COFF_F_MAG_I386:
            if (options.machine == MACHINE_UNKNOWN)
                options.machine = MACHINE_X86;
            else if (options.machine != MACHINE_X86)
                apperror(RCWARNING(ERROR_INVALID_MACHINE), cur_mod->obj_file->name);
            break;

        case COFF_F_MAG_ARM:
            if (options.machine == MACHINE_UNKNOWN)
                options.machine = MACHINE_ARM;
            else if (options.machine != MACHINE_ARM)
                apperror(RCFATAL(ERROR_INVALID_MACHINE), cur_mod->obj_file->name);
            break;

        case COFF_F_MAG_UNKNOWN:  /* any machine */
            break;

        default:  /* all other machines */
            apperror(RCFATAL(ERROR_INVALID_MACHINE), cur_mod->obj_file->name);
            break;
    }

    /*
     * Initialize the image pointers.
     */
    scnhdr = (COFF_SCNHDR *)((char *)base + COFF_FILHSZ + fhdr->f_opthdr);
    symptr = (COFF_SYMENT *)((char *)base + fhdr->f_symptr);
    strptr = (char *)(symptr + fhdr->f_nsyms) + sizeof(long);

    nscns = fhdr->f_nscns;
    nsyms = fhdr->f_nsyms;

    /*
     * Filter out bogus files (can be improved!).
     */
    if (nscns == 0)
    {
        apperror(RCWARNING(ERROR_INVALID_OBJECT), cur_mod->obj_file->name);
        return;
    }

    /*
     * Allocate a big section buffer, when needed.
     */
    if (nscns > NELEMS(seg_array))
        seg_list = (SEGENTRY **)my_alloc(nscns * sizeof(SEGENTRY *));

    /*
     * Process all sections.
     */
    for (scnum = 1; scnum <= nscns; scnum++)
    {
        /* Save pointer to section (NULL if section was skipped) */
        seg_list[scnum-1] = process_section(&scnhdr[scnum-1], scnum, base);
    }

    /*
     * Process all relocations.
     */
    for (scnum = 1; scnum <= nscns; scnum++)
    {
        if (seg_list[scnum-1] != NULL)
            process_relocations(&scnhdr[scnum-1], seg_list[scnum-1], base);
    }

    /*
     * Process all linenumbers.
     */
    for (scnum = 1; scnum <= nscns; scnum++)
    {
        if (seg_list[scnum-1] != NULL && (options.debug || options.mapfile_lines))
            process_linenumbers(&scnhdr[scnum-1], seg_list[scnum-1], base);
    }

    /*
     * Process all symbols.
     */
    for (i = 0; i < nsyms; i++)
    {
        COFF_SYMENT *se = &symptr[i];

        if (se->n_scnum > 0)
        {
            if (se->n_scnum > nscns)
                apperror(RCFATAL(ERROR_INVALID_OBJECT), cur_mod->obj_file->name);

            process_defined_symbol(se);
        }
        else if (se->n_scnum == COFF_N_ABS)
        {
            process_absolute_symbol(se);
        }
        else if (se->n_scnum == COFF_N_DEBUG)
        {
            if (options.debug || options.mapfile_lines)
                process_debug_symbol(se);
        }
        else if (se->n_scnum == COFF_N_UNDEF)
        {
            if (se->n_value == 0)
                process_declared_symbol(se);
            else
                process_common_symbol(se);
        }

        /* Skip auxiliary symbol entries */
        i += se->n_numaux;
    }

    /*
     * Free a big section buffer.
     */
    if (seg_list != seg_array)
    {
        my_free(seg_list);
        seg_list = seg_array;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: process_section                                                *
 *                                                                          *
 * Purpose : Process a single section.                                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           98-12-05  Support for CodeView sections added.                 *
 *           99-02-13  Support for delayed imports added.                   *
 *           99-03-18  Bugfix: *must* tweak alignment on import sections.   *
 *           00-12-06  Bugfix: can't just "ignore empty sections" by        *
 *                             checking the raw data size.                  *
 *           03-09-14  Added support for COFF_COMDAT_LARGEST.               *
 *                                                                          *
 ****************************************************************************/

static SEGENTRY *process_section(COFF_SCNHDR *sh, short scnum, const void *base)
{
    char name[8+1];
    SCNENTRY *scn;
    SEGENTRY *seg;
    SYMENTRY *compub = NULL;

    /*
     * Get the section name.
     */
    sscanf(sh->s_name, "%8[^ ]", name);

    /*
     * Process debug sections.
     */
    if (strcmp(name, COFF_DEBUGS) == 0 ||
        strcmp(name, COFF_DEBUGT) == 0 ||
        strcmp(name, COFF_DEBUGP) == 0)
    {
        if (!options.debug_cv) return NULL;
    }

    if (strcmp(name, COFF_DEBUGF) == 0)
    {
        if (!options.debug) return NULL;
    }

    /*
     * Process special sections.
     */
    if ((sh->s_flags & COFF_STYP_REMOVE) ||
        (sh->s_flags & COFF_STYP_INFO))
    {
        if (strcmp(name, COFF_DRECTVE) == 0)
            process_drectve_section(sh, base);
        return NULL;
    }
    else if (sh->s_flags & COFF_STYP_COMDAT)
    {
        if (!process_comdat_section(sh, scnum, &compub))
            return NULL;
    }

    /*
     * Allocate a section buffer.
     */
    if (strcmp(name, COFF_IDATA4) == 0 ||
        strcmp(name, COFF_IDATA5) == 0 ||
        strcmp(name, COFF_DIDAT4) == 0 ||
        strcmp(name, COFF_DIDAT5) == 0 ||
        strcmp(name, COFF_DIDAT7) == 0 ||
        strcmp(name, COFF_DIDAT8) == 0)
    {
        scn = lookup_section(name, sh->s_flags);
        seg = add_segment_to_import_section(scn, cur_mod->lib_file);
    }
    else
    {
        scn = lookup_section(name, sh->s_flags);
        seg = add_segment_to_section(scn);
    }

    seg->alignment = SECTION_ALIGNMENT(sh);
    seg->size = sh->s_size;
    seg->mod = cur_mod;

    /*
     * Some modules have byte alignment while others have dword alignment.
     * If we don't handle this, there will be gaps in the address tables
     * causing access violations etc.
     */
    if (strcmp(name, COFF_IDATA2) == 0 ||
        strcmp(name, COFF_IDATA3) == 0 ||
        strcmp(name, COFF_IDATA4) == 0 ||
        strcmp(name, COFF_IDATA5) == 0)
        seg->alignment = sizeof(long);

    /*
     * Copy *initialized* raw data. Use scn->flags if lookup_section changed it.
     */
    if ((scn->flags & COFF_STYP_TEXT) ||
        (scn->flags & COFF_STYP_DATA))
    {
        seg->data = my_alloc(seg->size);
        memcpy(seg->data, ((char *)base + sh->s_scnptr), seg->size);
    }

    if (options.section_alignment < seg->alignment)
        apperror(RCFATAL(ERROR_ALIGN_TOO_SMALL), seg->alignment);

    /*
     * the comdat section type COFF_COMDAT_LARGEST needs more work.
     */
    if (compub && compub->comdat == COFF_COMDAT_LARGEST)
    {
        if (compub->comseg == NULL)
        {
            /* first comdat section is always largest */
            compub->comseg = seg;
        }
        else if (compub->comseg->size < seg->size)
        {
            SEGENTRY *seg;
            SEGENTRY *seg_prev;

            /* this comdat section is now largest -- drop the previous */
            for (seg = scn->segs, seg_prev = NULL;
                 seg != NULL;
                 seg_prev = seg, seg = seg->next)
            {
                if (seg == compub->comseg)
                {
                    discard_segment(seg);

                    if (seg_prev)
                        seg_prev->next = seg->next;
                    else
                        scn->segs = seg->next;
                    break;
                }
            }
        }
    }

    return seg;
}

/****************************************************************************
 *                                                                          *
 * Function: process_drectve_section                                        *
 *                                                                          *
 * Purpose : Process the special ".drectve" section.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           99-01-29  Bugfix: didn't free the raw data buffer.             *
 *                                                                          *
 ****************************************************************************/

static void process_drectve_section(COFF_SCNHDR *sh, const void *base)
{
    char **eargv;
    char *buf;
    int eargc;

    buf = (char *)my_alloc(sh->s_size+1);
    memcpy(buf, ((char *)base + sh->s_scnptr), sh->s_size);
    buf[sh->s_size] = '\0';

    tokenize(buf, &eargc, &eargv);
    link_args(eargc, eargv, TRUE);
    my_free(eargv);

    my_free(buf);
}

/****************************************************************************
 *                                                                          *
 * Function: process_comdat_section                                         *
 *                                                                          *
 * Purpose : Process the special comdat section.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           98-03-21  Checksum added to filter out unwanted warnings.      *
 *           03-09-14  Added support for COFF_COMDAT_LARGEST.               *
 *                                                                          *
 ****************************************************************************/

static bool_t process_comdat_section(COFF_SCNHDR *sh, short scnum, SYMENTRY **compub)
{
    uchar_t comdat = 0;
    long checksum = 0;
    long i;

    UNREFERENCED_PARAMETER(sh);

    /*
     * The Selection field of the Section Definition auxiliary format is
     * applicable if the section is a COMDAT section: a section that can be
     * defined by more than one object file. (The flag COFF_STYP_COMDAT
     * is set in the Section Flags field of the section header.) The way
     * that the linker resolves the multiple definitions of COMDAT sections
     * is determined by the Selection field.
     *
     * The first symbol having the section value of the COMDAT section is
     * the section symbol. This symbol has the name of the section, Value
     * field equal to 0, the section number of the COMDAT section in
     * question, Type field equal to COFF_T_NULL, Class field equal to
     * COFF_C_STAT, and one auxiliary record. The second symbol is called
     * "the COMDAT symbol" and is used by the linker in conjunction with
     * the Selection field.
     *
     * Values for the Selection field are shown below:
     *
     * COFF_COMDAT_NODUPS (1)
     *   The linker generates a warning if more than one section defines
     *   the same COMDAT symbol, but links in one of the sections anyway.
     *
     * COFF_COMDAT_ANY (2)
     *   Any section defining the same COMDAT symbol may be linked; the
     *   rest are removed.
     *
     * COFF_COMDAT_SAMESZ (3)
     *   The linker chooses an arbitrary section among the duplicate
     *   sections (having same COMDAT symbol); however, all must be
     *   the same size or the linker generates a warning.
     *
     * COFF_COMDAT_EXACT (4)
     *   All duplicate sections must match exactly. One of them is linked.
     *   (This is not currently implemented in the linker).
     *
     * COFF_COMDAT_ASSOC (5)
     *   The section is linked if a certain other COMDAT section is linked.
     *   This other section is indicated by the Number field of the
     *   auxiliary symbol record for the section definition. Use of this
     *   setting is useful for definitions that have components in multiple
     *   sections (for example, code in one and data in another), but where
     *   all must be linked together.
     *
     * COFF_COMDAT_LARGEST (6)
     *   The linker chooses the largest from the definitions for this symbol.
     *   If multiple definitions have this size the choice between them is
     *   arbitrary.
     */

    for (i = 0; i < nsyms; i++)
    {
        COFF_SYMENT *se = &symptr[i];

        if (se->n_scnum == scnum)
        {
            if (comdat == 0)
            {
                /*
                 * Process the section symbol.
                 */
                COFF_AUXENT *ae;

                if (se->n_sclass != COFF_C_STAT ||
                    se->n_type != COFF_T_NULL ||
                    se->n_value != 0 ||
                    se->n_numaux != 1)
                {
                    apperror(RCFATAL(ERROR_INVALID_OBJECT), cur_mod->obj_file->name);
                    return FALSE;  /* skip this section */
                }

                ae = (COFF_AUXENT *)(se+1);
                comdat = ae->x_sel;
                checksum = ae->x_chksum;

                if (comdat == COFF_COMDAT_ASSOC && seg_list[ae->x_scnum-1] == NULL)
                    return FALSE;  /* skip this section */
            }
            else if (se->n_sclass == COFF_C_EXT)
            {
                /*
                 * Process the COMDAT symbol.
                 */
                SYMENTRY *pub;

                *compub = pub = lookup_public(symbol_name(se));

                if (comdat != COFF_COMDAT_NODUPS &&
                    comdat != COFF_COMDAT_ANY &&
                    comdat != COFF_COMDAT_SAMESZ &&
                    comdat != COFF_COMDAT_EXACT &&
                    comdat != COFF_COMDAT_ASSOC &&
                    comdat != COFF_COMDAT_LARGEST)
                    apperror(RCWARNING(ERROR_UNKNOWN_COMDAT_TYPE), (int)comdat, pub->name);

                if (pub->comdat == COFF_COMDAT_NODUPS && pub->checksum != checksum)
                    apperror(RCWARNING(ERROR_MULTIPLE_COMDAT), pub->name);

                if (pub->comdat == COFF_COMDAT_NODUPS ||
                    pub->comdat == COFF_COMDAT_ANY ||
                    pub->comdat == COFF_COMDAT_SAMESZ ||
                    pub->comdat == COFF_COMDAT_EXACT)
                    return FALSE;  /* skip this section */

                if (pub->comdat == 0)
                {
                    pub->comdat = comdat;
                    pub->checksum = checksum;
                }

                /* include this section */
                return TRUE;
            }
            else
            {
                /* ignore other COMDATs */
                return TRUE;
            }
        }

        /* Skip auxiliary symbol entries */
        i += se->n_numaux;
    }

    return TRUE;
}

/****************************************************************************
 *                                                                          *
 * Function: process_relocations                                            *
 *                                                                          *
 * Purpose : Process relocations for the given section.                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           99-04-24  Support for 'weak external' added.                   *
 *           99-11-14  Set 'referenced' flag for static symbols too.        *
 *           00-02-07  Extended relocation check added.                     *
 *                                                                          *
 ****************************************************************************/

static void process_relocations(COFF_SCNHDR *sh, SEGENTRY *seg, const void *base)
{
    COFF_RELOC *re;
    int i;

    if ((sh->s_flags & COFF_STYP_XRELOC) != 0)  /* && sh->s_nreloc != 0xFFFF) */
        apperror(RCFATAL(ERROR_INVALID_OBJECT), cur_mod->obj_file->name);

    /*
     * Walk through all relocations for this section.
     */
    re = (COFF_RELOC *)((char *)base + sh->s_relptr);
    for (i = 0; i < sh->s_nreloc; i++, re++)
    {
        COFF_SYMENT *se = &symptr[re->r_symndx];
        SYMENTRY *sym = NULL;

        /*
         * Get the relocation symbol.
         */
        if (se->n_sclass == COFF_C_EXT ||
            se->n_sclass == COFF_C_WEAKEXT)
        {
            sym = lookup_public(symbol_name(se));
            sym->flags.referenced = TRUE;
        }
        else if (se->n_sclass == COFF_C_SECT)
        {
            sym = lookup_public(section_symbol_name(se));
            sym->flags.referenced = TRUE;
        }
        else  /* COFF_C_STAT or COFF_C_LABEL */
        {
            SEGENTRY *seg;

            seg = seg_list[se->n_scnum-1];
            if (seg) sym = lookup_static(seg, symbol_name(se));
            if (sym) sym->flags.referenced = TRUE;   /* 99-11-14 */
        }

        /*
         * Process the relocation.
         */
        if (sym != NULL)
        {
            RELENTRY *rel;

            rel = add_relocation_to_segment(seg);
            rel->offset = re->r_vaddr;
            rel->type = re->r_type;
            rel->sym = sym;
        }
        else
        {
            /* avoid strange error when linking with debug info... (01-06-16) */
            if (options.machine != MACHINE_ARM || strcmp(symbol_name(se), "__drectve") != 0)
            {
                /* No symbol for the relocation. Not very good! */
                apperror(RCERROR(ERROR_BAD_RELOC_SYMCLASS), symbol_name(se));
            }
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: process_linenumbers                                            *
 *                                                                          *
 * Purpose : Process linenumbers for the given section.                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-11-29  Created                                              *
 *           02-04-04  Use function symbol in linenumber info (bugfix).     *
 *                                                                          *
 ****************************************************************************/

static void process_linenumbers(COFF_SCNHDR *sh, SEGENTRY *seg, const void *base)
{
    COFF_LINENO *le;
    LINENTRY *lines;
    int i;

    if (sh->s_nlnno == 0)
        return;

    /*
     * Allocate array for the linenumbers.
     */
    seg->lines = (LINENTRY *)my_alloc(sh->s_nlnno * sizeof(LINENTRY));
    seg->nlines = sh->s_nlnno;

    /*
     * Walk through all linenumbers for this section.
     */
    lines = seg->lines;
    le = (COFF_LINENO *)((char *)base + sh->s_lnnoptr);
    for (i = 0; i < sh->s_nlnno; i++, le++, lines++)
    {
        if (le->l_lnno == 0)
        {
            //
            // Process linenumber for the function entry symbol.
            //
            COFF_SYMENT *se = &symptr[le->l_symndx];

            lines->sym = (se->n_sclass == COFF_C_EXT) ?
                lookup_public(symbol_name(se)) :
                lookup_static(seg, symbol_name(se));
            lines->vaddr = 0;
            lines->lineno = 0;

            if (se->n_numaux != 0)
            {
                COFF_AUXENT *ae = (COFF_AUXENT *)(se+1);

                se = &symptr[ae->x_tagndx];
                if (se->n_numaux != 0)
                {
                    ae = (COFF_AUXENT *)(se+1);
                    lines->lineno = ae->x_lnno;  /* only for CV info */
                }
            }
        }
        else
        {
            //
            // Process linenumber.
            //
            lines->sym = NULL;
            lines->vaddr = le->l_paddr;
            lines->lineno = le->l_lnno;
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: process_defined_symbol                                         *
 *                                                                          *
 * Purpose : Process a symbol definition.                                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           98-12-09  COFF_C_FCN case added.                               *
 *           99-02-13  Support for delayed imports added.                   *
 *           99-08-19  Bugfix: dont't report multiple defs for commons.     *
 *           03-06-27  Added force_multiple option.                         *
 *                                                                          *
 ****************************************************************************/

static void process_defined_symbol(COFF_SYMENT *se)
{
    SEGENTRY *seg;

    seg = seg_list[se->n_scnum-1];
    if (seg != NULL)
    {
        SYMENTRY *pub;

        if (se->n_sclass == COFF_C_EXT)
        {
            pub = lookup_public(symbol_name(se));

            /*
             * Is this one of the special null-thunk-data symbols?
             * "\177<name>_NULL_THUNK_DATA" or
             * "\177<name>_NULL_THUNK_DATA_DLA" or
             * "\177<name>_NULL_THUNK_DATA_DLB" or
             * "\177<name>_NULL_THUNK_DATA_DLN" or
             * "\177<name>_NULL_THUNK_DATA_DLU".
             */
            if (pub->name[0] == '\177' && strstr(
                pub->name, "_NULL_THUNK_DATA") != NULL)
            {
                int i;

                /* Make sure this section stays in the back!! */
                for (i = 1; i <= nscns; i++)
                {
                    if (seg_list[i-1] != NULL)
                        seg_list[i-1]->keep_last = TRUE;
                }
            }

            if (pub->type == unresolved ||
                pub->type == unresolved_weak)
            {
                pub->def_mod = cur_mod;
                unresolved_count--;
            }
            else if (pub->type == resolved)
            {
                /* It seems ok for commons to 'overlap' other types */
                /* RP 2011-09-15
                if (!pub->flags.common && !options.force_multiple)*/
                if (!pub->flags.common)
                {
                    char msg[512];

                    sprintf( msg, "%s(%s)->%s previously defined at %s(%s)",
                                  ( cur_mod->lib_file ? cur_mod->lib_file->name : "" ),
                                  cur_mod->obj_file->name,
                                  pub->name,
                                  ( pub->def_mod && pub->def_mod->lib_file ? pub->def_mod->lib_file->name : "" ),
                                  pub->def_mod->obj_file->name );

                    /* RP 2011-09-15
                    apperror(RCERROR(ERROR_MULTIPLE_SYMBOLS), msg);*/

                    /* RP 2011-09-15 */
                    if (options.force_multiple)
                    {
                       apperror(RCWARNING(ERROR_MULTIPLE_SYMBOLS), msg);
                    }
                    else
                    {
                       apperror(RCERROR(ERROR_MULTIPLE_SYMBOLS), msg);
                    }
                    /* RP END 2011-09-15 */
                }
                return;
            }

            /* RP 2011-09-15 fixed GPF */
            pub->def_mod = cur_mod;

            pub->flags.function |= ISFCN(se->n_type);
            pub->value = se->n_value;
            pub->class = se->n_sclass;
            pub->type = resolved;

            add_public_to_segment(seg, pub);
        }
        else if (se->n_sclass == COFF_C_SECT)
        {
            pub = lookup_public(section_symbol_name(se));

            if (pub->type == unresolved ||
                pub->type == unresolved_weak)
            {
                pub->def_mod = cur_mod;
                unresolved_count--;
            }
            else if (pub->type == resolved)
            {
                /* RP 2011-09-15
                if (!options.force_multiple)*/
                {
                    char msg[512];

                    sprintf( msg, "%s(%s)->%s previously defined at %s(%s)",
                                  ( cur_mod->lib_file ? cur_mod->lib_file->name : "" ),
                                  cur_mod->obj_file->name,
                                  pub->name,
                                  ( pub->def_mod && pub->def_mod->lib_file ? pub->def_mod->lib_file->name : "" ),
                                  pub->def_mod->obj_file->name );

                    /* RP 2011-09-15
                    apperror(RCERROR(ERROR_MULTIPLE_SYMBOLS), msg);*/

                    /* RP 2011-09-15 */
                    if (options.force_multiple)
                    {
                       apperror(RCWARNING(ERROR_MULTIPLE_SYMBOLS), msg);
                    }
                    else
                    {
                       apperror(RCERROR(ERROR_MULTIPLE_SYMBOLS), msg);
                    }
                    /* RP END 2011-09-15 */
                }
                return;
            }

            /* RP 2011-09-15 fixed GPF */
            pub->def_mod = cur_mod;

            pub->flags.internal = TRUE;
            pub->value = 0;
            pub->class = se->n_sclass;
            pub->type = resolved;

            add_public_to_segment(seg, pub);
        }
        else if (se->n_sclass == COFF_C_FCN)
        {
            /*
             * Used by Microsoft tools for symbol records that define
             * the extent of a function: begin function (named .bf),
             * end function (.ef), and lines in function (.lf). For
             * .lf records, Value gives the number of source lines in
             * the function. For .ef records, Value gives the size
             * of function code.
             */
        }
        else  /* COFF_C_STAT or COFF_C_LABEL */
        {
            SYMENTRY *sym;

            sym = lookup_static(seg, symbol_name(se));

            if (sym->type == resolved)
            {
                /* RP 2011-09-15
                if (!options.force_multiple)*/
                {
                    char msg[512];

                    sprintf( msg, "%s(%s)->%s previously defined at %s(%s)",
                                  ( cur_mod->lib_file ? cur_mod->lib_file->name : "" ),
                                  cur_mod->obj_file->name,
                                  sym->name,
                                  ( sym->def_mod->lib_file ? sym->def_mod->lib_file->name : "" ),
                                  sym->def_mod->obj_file->name );

                    /* RP 2011-09-15
                    apperror(RCERROR(ERROR_MULTIPLE_SYMBOLS), msg);*/

                    /* RP 2011-09-15 */
                    if (options.force_multiple)
                    {
                       apperror(RCWARNING(ERROR_MULTIPLE_SYMBOLS), msg);
                    }
                    else
                    {
                       apperror(RCERROR(ERROR_MULTIPLE_SYMBOLS), msg);
                    }
                    /* RP END 2011-09-15 */
                }
                return;
            }

            sym->flags.function |= ISFCN(se->n_type);
            sym->value = se->n_value;
            sym->class = se->n_sclass;
            sym->type = resolved;

            /*
             * Special handling of static import sections.
             */
            if (strcmp(sym->name, COFF_IDATA4) == 0 ||
                strcmp(sym->name, COFF_IDATA5) == 0 ||
                strcmp(sym->name, COFF_DIDAT4) == 0 ||
                strcmp(sym->name, COFF_DIDAT5) == 0 ||
                strcmp(sym->name, COFF_DIDAT7) == 0 ||
                strcmp(sym->name, COFF_DIDAT8) == 0)
            {
                pub = lookup_public(section_symbol_name(se));

                if (pub->type == unresolved ||
                    pub->type == unresolved_weak)
                {
                    pub->def_mod = cur_mod;
                    unresolved_count--;
                }
                else if (pub->type == resolved)
                {
                    return;
                }

                /* RP 2011-09-15 fixed GPF */
                pub->def_mod = cur_mod;

                pub->flags.internal = TRUE;
                pub->value = 0;
                pub->class = 0;
                pub->type = resolved;

                add_public_to_segment(seg, pub);
            }
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: process_absolute_symbol                                        *
 *                                                                          *
 * Purpose : Process a absolute symbol definition.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           98-11-08  Bugfix: symbols are not always public!?              *
 *           03-06-27  Added force_multiple option.                         *
 *                                                                          *
 ****************************************************************************/

static void process_absolute_symbol(COFF_SYMENT *se)
{
    if (se->n_sclass == COFF_C_EXT)
    {
        SYMENTRY *pub;

        pub = lookup_public(symbol_name(se));

        if (pub->type == unresolved ||
            pub->type == unresolved_weak)
        {
            pub->def_mod = cur_mod;
            unresolved_count--;
        }
        else if (pub->type == resolved)
        {
            /* RP 2011-09-15
            if ((!pub->flags.absolute || pub->value != se->n_value) && !options.force_multiple)*/
            if (!pub->flags.absolute || pub->value != se->n_value)
            {
                char msg[512];

                sprintf( msg, "%s(%s)->%s previously defined at %s(%s)",
                              ( cur_mod->lib_file ? cur_mod->lib_file->name : "" ),
                              cur_mod->obj_file->name,
                              pub->name,
                              ( pub->def_mod && pub->def_mod->lib_file ? pub->def_mod->lib_file->name : "" ),
                              pub->def_mod->obj_file->name );

                /* RP 2011-09-15
                apperror(RCERROR(ERROR_MULTIPLE_SYMBOLS), msg);*/

                /* RP 2011-09-15 */
                if (options.force_multiple)
                {
                   apperror(RCWARNING(ERROR_MULTIPLE_SYMBOLS), msg);
                }
                else
                {
                   apperror(RCERROR(ERROR_MULTIPLE_SYMBOLS), msg);
                }
                /* RP END 2011-09-15 */
            }
            return;
        }

        /* RP 2011-09-15 fixed GPF */
        pub->def_mod = cur_mod;

        pub->flags.absolute = TRUE;
        pub->value = se->n_value;
        pub->class = se->n_sclass;
        pub->type = resolved;
    }
    else /* COFF_C_STAT */
    {
        /*
         * Static symbols are always associated with a section (SEGENTRY).
         * Absolute symbols are -by definition- not. The only static
         * absolute symbol i have seen is "@comp.id" (in new Microsoft
         * import libraries). I don't know what it's good for, but it
         * doesn't seem to matter. We just ignore them.
         */
    }
}

/****************************************************************************
 *                                                                          *
 * Function: process_debug_symbol                                           *
 *                                                                          *
 * Purpose : Process a debug symbol definition.                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-11-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void process_debug_symbol(COFF_SYMENT *se)
{
    if (se->n_sclass == COFF_C_FILE)
    {
        COFF_AUXENT *ae;

        ae = (COFF_AUXENT *)(se+1);
        cur_mod->src_file = lookup_name(basename(ae->x_fname));
    }
}

/****************************************************************************
 *                                                                          *
 * Function: process_common_symbol                                          *
 *                                                                          *
 * Purpose : Process a common symbol definition.                            *
 *                                                                          *
 * Comment : A common symbol is the only one that can be multiply defined.  *
 *           The final size of the symbol is the largest value found.       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void process_common_symbol(COFF_SYMENT *se)
{
    if (se->n_sclass == COFF_C_SECT)
    {
        /*
         * We are here only because the symbol value
         * contains the section characteristics!
         */
        process_declared_symbol(se);
    }
    else
    {
        SYMENTRY *pub;

        pub = lookup_public(symbol_name(se));

        if (pub->type == unresolved ||
            pub->type == unresolved_weak)
        {
            pub->def_mod = cur_mod;
            unresolved_count--;
        }
        else if (pub->type == resolved)
        {
            /* it's ok for commons to overlap */
            if (!pub->flags.common) return;
        }

        /* RP 2011-09-15 fixed GPF */
        pub->def_mod = cur_mod;

        pub->flags.common = TRUE;
        pub->type = resolved;
        pub->class = se->n_sclass;

        /* Remember the largest common */
        pub->value = max(pub->value, se->n_value);
    }
}

/****************************************************************************
 *                                                                          *
 * Function: process_declared_symbol                                        *
 *                                                                          *
 * Purpose : Process a external symbol declaration.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           03-07-16  Added support for badly mangled symbols from POLIB.  *
 *                                                                          *
 ****************************************************************************/

static void process_declared_symbol(COFF_SYMENT *se)
{
    if (se->n_sclass == COFF_C_EXT)
    {
        SYMENTRY *pub;
        char *p;

        pub = lookup_public(symbol_name(se));
        pub->flags.referenced = TRUE;

        if (pub->type == unused)
        {
            pub->type = unresolved;
            pub->dec_mod = cur_mod;
            unresolved_count++;

            /*
             * We have problems when building import libraries from DLLs.
             * It's impossible to determine the argument size, so we can't
             * create a correctly mangled name in POLIB. We try to solve
             * this mess through weak externals...
             */
            if (*pub->name == '_' && (p = strchr(pub->name, '@')) != NULL)
            {
                pub->weak_name = my_alloc(p - pub->name + 1);
                strncpy(pub->weak_name, pub->name, p - pub->name);
                pub->weak_name[p - pub->name] = '\0';
                pub->type = unresolved_weak;
            }
        }
    }
    else if (se->n_sclass == COFF_C_SECT)
    {
        SYMENTRY *pub;

        pub = lookup_public(section_symbol_name(se));
        pub->flags.referenced = TRUE;

        if (pub->type == unused)
        {
            pub->type = unresolved;
            pub->dec_mod = cur_mod;
            unresolved_count++;

            if( pub->type == unresolved )
            {
               if( strncmp( pub->name, "__imp_", 6 ) == 0 )
               {
                  pub->weak_name = tstrcpy( pub->name + 6 );
                  pub->type = unresolved_weak;
                  //OutputDebugString( pub->weak_name );
               }
            }
        }
    }
    else if (se->n_sclass == COFF_C_WEAKEXT)
    {
        /*
         * "Weak externals" are a mechanism for object files allowing
         * flexibility at link time. A module can contain an unresolved
         * external symbol (sym1), but it can also include an auxiliary
         * record indicating that if sym1 is not present at link time,
         * another external symbol (sym2) is used to resolve references
         * instead.
         *
         * If a definition of sym1 is linked, then an external reference
         * to the symbol is resolved normally. If a definition of sym1
         * is not linked, then all references to the weak external for
         * sym1 refer to sym2 instead. The external symbol, sym2, must
         * always be linked; typically it is defined in the module
         * containing the weak reference to sym1.
         */

        COFF_AUXENT *ae;
        SYMENTRY *pub;

        pub = lookup_public(symbol_name(se));
        pub->flags.referenced = TRUE;

        if (se->n_numaux != 1)
            apperror(RCFATAL(ERROR_INVALID_OBJECT), cur_mod->obj_file->name);

        ae = (COFF_AUXENT *)(se+1);
        se = &symptr[ae->x_tagndx];

        if (ae->x_tsize == COFF_WEAKEXT_NOLIB ||
            ae->x_tsize == COFF_WEAKEXT_LIB)
        {
            if (pub->type == unused)
            {
                pub->flags.nosearch = (ae->x_tsize == COFF_WEAKEXT_NOLIB);
                pub->weak_name = tstrcpy(symbol_name(se));
                pub->type = unresolved_weak;
                pub->dec_mod = cur_mod;
                unresolved_count++;
            }
        }
        else if (ae->x_tsize == COFF_WEAKEXT_ALIAS)
        {
            if (pub->type == unresolved ||
                pub->type == unresolved_weak)
            {
                SYMENTRY *pub_alias;

                pub_alias = lookup_public(symbol_name(se));
                replace_public_node(pub, pub_alias);

                pub->type = unused;
                unresolved_count--;
            }
        }
        else
        {
            apperror(RCFATAL(ERROR_UNKNOWN_WEAK_TYPE),
                ae->x_tsize, pub->name);
        }
    }
    else
    {
        /* Oops, did we have this class too?! */
        apperror(RCERROR(ERROR_INTERNAL), "process_declared_symbol");
    }
}

/****************************************************************************
 *                                                                          *
 * Function: post_process_common_symbols                                    *
 *                                                                          *
 * Purpose : Allocate space for communals in the data section (".bss").     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

void post_process_common_symbols(void)
{
    SYMENTRY *pub;

    for (pub = public_list; pub != NULL; pub = pub->next)
    {
        if (pub->flags.common)
        {
            SCNENTRY *scn;
            SEGENTRY *seg;

            /*
             * Allocate a section buffer.
             */
            scn = lookup_section(COFF_BSS, COFF_BSS_FLAGS);
            seg = add_segment_to_section(scn);

            seg->alignment = sizeof(long);
            seg->size = pub->value;
            pub->value = 0;  /* length --> offset */

            /*
             * Associate the communal with the section.
             */
            add_public_to_segment(seg, pub);
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: open_object_map                                                *
 *                                                                          *
 * Purpose : Create a file-mapping of a object file.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
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
 *           98-01-28  Created                                              *
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
 * Function: section_symbol_name                                            *
 *                                                                          *
 * Purpose : Return a symbol name to handle (import) section fixups.        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static const char *section_symbol_name(COFF_SYMENT *se)
{
    static char name[MAX_PATH];

    if (cur_mod->lib_file == NULL)
        return symbol_name(se);

    strcpy(name, basename(cur_mod->lib_file->name));
    update_extension_in_file(name, "");
    strcat(name, ":");
    strcat(name, symbol_name(se));

    return name;
}

/****************************************************************************
 *                                                                          *
 * Function: declare_entry_point                                            *
 *                                                                          *
 * Purpose : Declare the entry point symbol.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void declare_entry_point(void)
{
    if (!options.no_entry)
    {
        pub_entry = lookup_public(get_entry_point());
        pub_entry->flags.referenced = TRUE;
        pub_entry->dec_mod = cur_mod;

        if (pub_entry->type == unused)
        {
            pub_entry->weak_name = tstrcat("_", pub_entry->name);
            pub_entry->type = unresolved_weak;
            unresolved_count++;
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: get_entry_point                                                *
 *                                                                          *
 * Purpose : Return the name of the entry point.                            *
 *                                                                          *
 * Comment : Use exact symbol names for the predefined entry points, so     *
 *           that we catch them during the first library pass.              *
 *           If not, we get weird problems linking MS VC++ programs.        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           98-02-17  NT driver entry point "DriverEntry" added.           *
 *           00-11-16  Windows CE entry point "WinMainCRTStartup" added.    *
 *           02-07-24  Windows CE entry point "_DllMainCRTStartup" added.   *
 *                                                                          *
 ****************************************************************************/

static const char *get_entry_point(void)
{
    if (entry_point) return entry_point;

    if (options.DLL)
    {
        if (options.subsystem == COFF_SS_WCEGUI)
            return "_DllMainCRTStartup";
        else
            return "__DllMainCRTStartup@12";
    }

    if (options.subsystem == COFF_SS_WINCUI ||
        options.subsystem == COFF_SS_POSIXCUI ||
        options.subsystem == COFF_SS_OS2CUI)
        return "_mainCRTStartup";

    if (options.subsystem == COFF_SS_WINGUI)
        return "_WinMainCRTStartup";

    if (options.subsystem == COFF_SS_WCEGUI)
        return "WinMainCRTStartup";

    if (options.subsystem == COFF_SS_NATIVE)
        return "_DriverEntry";

    return "_main";
}

