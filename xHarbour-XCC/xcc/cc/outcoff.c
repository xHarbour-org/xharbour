/****************************************************************************
 *                                                                          *
 * File    : outcoff.c                                                      *
 *                                                                          *
 * Purpose : ISO C Compiler; Generic Assembler; COFF output driver.         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           00-11-21  Support for ARM target added.                        *
 *           03-04-26  New field in FCNENTRY: nlineno (# of COFF_LINENO).   *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include <coff.h>
#include "lcc.h"

/* true if the current target is X86 */
#define IS_TARGET_X86  (OF->init == x86_init)

typedef struct _RELENTRY {
    struct _RELENTRY *link;     /* ptr to next node */
    COFF_RELOC re;              /* std COFF relocation entry */
    enum {
        SECT_SYMBOL,            /* refers to a section */
        REAL_SYMBOL             /* refers to a 'real' symbol */
    } symtype;
} RELENTRY;

typedef struct _LINENTRY {
    struct _LINENTRY *link;     /* ptr to next node */
    COFF_LINENO le;             /* std COFF linenumber entry */
} LINENTRY;

typedef struct _SCNENTRY {
    COFF_SCNHDR sh;             /* std COFF section header */
    SAA *data;                  /* raw data pointer */
    RELENTRY *rlist;            /* relocations */
    LINENTRY *llist;            /* linenumbers */
    long segment;               /* internal segment id */
    long nreloc;                /* number of relocations (room for extended relocation) */
} SCNENTRY;

typedef struct _FCNENTRY {
    COFF_SYMENT *se_fcn;        /* ptr to symbol entry (function) */
    COFF_AUXENT *ae_fcn;        /* ptr to aux entry (function) */
    COFF_SYMENT *se_bf;         /* ptr to symbol entry (.bf) */
    COFF_AUXENT *ae_bf;         /* ptr to aux entry (.bf) */
    COFF_SYMENT *se_lf;         /* ptr to symbol entry (.lf) */
    COFF_SYMENT *se_ef;         /* ptr to symbol entry (.ef) */
    COFF_AUXENT *ae_ef;         /* ptr to aux entry (.ef) */
    long symndx;                /* index of se_fcn (followed by all the other) */
    long size;                  /* size of function */
    int lineno;                 /* first source line of function */
    int lineno_last;            /* last source line of function */
    long nlineno;               /* number of source records */
    char name[1];               /* name of function */
} FCNENTRY;

#define TEXT_FLAGS  (COFF_STYP_TEXT|COFF_STYP_A16|COFF_STYP_READ|COFF_STYP_EXEC)
#define DATA_FLAGS  (COFF_STYP_DATA|COFF_STYP_A4|COFF_STYP_READ|COFF_STYP_WRITE)
#define RDATA_FLAGS (COFF_STYP_DATA|COFF_STYP_A4|COFF_STYP_READ)
#define DBG_FLAGS   (COFF_STYP_DATA|COFF_STYP_NOPAD|COFF_STYP_DISCARD|COFF_STYP_READ)
#define BSS_FLAGS   (COFF_STYP_BSS|COFF_STYP_A4|COFF_STYP_READ|COFF_STYP_WRITE)
#define INFO_FLAGS  (COFF_STYP_INFO|COFF_STYP_A1|COFF_STYP_REMOVE)

#define SECT_DELTA 32

static char coff_srcfile[256];

static SCNENTRY **sects;
static short nscns;
static short nscns_max;

static long defseg;

static int ndefsyms;
static int nfilesyms;

static SAA *syms;
static ulong_t nsyms;

static RAA *bsym;
static RAA *symval;

static SAA *strs;
static ulong_t strslen;

static LIST *fcnlist;
static FCNENTRY *fcn;

/* Static function prototypes */
static long coff_segment(const char *, const char *, int, int *);
static void cache_raw_data(SCNENTRY *, const void *, long);
static void write_object_file(void);
static void write_relocations(SCNENTRY *);
static void write_linenumbers(SCNENTRY *);
static void write_symbol_table(void);
static int new_section(const char *, ulong_t);
static void new_reloc(SCNENTRY *, long, int);
static void finalize_relocation_count(SCNENTRY *);
static void new_lineno(SCNENTRY *, int);

/****************************************************************************
 *                                                                          *
 * Function: coff_filename                                                  *
 *                                                                          *
 * Purpose : Construct the output filename, if not given by the user.       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           03-06-15  Bugfix: only copy source basename if not in M$ mode. *
 *                                                                          *
 ****************************************************************************/

static void coff_filename(char *outfile, const char *srcfile)
{
    strcpy(coff_srcfile, options.microsoft ? srcfile : basename(srcfile));

    if (*outfile == '\0')
    {
        strcpy(outfile, basename(srcfile));
        update_extension_in_file(outfile, ".obj");
    }
}

/****************************************************************************
 *                                                                          *
 * Function: coff_filebeg                                                   *
 *                                                                          *
 * Purpose : Prepare to write to the output file.                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void coff_filebeg(void)
{
    sects = NULL;
    nscns = nscns_max = 0;

    syms = saa_init(COFF_SYMESZ);
    nsyms = 0;
    bsym = raa_init();
    symval = raa_init();

    strs = saa_init(1);
    strslen = 0;

    defseg = seg_alloc();
}

/****************************************************************************
 *                                                                          *
 * Function: coff_fileend                                                   *
 *                                                                          *
 * Purpose : Finish writing to the output file.                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void coff_fileend(bool_t abort)
{
    int i;

    if (!abort) write_object_file();

    for (i = 0; i < nscns; i++)
    {
        if (sects[i]->data)
            saa_free(sects[i]->data);
    }

    /*
     * We might get called multiple times, with the abort flag set.
     * Make sure we can handle this properly.
     */
    my_free(sects); sects = NULL; nscns = 0;
    saa_free(syms); syms = NULL;
    raa_free(bsym); bsym = NULL;
    raa_free(symval); symval = NULL;
    saa_free(strs); strs = NULL;
    fcn = NULL;
}

/****************************************************************************
 *                                                                          *
 * Function: coff_output                                                    *
 *                                                                          *
 * Purpose : Handle output of code or data.                                 *
 *                                                                          *
 * Comment : This procedure is called by assemble() to write actual         *
 *           generated code or data to the object file. Typically it        *
 *           doesn't have to actually *write* it, just store it for         *
 *           later. The 'type' argument specifies the type of output        *
 *           data, and usually the size as well.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           00-11-21  Support for ARM target added.                        *
 *                                                                          *
 ****************************************************************************/

static void coff_output(long segment, const void *data, ulong_t type, long segref)
{
    long rawbytes;
    SCNENTRY *s;
    int i;

    rawbytes = type & OUT_SIZMASK;
    type &= OUT_TYPMASK;

    /*
     * Handle absolute-assembly (structure definitions).
     */
    if (segment == NO_SEG)
    {
        if (type != OUT_RESERVE)
            apperror(RCERROR(ERROR_COFF_CODE_IN_ABS_SEGMENT));
        return;
    }

    /*
     * Lookup the segment entry.
     */
    s = NULL;
    for (i = 0; i < nscns; i++)
    {
        if (segment == sects[i]->segment)
        {
            s = sects[i];
            break;
        }
    }

    if (!s)
    {
        if (segment != coff_segment(COFF_TEXT, "", /*pass*/ 2, NULL))
            apperror(RCFATAL(ERROR_COFF_UNKNOWN_SEGMENT));
        else
            s = sects[nscns-1];
    }

    /*
     * Are we trying to emit data to a BSS section?
     */
    if (!s->data && type != OUT_RESERVE)
    {
        apperror(RCWARNING1(ERROR_COFF_DATA_IN_BSS_SEGMENT));

        if (type == OUT_REL2ADR)
            rawbytes = 2;
        else if (type == OUT_REL3ADR)
            rawbytes = 3;
        else if (type == OUT_REL4ADR)
            rawbytes = 4;

        s->sh.s_size += rawbytes;
        return;
    }

    /*
     * Handle the various output types.
     */
    if (type == OUT_RESERVE)
    {
        /*
         * Reserve x number of bytes.
         */
        if (s->data != NULL)
            cache_raw_data(s, NULL, rawbytes);
        else
            s->sh.s_size += rawbytes;
    }
    else if (type == OUT_RAWDATA)
    {
        /*
         * Output raw data.
         */
        assert(segref == NO_SEG);
        cache_raw_data(s, data, rawbytes);
    }
    else if (type == OUT_ADDRESS)
    {
        if (rawbytes != 2 && rawbytes != 4 && segref != NO_SEG)
        {
            apperror(RCERROR(ERROR_COFF_UNSUPPORTED_RELOC));
        }
        else
        {
            long tmp;

            if (segref != NO_SEG)
            {
                int type;

                if (s->sh.s_flags == DBG_FLAGS)
                {
                    if (strncmp(COFF_DEBUGF, s->sh.s_name, 8) == 0)
                        type = COFF_R_I386_DIR32NB;  /* X86 target only */
                    else if (rawbytes == 4)
                        type = (IS_TARGET_X86) ? COFF_R_I386_SECREL : COFF_R_ARM_SECREL;
                    else
                        type = (IS_TARGET_X86) ? COFF_R_I386_SECTION : COFF_R_ARM_SECTION;
                }
                else if (rawbytes == 4)
                    type = (IS_TARGET_X86) ? COFF_R_I386_DIR32 : COFF_R_ARM_ADDR32;
                else
                    type = COFF_R_I386_REL16;

                if ((segref & SEG_SYM) == 0 && segref % 2)
                    apperror(RCERROR(ERROR_COFF_NO_SEGMENT_BASE_REF));
                else
                    new_reloc(s, segref, type);
            }

            tmp = *(long *)data;
            cache_raw_data(s, &tmp, rawbytes);
        }
    }
    else if (type == OUT_REL2ADR)
    {
        apperror(RCERROR(ERROR_COFF_UNSUPPORTED_RELOC));
    }
    else if (type == OUT_REL3ADR)  /* ARM */
    {
        if (segref == segment)
        {
            apperror(RCFATAL(ERROR_INTERNAL), "coff_output()");
        }
        else if (segref == NO_SEG)
        {
            apperror(RCERROR(ERROR_COFF_NO_REL_TO_ABS_REF));
        }
        else
        {
            long tmp;

            if (segref != NO_SEG && (segref & SEG_SYM) == 0 && segref % 2)
                apperror(RCERROR(ERROR_COFF_NO_SEGMENT_BASE_REF));
            else
                new_reloc(s, segref, COFF_R_ARM_BRANCH24);

            tmp = *(long *)data + 4 - rawbytes;
            cache_raw_data(s, &tmp, 4L);
        }
    }
    else if (type == OUT_REL4ADR)  /* X86 */
    {
        if (segref == segment)
        {
            apperror(RCFATAL(ERROR_INTERNAL), "coff_output()");
        }
        else if (segref == NO_SEG)
        {
            apperror(RCERROR(ERROR_COFF_NO_REL_TO_ABS_REF));
        }
        else
        {
            long tmp;

            if (segref != NO_SEG && (segref & SEG_SYM) == 0 && segref % 2)
                apperror(RCERROR(ERROR_COFF_NO_SEGMENT_BASE_REF));
            else
                new_reloc(s, segref, COFF_R_I386_REL32);

            tmp = *(long *)data + 4 - rawbytes;
            cache_raw_data(s, &tmp, 4L);
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: coff_symbol                                                    *
 *                                                                          *
 * Purpose : Handle a symbol definition.                                    *
 *                                                                          *
 * Comment : This procedure is called once for every symbol defined in      *
 *           the module being assembled. It gives the name and value of     *
 *           the symbol, in POCC's terms, and indicates whether it has      *
 *           been declared to be global. Note that the parameter "name",    *
 *           when passed, will point to a piece of static storage           *
 *           allocated inside the label manager - it's safe to keep using   *
 *           that pointer, because the label manager doesn't clean up       *
 *           until after the output driver has.                             *
 *                                                                          *
 *           Values of 'type' are: SYMDEF_LOCAL (local), SYMDEF_GLOBAL      *
 *           (global), SYMDEF_COMMON (common, offset holds *size*) or       *
 *           SYMDEF_EXTERN (external).                                      *
 *                                                                          *
 *           This routine explicitly *is* allowed to call the label         *
 *           manager to define further symbols, if it wants to, even        *
 *           though it's been called *from* the label manager. That much    *
 *           re-entrancy is guaranteed in the label manager. However, the   *
 *           label manager will in turn call this routine, so it should     *
 *           be prepared to be re-entrant itself.                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static long coff_symbol(const char *name, long segment, long offset, int type)
{
    bool_t isfcn = FALSE;
    COFF_SYMENT *se;
    long symndx = nsyms;

    /*
     * Special labels begin with two periods (NASM). The only special
     * symbols that POCC supports must also be followed by a question mark.
     */
    if (name[0] == '.' && name[1] == '.')
    {
        if (name[2] != '?')
            apperror(RCERROR(ERROR_COFF_UNKNOWN_SPECIAL), name);
        return 0;
    }

    /* remember symbol index, if emitting debug info */
    if (fcn && fcn->symndx == 0 && strcmp(fcn->name, name) == 0)
        fcn->symndx = symndx, isfcn = TRUE;

    /*
     * Allocate and initialize a symbol entry.
     */
    se = saa_wstruct(syms);

    if (strlen(name) > COFF_SYMNMLEN)
    {
        saa_wbytes(strs, name, (long)(strlen(name)+1));
        se->n_offset = 4+strslen; strslen += strlen(name)+1;
        se->n_zeroes = 0;
    }
    else
    {
        strncpy(se->n_name, name, COFF_SYMNMLEN);
    }

    se->n_sclass = (type == SYMDEF_LOCAL) ? COFF_C_STAT : COFF_C_EXT;
    se->n_type = (isfcn) ? COFF_DT_FCN<<COFF_N_BTSHFT : COFF_T_NULL;
    se->n_numaux = 0;

    if (segment == NO_SEG)
    {
        se->n_scnum = COFF_N_ABS;
    }
    else
    {
        se->n_scnum = COFF_N_UNDEF;
        if (type != SYMDEF_EXTERN)
        {
            int i;

            for (i = 0; i < nscns; i++)
            {
                if (segment == sects[i]->segment)
                {
                    se->n_scnum = (1+i);
                    break;
                }
            }

            if (se->n_scnum == COFF_N_UNDEF)
                se->n_sclass = COFF_C_EXT;
        }
    }

    if (type == SYMDEF_COMMON)
        se->n_value = offset;
    else
        se->n_value = (se->n_scnum == COFF_N_UNDEF) ? 0 : offset;

    /*
     * define the references from external-symbol segment numbers to these symbol records.
     */
    if (se->n_scnum == COFF_N_UNDEF)
        bsym = raa_write(bsym, segment, nsyms);

    if (segment != NO_SEG)
        symval = raa_write(symval, segment, se->n_scnum ? 0 : se->n_value);

    nsyms++;

    /*
     * If this entry is for a function, and we are emitting debug info,
     * we need to output several extra symbol and auxiliary symbol entries.
     */
    if (isfcn && options.dbglevel > 0)
    {
        se->n_numaux = 1;
        fcn->se_fcn = se;

        /*
         * Output function symbol auxiliary entry.
         */
        fcn->ae_fcn = saa_wstruct(syms);
        fcn->ae_fcn->x_tagndx = 0;
        fcn->ae_fcn->x_tsize = 0;
        fcn->ae_fcn->x_lnnoptr = 0;
        fcn->ae_fcn->x_endndx = 0;

        nsyms++;

        /*
         * Output .bf symbol and auxiliary entries.
         */
        fcn->se_bf = saa_wstruct(syms);
        strcpy(fcn->se_bf->n_name, ".bf");
        fcn->se_bf->n_sclass = COFF_C_FCN;
        fcn->se_bf->n_type = COFF_T_NULL;
        fcn->se_bf->n_numaux = 1;
        fcn->se_bf->n_scnum = se->n_scnum;
        fcn->se_bf->n_value = 0;

        nsyms++;

        fcn->ae_bf = saa_wstruct(syms);
        fcn->ae_bf->x_tagndx = 0;
        fcn->ae_bf->x_lnno = 0;
        fcn->ae_bf->x_size = 0;
        fcn->ae_bf->x_lnnoptr = 0;
        fcn->ae_bf->x_endndx = 0;
        fcn->ae_bf->x_tvndx = 0;

        nsyms++;

        /*
         * Output .lf symbol entry.
         */
        fcn->se_lf = saa_wstruct(syms);
        strcpy(fcn->se_lf->n_name, ".lf");
        fcn->se_lf->n_sclass = COFF_C_FCN;
        fcn->se_lf->n_type = COFF_T_NULL;
        fcn->se_lf->n_numaux = 0;
        fcn->se_lf->n_scnum = se->n_scnum;
        fcn->se_lf->n_value = 0;

        nsyms++;

        /*
         * Output .ef symbol and auxiliary entries.
         */
        fcn->se_ef = saa_wstruct(syms);
        strcpy(fcn->se_ef->n_name, ".ef");
        fcn->se_ef->n_sclass = COFF_C_FCN;
        fcn->se_ef->n_type = COFF_T_NULL;
        fcn->se_ef->n_numaux = 1;
        fcn->se_ef->n_scnum = se->n_scnum;
        fcn->se_ef->n_value = 0;

        nsyms++;

        fcn->ae_ef = saa_wstruct(syms);
        fcn->ae_ef->x_tagndx = 0;
        fcn->ae_ef->x_lnno = 0;
        fcn->ae_ef->x_size = 0;
        fcn->ae_ef->x_lnnoptr = 0;
        fcn->ae_ef->x_endndx = 0;
        fcn->ae_ef->x_tvndx = 0;

        nsyms++;
    }

    return symndx+1;  /* non-zero */
}

/****************************************************************************
 *                                                                          *
 * Function: coff_segment                                                   *
 *                                                                          *
 * Purpose : Handle a segment change in the source code.                    *
 *                                                                          *
 * Comment : This procedure is called when the source code requests a       *
 *           segment change. It should return the corresponding segment     *
 *           number for the name, or NO_SEG if the name is not a valid      *
 *           segment name.                                                  *
 *                                                                          *
 *           It may also be called with NULL, in which case it is to        *
 *           return the *default* segment number for starting assembly in.  *
 *                                                                          *
 *           It is also allowed to specify a default instruction size for   *
 *           the segment, by setting '*bits' to 16 or 32. Or, if it         *
 *           doesn't wish to define a default, it can leave 'bits' alone.   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           00-11-26  Section COFF_PDATA added.                            *
 *           01-08-20  Section COFF_RDATA added.                            *
 *           02-01-14  Section COFF_XDATA added.                            *
 *           03-08-01  Sections .CRT$XIC and .CRT$XTC added.                *
 *           04-03-14  Support for user-defined segment names added.        *
 *                                                                          *
 ****************************************************************************/

static long coff_segment(const char *name, const char *attr, int pass, int *bits)
{
    char sname[9];
    int i;

    if (!name && bits)
        *bits = 32;

    if (!name)
        return defseg;

    if (strlen(name) > 8)
    {
        if (pass == 1)
            apperror(RCWARNING1(ERROR_COFF_SECTNAME_TOO_LONG), name);
        name = strncpy(sname, name, 8);
        sname[8] = '\0';
    }

    for (i = 0; i < nscns; i++)
        if (strncmp(name, sects[i]->sh.s_name, 8) == 0)
            break;

    if (i == nscns)
    {
        ulong_t flags;

        if (strcmp(name, COFF_TEXT) == 0)
            flags = TEXT_FLAGS;
        else if (strcmp(name, COFF_DATA) == 0)
            flags = DATA_FLAGS;
        else if (strcmp(name, COFF_RDATA) == 0)
            flags = RDATA_FLAGS;
        else if (strcmp(name, COFF_BSS) == 0)
            flags = BSS_FLAGS;
        else if (strcmp(name, COFF_DEBUGF) == 0)
            flags = DBG_FLAGS;
        else if (strcmp(name, COFF_DEBUGS) == 0)
            flags = DBG_FLAGS;
        else if (strcmp(name, COFF_DEBUGT) == 0)
            flags = DBG_FLAGS;
        else if (strcmp(name, COFF_PDATA) == 0)
            flags = RDATA_FLAGS;
        else if (strcmp(name, COFF_XDATA) == 0)
            flags = RDATA_FLAGS;
        else if (strcmp(name, COFF_DRECTVE) == 0)
            flags = INFO_FLAGS;
        else if (strcmp(name, ".CRT$XIC") == 0)
            flags = RDATA_FLAGS;
        else if (strcmp(name, ".CRT$XTC") == 0)
            flags = RDATA_FLAGS;
        else
        {
            for (flags = 0; attr && *attr; attr++)
            {
                switch (*attr)
                {
                    case 'e': case 'E': flags |= COFF_STYP_EXEC; break;
                    case 'r': case 'R': flags |= COFF_STYP_READ; break;
                    case 'w': case 'W': flags |= COFF_STYP_WRITE; break;
                    default:
                        apperror(RCWARNING1(ERROR_COFF_UNKNOWN_SECT_ATTRIB), *attr);
                        break;
                }
            }
            if (flags == 0)
                flags = TEXT_FLAGS;  /* default like older versions */
            else if (flags & COFF_STYP_EXEC)
                flags |= COFF_STYP_TEXT|COFF_STYP_A16;
            else
                flags |= COFF_STYP_DATA|COFF_STYP_A4;
        }

        i = new_section(name, flags);
        sects[i]->sh.s_flags = flags;
    }

    return sects[i]->segment;
}

/****************************************************************************
 *                                                                          *
 * Function: coff_segbase                                                   *
 *                                                                          *
 * Purpose : Handle a segment base change (pretty complex!).                *
 *                                                                          *
 * Comment : This procedure is called to modify the segment base values     *
 *           returned from the SEG operator. It is given a segment base     *
 *           value (i.e. a segment value with the low bit set), and is      *
 *           required to produce in return a segment value which may be     *
 *           different. It can map segment bases to absolute numbers by     *
 *           means of returning SEG_ABS types.                              *
 *                                                                          *
 *           It should return NO_SEG if the segment base cannot be          *
 *           determined; the evaluator (which calls this routine) is        *
 *           responsible for throwing an error condition if that occurs     *
 *           in pass two or in a critical expression.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static long coff_segbase(long segment)
{
    return segment;
}

/****************************************************************************
 *                                                                          *
 * Function: coff_directive                                                 *
 *                                                                          *
 * Purpose : Handle any driver specific directives.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           00-11-21  Bugfix: skip initial escape character ($).           *
 *                                                                          *
 ****************************************************************************/

static bool_t coff_directive(const char *keyword, const char *value, int pass)
{
    if (strcmp(keyword, "function") == 0)  /* [FUNCTION name] */
    {
        if (*value == '$') value++;  /* skip initial $ */

        if (pass == 2)
        {
            fcn = memalloc(sizeof(*fcn) + strlen(value), PERM);
            memset(fcn, 0, sizeof(*fcn));
            strcpy(fcn->name, value);

            fcnlist = listappend(fcn, fcnlist);
        }
        else if (pass == 3)
        {
            LIST *lp = fcnlist;
            do
            {
                lp = lp->link;
                fcn = (FCNENTRY *)lp->data;
                if (strcmp(fcn->name, value) == 0) break;
            } while (lp != fcnlist);

            if (fcn->se_fcn != NULL)
                fcn->size = sects[fcn->se_fcn->n_scnum-1]->sh.s_size;
        }
        return TRUE;
    }

    return FALSE;
}

/****************************************************************************
 *                                                                          *
 * Function: coff_lineno                                                    *
 *                                                                          *
 * Purpose : Handle a linenumber definition.                                *
 *                                                                          *
 * Comment : This procedure is called once for every source line defined    *
 *           in the module being assembled (during pass 3).                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void coff_lineno(long segment, int lineno)
{
    SCNENTRY *s;
    int i;

    /* linenumbers are always relative a function */
    if (!fcn) return;

    s = NULL;
    for (i = 0; i < nscns; i++)
    {
        if (segment == sects[i]->segment)
        {
            s = sects[i];
            break;
        }
    }

    if (!s)
    {
        if (segment != coff_segment(COFF_TEXT, "", /*pass*/ 2, NULL))
            apperror(RCFATAL(ERROR_COFF_UNKNOWN_SEGMENT));
        else
            s = sects[nscns-1];
    }

    if (fcn->lineno == 0)
        fcn->lineno = fcn->lineno_last = lineno, lineno = 0;  /* line # on opening brace */
    else if (fcn->lineno_last < lineno)
        fcn->lineno_last = lineno;

    new_lineno(s, lineno);
}

/****************************************************************************
 *                                                                          *
 * Function: cache_raw_data                                                 *
 *                                                                          *
 * Purpose : Cache raw data for the given section.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void cache_raw_data(SCNENTRY *s, const void *data, long size)
{
    saa_wbytes(s->data, data, size);
    s->sh.s_size += size;
}

/****************************************************************************
 *                                                                          *
 * Function: write_object_file                                              *
 *                                                                          *
 * Purpose : Write the entire object file.                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           00-11-21  Support for ARM target added.                        *
 *           01-07-27  Removed unnecessary update of s_vsize field.         *
 *           04-12-14  Support for extended relocations added.              *
 *                                                                          *
 ****************************************************************************/

static void write_object_file(void)
{
    COFF_FILHDR hdr;
    long filepos, size;
    int i;

    /* Calculate number of symbol entries needed for the filename */
    nfilesyms = (options.microsoft) ? 1 + (strlen(coff_srcfile) + COFF_AUXESZ) / COFF_AUXESZ : 2;

    /*
     * Work out how big the file will get. Calculate the
     * start of the 'real' symbols at the same time.
     */
    filepos = COFF_FILHSZ + COFF_SCNHSZ * nscns;
    ndefsyms = nfilesyms;

    for (i = 0; i < nscns; i++)
    {
        if (sects[i]->data != NULL)
        {
            /* finalize standard or extended relocation count */
            finalize_relocation_count(sects[i]);

            sects[i]->sh.s_scnptr = filepos;
            filepos += sects[i]->sh.s_size;
            sects[i]->sh.s_relptr = filepos;
            filepos += sects[i]->nreloc * COFF_RELSZ;
            sects[i]->sh.s_lnnoptr = filepos;
            filepos += sects[i]->sh.s_nlnno * COFF_LINESZ;
        }
        else
        {
            sects[i]->sh.s_scnptr = sects[i]->sh.s_relptr = sects[i]->sh.s_lnnoptr = 0;
        }

        /* two for each section */
        ndefsyms += 2;
    }

    /*
     * Output the file header.
     */
    hdr.f_magic = (IS_TARGET_X86) ? COFF_F_MAG_I386 : COFF_F_MAG_ARM;
    hdr.f_nscns = nscns;
    hdr.f_timdat = time(NULL);
    hdr.f_symptr = filepos;
    hdr.f_nsyms = nsyms + ndefsyms;
    hdr.f_opthdr = 0;
    hdr.f_flags = COFF_F_32BIT;
    fwrite(&hdr, COFF_FILHSZ, 1, fdo);

    /*
     * Output the section headers.
     */
    for (i = 0; i < nscns; i++)
    {
        fwrite(&sects[i]->sh, COFF_SCNHSZ, 1, fdo);
    }

    /*
     * Output the sections and their relocations.
     */
    for (i = 0; i < nscns; i++)
    {
        if (sects[i]->data != NULL)
        {
            saa_fpwrite(sects[i]->data, fdo);
            write_relocations(sects[i]);
            write_linenumbers(sects[i]);
        }
    }

    /*
     * Output the symbol and string tables.
     */
    write_symbol_table();

    size = sizeof(size) + strslen;
    fwrite(&size, sizeof(size), 1, fdo);  /* length includes length count */
    saa_fpwrite(strs, fdo);
}

/****************************************************************************
 *                                                                          *
 * Function: write_relocations                                              *
 *                                                                          *
 * Purpose : Write relocations for a section.                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void write_relocations(SCNENTRY *s)
{
    if (s->rlist)
    {
        RELENTRY *r = s->rlist;
        do
        {
            r = r->link;

            switch (r->symtype)
            {
                /* relocate the relocation symbol ;) */
                case REAL_SYMBOL: r->re.r_symndx += ndefsyms; break;
                case SECT_SYMBOL: r->re.r_symndx += nfilesyms; break;
            }

            fwrite(&r->re, COFF_RELSZ, 1, fdo);

        } while (r != s->rlist);
    }
}

/****************************************************************************
 *                                                                          *
 * Function: write_linenumbers                                              *
 *                                                                          *
 * Purpose : Write linenumbers for a section.                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void write_linenumbers(SCNENTRY *s)
{
    if (s->llist)
    {
        LINENTRY *l = s->llist;
        do
        {
            l = l->link;

            if (l->le.l_lnno == 0)
                l->le.l_symndx += ndefsyms;

            fwrite(&l->le, COFF_LINESZ, 1, fdo);

        } while (l != s->llist);
    }
}

/****************************************************************************
 *                                                                          *
 * Function: write_symbol_table                                             *
 *                                                                          *
 * Purpose : Write the symbol table.                                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           03-04-26  Bugfix: lnnopos added for correct offset.            *
 *                                                                          *
 ****************************************************************************/

static void write_symbol_table(void)
{
    COFF_SYMENT se;
    COFF_AUXENT ae;
    short i;
    ulong_t j;
    char *p;

    /*
     * The '.file' record, followed by the file auxiliary record(s).
     */
    strncpy(se.n_name, ".file", COFF_SYMNMLEN);
    se.n_value = 0;
    se.n_scnum = COFF_N_DEBUG;
    se.n_type = COFF_T_NULL;
    se.n_sclass = COFF_C_FILE;
    se.n_numaux = nfilesyms-1;
    fwrite(&se, COFF_SYMESZ, 1, fdo);

    for (p = coff_srcfile, i = 0; i < se.n_numaux; i++, p += COFF_AUXESZ)
    {
        strncpy(ae.x_fname, p, sizeof(ae.x_fname));
        fwrite(&ae, COFF_AUXESZ, 1, fdo);
    }

    /*
     * The section records, with their auxiliaries.
     */
    for (i = 0; i < nscns; i++)
    {
        strncpy(se.n_name, sects[i]->sh.s_name, COFF_SYMNMLEN);
        se.n_value = 0;
        se.n_scnum = (short)(i+1);
        se.n_type = COFF_T_NULL;
        se.n_sclass = COFF_C_STAT;
        se.n_numaux = 1;
        fwrite(&se, COFF_SYMESZ, 1, fdo);

        ae.x_ssize = sects[i]->sh.s_size;
        ae.x_nreloc = sects[i]->sh.s_nreloc;
        ae.x_nlnno = sects[i]->sh.s_nlnno;
        ae.x_chksum = 0;
        ae.x_scnum = 0;
        ae.x_sel = 0;
        fwrite(&ae, COFF_AUXESZ, 1, fdo);
    }

    /*
     * Update function symbols (debug info).
     */
    if (fcnlist && options.dbglevel > 0)
    {
        LIST *lp = fcnlist;
        FCNENTRY *nxt;
        SCNENTRY *s;
        long lnnopos = 0;
        do
        {
            lp = lp->link;

            fcn = (FCNENTRY *)lp->data;
            nxt = (lp != fcnlist) ? (FCNENTRY *)lp->link->data : NULL;
            s = sects[fcn->se_fcn->n_scnum-1];

            fcn->ae_fcn->x_tagndx = ndefsyms + fcn->symndx+2;
            fcn->ae_fcn->x_endndx = (nxt) ? ndefsyms + nxt->symndx : 0;
            fcn->ae_fcn->x_tsize = ((nxt) ? nxt->size : s->sh.s_size) - fcn->size - 1;
            fcn->ae_fcn->x_lnnoptr = s->sh.s_lnnoptr + lnnopos;
            fcn->se_bf->n_value = s->sh.s_nlnno;
            fcn->ae_bf->x_lnno = fcn->lineno;
            fcn->ae_bf->x_endndx = (nxt) ? ndefsyms + nxt->symndx+2 : 0;
            fcn->se_lf->n_value = s->sh.s_nlnno;
            fcn->se_ef->n_value = fcn->ae_fcn->x_tsize;
            fcn->ae_ef->x_lnno = fcn->lineno_last;

            lnnopos += fcn->nlineno * sizeof(COFF_LINENO);
        } while (lp != fcnlist);
    }

    /*
     * The real symbols.
     */
    saa_rewind(syms);
    for (j = 0; j < nsyms; j++)
    {
        /* write a normal or auxiliary entry */
        fwrite(saa_rstruct(syms), COFF_SYMESZ, 1, fdo);
    }
}

/****************************************************************************
 *                                                                          *
 * Function: new_section                                                    *
 *                                                                          *
 * Purpose : Add a new section to the file.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int new_section(const char *name, ulong_t flags)
{
    SCNENTRY *s;

    s = memalloc(sizeof(*s), PERM);
    memset(s, 0, sizeof(*s));

    s->data = (flags != BSS_FLAGS) ? saa_init(1) : NULL;
    s->segment = (strcmp(name, COFF_TEXT) == 0) ? defseg : seg_alloc();
    strncpy(s->sh.s_name, name, sizeof(s->sh.s_name));
    s->sh.s_flags = flags;

    if (nscns == nscns_max)
    {
        nscns_max += SECT_DELTA;
        sects = my_realloc(sects, nscns_max * sizeof(*sects));
    }

    sects[nscns] = s;
    return nscns++;
}

/****************************************************************************
 *                                                                          *
 * Function: new_reloc                                                      *
 *                                                                          *
 * Purpose : Add a new symbol relocation to the file.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *           04-12-14  Use long counter to handle extended relocations.     *
 *                                                                          *
 ****************************************************************************/

static void new_reloc(SCNENTRY *s, long segment, int type)
{
    RELENTRY *r;
    int i;

    r = memalloc(sizeof(*r), PERM);

    if (s->rlist != NULL)
    {
        r->link = s->rlist->link;
        s->rlist->link = r;
    }
    else
    {
        r->link = r;
    }

    s->rlist = r;
    s->nreloc++;

    r->re.r_vaddr = s->sh.s_size;
    r->re.r_type = type;

    r->symtype = REAL_SYMBOL;
    for (i = 0; i < nscns; i++)
    {
        if (segment == sects[i]->segment)
        {
            r->re.r_symndx = i*2;
            r->symtype = SECT_SYMBOL;
            break;
        }
    }

    if (r->symtype == REAL_SYMBOL)
    {
        if (segment & SEG_SYM)
            r->re.r_symndx = (segment & ~SEG_SYM) - 1;
        else
            r->re.r_symndx = raa_read(bsym, segment);
    }
}

/****************************************************************************
 *                                                                          *
 * Function: finalize_relocation_count                                      *
 *                                                                          *
 * Purpose : Set the final relocation count - standard or extended.         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           04-12-14  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void finalize_relocation_count(SCNENTRY *s)
{
    if (s->nreloc > 0xFFFF)
    {
        /* relocation count won't fit in standard field */
        RELENTRY *r;

        r = memalloc(sizeof(*r), PERM);

        r->link = s->rlist->link;
        s->rlist->link = r;

        s->nreloc++;

        /* first relocation entry holds the count */
        r->re.r_vaddr = s->nreloc;
        r->re.r_type = 0;
        r->re.r_symndx = 0;
        r->symtype = SECT_SYMBOL;

        /* mark as extended relocation */
        s->sh.s_nreloc = 0xFFFF;
        s->sh.s_flags |= COFF_STYP_XRELOC;
    }
    else
    {
        /* relocation count fits in standard field */
        s->sh.s_nreloc = (ushort_t)s->nreloc;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: new_lineno                                                     *
 *                                                                          *
 * Purpose : Add a new linenumber to the file.                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void new_lineno(SCNENTRY *s, int lineno)
{
    LINENTRY *l;

    if (lineno != 0)
    {
        if (lineno == fcn->lineno)
            return;

        /* line numbers and addresses must always increase */
        if (s->llist && s->llist->le.l_lnno >= (lineno - fcn->lineno))
            return;
    }

    l = memalloc(sizeof(*l), PERM);

    if (s->llist != NULL)
    {
        l->link = s->llist->link;
        s->llist->link = l;
    }
    else
    {
        l->link = l;
    }

    s->llist = l;
    s->sh.s_nlnno++;
    fcn->nlineno++;

    if (lineno == 0)
    {
        l->le.l_symndx = fcn->symndx;
        l->le.l_lnno = 0;
    }
    else
    {
        l->le.l_paddr = s->sh.s_size;
        l->le.l_lnno = lineno - fcn->lineno;
    }
}

OUTFMT x86coffOF = {
    TRUE,
    x86_init,
    assembler,
    coff_filename,
    coff_filebeg,
    coff_fileend,
    coff_output,
    coff_symbol,
    coff_segment,
    coff_segbase,
    coff_directive,
    coff_lineno
};

OUTFMT armcoffOF = {
    TRUE,
    arm_init,
    assembler,
    coff_filename,
    coff_filebeg,
    coff_fileend,
    coff_output,
    coff_symbol,
    coff_segment,
    coff_segbase,
    coff_directive,
    coff_lineno
};

