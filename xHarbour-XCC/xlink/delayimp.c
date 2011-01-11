/****************************************************************************
 *                                                                          *
 * File    : delayimp.c                                                     *
 *                                                                          *
 * Purpose : Win32 Linker; delay load import management (X86 only).         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           99-02-14  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "link.h"

#define MAX_IMPORT_OBJECT_SIZE  0x0020000   /* 128 KB */

/* Locals */
static char *strptr = NULL;

/* Static function prototypes */
static void write_symbol_name(COFF_SYMENT *, const char *, void **);

/****************************************************************************
 *                                                                          *
 * Function: build_delay_import_module_object                               *
 *                                                                          *
 * Purpose : Build per-module delay import object module.                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           99-02-14  Created                                              *
 *                                                                          *
 ****************************************************************************/

void build_delay_import_module_object(void *ip, const char *modname)
{
    COFF_FILHDR *fhdr;
    COFF_SCNHDR *sh;
    COFF_RELOC *re;
    COFF_SYMENT *se;
    void *base;
    void *ip_raw;
    char name[MAX_PATH];
    char *dot = strchr(modname, '.');

    base = ip;

    /*
     * Write the file header.
     */
    fhdr = (COFF_FILHDR *)ip;
    ip = ((COFF_FILHDR *)ip + 1);

    fhdr->f_magic = COFF_F_MAG_I386;
    fhdr->f_nscns = 6 + (options.delay_bind ? 1 : 0) + (options.delay_unload ? 1 : 0);
    fhdr->f_timdat = time_stamp;
    /* fhdr->f_symptr = 0; */
    fhdr->f_nsyms = 13 + (options.delay_bind ? 2 : 0) + (options.delay_unload ? 2 : 0);
    fhdr->f_opthdr = 0;
    fhdr->f_flags = COFF_F_32BIT;

    /*
     * Reserve room for the sections headers.
     */
    sh = (COFF_SCNHDR *)ip;
    ip = ((COFF_SCNHDR *)ip + fhdr->f_nscns);

    /*
     * Write section header #1.
     */
    strncpy(sh->s_name, COFF_DIDAT2, 8);
    sh->s_vsize = 0;
    sh->s_vaddr = 0;
    sh->s_size = sizeof(COFF_DELAY);
    sh->s_scnptr = ((char *)ip - (char *)base);
    sh->s_relptr = ((char *)ip - (char *)base) + sh->s_size;
    sh->s_lnnoptr = 0;
    sh->s_nreloc = 4 + (options.delay_bind ? 1 : 0) + (options.delay_unload ? 1 : 0);
    sh->s_nlnno = 0;
    sh->s_flags = COFF_DATA_FLAGS|COFF_STYP_A4;

    /*
     * Write a Import Directory entry.
     */
    memset(ip, 0, sizeof(COFF_DELAY));
    ip = (char *)ip + sizeof(COFF_DELAY);

    /*
     * Reserve room for the relocations.
     */
    re = (COFF_RELOC *)ip;
    ip = ((COFF_RELOC *)ip + sh->s_nreloc);

    /*
     * Write relocations.
     */
    re->r_vaddr = offsetof(COFF_DELAY, di_modname);
    re->r_symndx = 2;
    re->r_type = COFF_R_I386_DIR32;
    re++;

    re->r_vaddr = offsetof(COFF_DELAY, di_hmod);
    re->r_symndx = 12;
    re->r_type = COFF_R_I386_DIR32;
    re++;

    re->r_vaddr = offsetof(COFF_DELAY, di_addrtab);
    re->r_symndx = 4;
    re->r_type = COFF_R_I386_DIR32;
    re++;

    re->r_vaddr = offsetof(COFF_DELAY, di_nametab);
    re->r_symndx = 3;
    re->r_type = COFF_R_I386_DIR32;
    re++;

    if (options.delay_bind)
    {
        re->r_vaddr = offsetof(COFF_DELAY, di_baddrtab);
        re->r_symndx = 13;
        re->r_type = COFF_R_I386_DIR32;
        re++;
    }
    if (options.delay_unload)
    {
        re->r_vaddr = offsetof(COFF_DELAY, di_uaddrtab);
        re->r_symndx = (options.delay_bind) ? 15 : 13;
        re->r_type = COFF_R_I386_DIR32;
        re++;
    }

    sh++;

    /*
     * Write section header #2.
     */
    strncpy(sh->s_name, COFF_DIDAT4, 8);
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
     * Write section header #3.
     */
    strncpy(sh->s_name, COFF_DIDAT5, 8);
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
     * Write section header #4.
     */
    strncpy(sh->s_name, COFF_DIDAT6, 8);
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

    strcpy(ip, modname);
    ip = (char *)ip + strlen(ip)+1;

    /* "Hint/Name tables are WORD aligned" */
    ip = align_word_ptr(ip);

    /*
     * Update the section header.
     */
    sh->s_size = ((char *)ip - (char *)ip_raw);
    sh++;

    /*
     * Write section header #5.
     */
    strncpy(sh->s_name, COFF_TEXT, 8);
    sh->s_vsize = 0;
    sh->s_vaddr = 0;
    sh->s_size = 14;
    sh->s_scnptr = ((char *)ip - (char *)base);
    sh->s_relptr = ((char *)ip - (char *)base) + sh->s_size;
    sh->s_lnnoptr = 0;
    sh->s_nreloc = 2;
    sh->s_nlnno = 0;
    sh->s_flags = COFF_TEXT_FLAGS|COFF_STYP_A1;

    /*
     * __tailMerge_XXX:
     * push "__DELAY_IMPORT_DESCRIPTOR_XXX"
     * call "___delayLoadHelper@8"
     * pop  edx
     * pop  ecx
     * jmp  eax
     */
#define TAILMERGE_CODE  "\x68\0\0\0\0\xE8\0\0\0\0\x5A\x59\xFF\xE0"
#define TAILMERGE_SIZE  sizeof(TAILMERGE_CODE)-1
    memcpy(ip, TAILMERGE_CODE, TAILMERGE_SIZE);
    ip = (char *)ip + TAILMERGE_SIZE;
#undef TAILMERGE_CODE
#undef TAILMERGE_SIZE

    /*
     * Reserve room for the relocations.
     */
    re = (COFF_RELOC *)ip;
    ip = ((COFF_RELOC *)ip + 2);

    /*
     * Write relocations.
     */
    re->r_vaddr = 1;
    re->r_symndx = 0;
    re->r_type = COFF_R_I386_DIR32;
    re++;

    re->r_vaddr = 6;
    re->r_symndx = 9;
    re->r_type = COFF_R_I386_REL32;
    re++;

    sh++;

    /*
     * Write section header #6.
     */
    strncpy(sh->s_name, COFF_DATA, 8);
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

    if (options.delay_bind)
    {
        /*
         * Write section header #7.
         */
        strncpy(sh->s_name, COFF_DIDAT7, 8);
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
    }

    if (options.delay_unload)
    {
        /*
         * Write section header #7/#8.
         */
        strncpy(sh->s_name, COFF_DIDAT8, 8);
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
     * Write symbol table.
     */
    sprintf(name, "__DELAY_IMPORT_DESCRIPTOR_%.*s",
        (dot) ? dot-modname : strlen(modname), modname);

    write_symbol_name(se, name, &ip);
    se->n_value = 0;
    se->n_scnum = 1;
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_EXT;
    se->n_numaux = 0;
    se++;  /* 0++ */

    strncpy(se->n_name, COFF_DIDAT2, 8);
    se->n_value = COFF_DATA_FLAGS;
    se->n_scnum = 1;
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_SECT;
    se->n_numaux = 0;
    se++;  /* 1++ */

    strncpy(se->n_name, COFF_DIDAT6, 8);
    se->n_value = 0;
    se->n_scnum = 4;
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_STAT;
    se->n_numaux = 0;
    se++;  /* 2++ */

    strncpy(se->n_name, COFF_DIDAT4, 8);
    se->n_value = COFF_DATA_FLAGS;
    se->n_scnum = COFF_N_UNDEF;
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_SECT;
    se->n_numaux = 0;
    se++;  /* 3++ */

    strncpy(se->n_name, COFF_DIDAT5, 8);
    se->n_value = COFF_DATA_FLAGS;
    se->n_scnum = COFF_N_UNDEF;
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_SECT;
    se->n_numaux = 0;
    se++;  /* 4++ */

    write_symbol_name(se, "__NULL_DELAY_IMPORT_DESCRIPTOR", &ip);
    se->n_value = 0;
    se->n_scnum = COFF_N_UNDEF;
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_EXT;
    se->n_numaux = 0;
    se++;  /* 5++ */

    sprintf(name, "\177%.*s_NULL_THUNK_DATA_DLA",
        (dot) ? dot-modname : strlen(modname), modname);

    write_symbol_name(se, name, &ip);
    se->n_value = 0;
    se->n_scnum = 2;
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_EXT;
    se->n_numaux = 0;
    se++;  /* 6++ */

    sprintf(name, "\177%.*s_NULL_THUNK_DATA_DLN",
        (dot) ? dot-modname : strlen(modname), modname);

    write_symbol_name(se, name, &ip);
    se->n_value = 0;
    se->n_scnum = 3;
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_EXT;
    se->n_numaux = 0;
    se++;  /* 7++ */

    strncpy(se->n_name, COFF_TEXT, 8);
    se->n_value = COFF_TEXT_FLAGS;
    se->n_scnum = 5;
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_SECT;
    se->n_numaux = 0;
    se++;  /* 8++ */

    write_symbol_name(se, "___delayLoadHelper@8", &ip);
    se->n_value = 0;
    se->n_scnum = COFF_N_UNDEF;
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_EXT;
    se->n_numaux = 0;
    se++;  /* 9++ */

    sprintf(name, "__tailMerge_%.*s",
        (dot) ? dot-modname : strlen(modname), modname);

    write_symbol_name(se, name, &ip);
    se->n_value = 0;
    se->n_scnum = 5;
    /* se->n_type = COFF_T_NULL; */
    se->n_type = (COFF_DT_FCN << COFF_N_BTSHFT);
    se->n_sclass = COFF_C_EXT;
    se->n_numaux = 0;
    se++;  /* 10++ */

    strncpy(se->n_name, COFF_DATA, 8);
    se->n_value = COFF_DATA_FLAGS;
    se->n_scnum = 6;
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_SECT;
    se->n_numaux = 0;
    se++;  /* 11++ */

    sprintf(name, "__hmod__%.*s",
        (dot) ? dot-modname : strlen(modname), modname);

    write_symbol_name(se, name, &ip);
    se->n_value = 0;
    se->n_scnum = 6;
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_EXT;
    se->n_numaux = 0;
    se++;  /* 12++ */

    if (options.delay_bind)
    {
        strncpy(se->n_name, COFF_DIDAT7, 8);
        se->n_value = COFF_DATA_FLAGS;
        se->n_scnum = COFF_N_UNDEF;
        se->n_type = COFF_T_NULL;
        se->n_sclass = COFF_C_SECT;
        se->n_numaux = 0;
        se++;  /* 13++ */

        sprintf(name, "\177%.*s_NULL_THUNK_DATA_DLB",
            (dot) ? dot-modname : strlen(modname), modname);

        write_symbol_name(se, name, &ip);
        se->n_value = 0;
        se->n_scnum = 7;
        se->n_type = COFF_T_NULL;
        se->n_sclass = COFF_C_EXT;
        se->n_numaux = 0;
        se++;  /* 14++ */
    }

    if (options.delay_unload)
    {
        strncpy(se->n_name, COFF_DIDAT8, 8);
        se->n_value = COFF_DATA_FLAGS;
        se->n_scnum = COFF_N_UNDEF;
        se->n_type = COFF_T_NULL;
        se->n_sclass = COFF_C_SECT;
        se->n_numaux = 0;
        se++;  /* 13++ / 15++ */

        sprintf(name, "\177%.*s_NULL_THUNK_DATA_DLU",
            (dot) ? dot-modname : strlen(modname), modname);

        write_symbol_name(se, name, &ip);
        se->n_value = 0;
        se->n_scnum = (options.delay_bind) ? 8 : 7;
        se->n_type = COFF_T_NULL;
        se->n_sclass = COFF_C_EXT;
        se->n_numaux = 0;
        se++;  /* 14++ / 16++ */
    }

    *(long *)strptr = ((char *)ip - strptr);
}

/****************************************************************************
 *                                                                          *
 * Function: build_delay_import_null_thunk_object                           *
 *                                                                          *
 * Purpose : Build global delay import null thunk object module.            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           99-02-14  Created                                              *
 *                                                                          *
 ****************************************************************************/

void build_delay_import_null_thunk_object(void *ip)
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
    strncpy(sh->s_name, COFF_DIDAT3, 8);
    sh->s_vsize = 0;
    sh->s_vaddr = 0;
    sh->s_size = sizeof(COFF_DELAY);
    sh->s_scnptr = ((char *)ip - (char *)base);
    sh->s_relptr = 0;
    sh->s_lnnoptr = 0;
    sh->s_nreloc = 0;
    sh->s_nlnno = 0;
    sh->s_flags = COFF_DATA_FLAGS|COFF_STYP_A4;

    /*
     * Write the NULL Import Directory entry.
     */
    memset(ip, 0, sizeof(COFF_DELAY));
    ip = (char *)ip + sizeof(COFF_DELAY);

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
    write_symbol_name(se, "__NULL_DELAY_IMPORT_DESCRIPTOR", &ip);
    se->n_value = 0;
    se->n_scnum = 1;
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_EXT;
    se->n_numaux = 0;

    *(long *)strptr = ((char *)ip - strptr);
}

/****************************************************************************
 *                                                                          *
 * Function: build_delay_import_object                                      *
 *                                                                          *
 * Purpose : Build per-symbol delay import object module.                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           99-02-14  Created                                              *
 *                                                                          *
 ****************************************************************************/

void build_delay_import_object(void *ip, const char *modname,
    const char *expname, const char *symname)
{
    /*
     * <expname> is the mangled name (_symbol@nn); always a function.
     * <symname> is the original name (_symbol).
     */
    COFF_FILHDR *fhdr;
    COFF_SCNHDR *sh;
    COFF_RELOC *re;
    COFF_SYMENT *se;
    COFF_AUXENT *ae;
    void *base;
    void *ip_raw;
    char name[MAX_PATH];
    char *dot = strchr(modname, '.');

    base = ip;

    /*
     * Write the file header.
     */
    fhdr = (COFF_FILHDR *)ip;
    ip = ((COFF_FILHDR *)ip + 1);

    fhdr->f_magic = COFF_F_MAG_I386;
    fhdr->f_nscns = 4 + (options.delay_bind ? 1 : 0) + (options.delay_unload ? 1 : 0);
    fhdr->f_timdat = time_stamp;
    /* fhdr->f_symptr = 0; */
    fhdr->f_nsyms = 13 + (options.delay_bind ? 2 : 0) + (options.delay_unload ? 2 : 0);
    fhdr->f_opthdr = 0;
    fhdr->f_flags = COFF_F_32BIT;

    /*
     * Reserve room for the sections headers.
     */
    sh = (COFF_SCNHDR *)ip;
    ip = ((COFF_SCNHDR *)ip + fhdr->f_nscns);

    /*
     * Write section header #1.
     */
    strncpy(sh->s_name, COFF_TEXT, 8);
    sh->s_vsize = 0;
    sh->s_vaddr = 0;
    sh->s_size = 6 + 12;
    sh->s_scnptr = ((char *)ip - (char *)base);
    sh->s_relptr = ((char *)ip - (char *)base) + sh->s_size;
    sh->s_lnnoptr = 0;
    sh->s_nreloc = 3;
    sh->s_nlnno = 0;
    sh->s_flags = COFF_TEXT_FLAGS|COFF_STYP_COMDAT|COFF_STYP_A1;

    /*
     * jmp dword ptr ds:["__imp_symbol"]
     * __imp_load__symbol:
     * push ecx
     * push edx
     * push "__imp_symbol"
     * jmp  "__tailMerge_XXX"
     */
#define DELAYIMP_CODE   "\xFF\x25\0\0\0\0\x51\x52\x68\0\0\0\0\xE9\0\0\0\0"
#define DELAYIMP_SIZE   sizeof(DELAYIMP_CODE)-1
    memcpy(ip, DELAYIMP_CODE, DELAYIMP_SIZE);
    ip = (char *)ip + DELAYIMP_SIZE;
#undef DELAYIMP_CODE
#undef DELAYIMP_SIZE

    /*
     * Reserve room for the relocations.
     */
    re = (COFF_RELOC *)ip;
    ip = ((COFF_RELOC *)ip + 3);

    /*
     * Write relocation.
     */
    re->r_vaddr = 2;
    re->r_symndx = 5;  /* __imp_symbol */
    re->r_type = COFF_R_I386_DIR32;
    re++;

    re->r_vaddr = 9;
    re->r_symndx = 5;  /* __imp_symbol */
    re->r_type = COFF_R_I386_DIR32;
    re++;

    re->r_vaddr = 14;
    re->r_symndx = 12;  /* __tailMerge_XXX */
    re->r_type = COFF_R_I386_REL32;
    re++;

    sh++;

    /*
     * Write section header #2.
     */
    strncpy(sh->s_name, COFF_DIDAT5, 8);
    sh->s_vsize = 0;
    sh->s_vaddr = 0;
    sh->s_size = sizeof(long);
    sh->s_scnptr = ((char *)ip - (char *)base);
    sh->s_relptr = ((char *)ip - (char *)base) + sh->s_size;
    sh->s_lnnoptr = 0;
    sh->s_nreloc = 1;
    sh->s_nlnno = 0;
    sh->s_flags = COFF_DATA_FLAGS|COFF_STYP_COMDAT|COFF_STYP_A4;

    *(long *)ip = 0;
    ip = ((long *)ip + 1);

    /*
     * Reserve room for the relocation.
     */
    re = (COFF_RELOC *)ip;
    ip = ((COFF_RELOC *)ip + 1);

    /*
     * Write relocation.
     */
    re->r_vaddr = 0;
    re->r_symndx = 11;  /* "__imp_load__symbol" */
    re->r_type = COFF_R_I386_DIR32;

    sh++;

    /*
     * Write section header #3.
     */
    strncpy(sh->s_name, COFF_DIDAT4, 8);
    sh->s_vsize = 0;
    sh->s_vaddr = 0;
    sh->s_size = sizeof(long);
    sh->s_scnptr = ((char *)ip - (char *)base);
    sh->s_relptr = ((char *)ip - (char *)base) + sh->s_size;
    sh->s_lnnoptr = 0;
    sh->s_nreloc = 1;
    sh->s_nlnno = 0;
    sh->s_flags = COFF_DATA_FLAGS|COFF_STYP_COMDAT|COFF_STYP_A4;

    *(long *)ip = 0;
    ip = ((long *)ip + 1);

    /*
     * Reserve room for the relocation.
     */
    re = (COFF_RELOC *)ip;
    ip = ((COFF_RELOC *)ip + 1);

    /*
     * Write relocation.
     */
    re->r_vaddr = 0;
    re->r_symndx = 8;  /* ".didat$6" */
    re->r_type = COFF_R_I386_DIR32;

    sh++;

    /*
     * Write section header #4.
     */
    strncpy(sh->s_name, COFF_DIDAT6, 8);
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

    strcpy(ip, symname);
    ip = (char *)ip + strlen(ip)+1;

    /* "Hint/Name tables are WORD aligned" */
    ip = align_word_ptr(ip);

    /*
     * Update the section header.
     */
    sh->s_size = ((char *)ip - (char *)ip_raw);
    sh++;

    if (options.delay_bind)
    {
        /*
         * Write section header #5.
         */
        strncpy(sh->s_name, COFF_DIDAT7, 8);
        sh->s_vsize = 0;
        sh->s_vaddr = 0;
        sh->s_size = sizeof(long);
        sh->s_scnptr = ((char *)ip - (char *)base);
        sh->s_relptr = ((char *)ip - (char *)base) + sh->s_size;
        sh->s_lnnoptr = 0;
        sh->s_nreloc = 0;
        sh->s_nlnno = 0;
        sh->s_flags = COFF_DATA_FLAGS|COFF_STYP_COMDAT|COFF_STYP_A4;

        *(long *)ip = 0;
        ip = ((long *)ip + 1);

        sh++;
    }

    if (options.delay_unload)
    {
        /*
         * Write section header #5/#6.
         */
        strncpy(sh->s_name, COFF_DIDAT8, 8);
        sh->s_vsize = 0;
        sh->s_vaddr = 0;
        sh->s_size = sizeof(long);
        sh->s_scnptr = ((char *)ip - (char *)base);
        sh->s_relptr = ((char *)ip - (char *)base) + sh->s_size;
        sh->s_lnnoptr = 0;
        sh->s_nreloc = 1;
        sh->s_nlnno = 0;
        sh->s_flags = COFF_DATA_FLAGS|COFF_STYP_COMDAT|COFF_STYP_A4;

        *(long *)ip = 0;
        ip = ((long *)ip + 1);

        /*
         * Reserve room for the relocation.
         */
        re = (COFF_RELOC *)ip;
        ip = ((COFF_RELOC *)ip + 1);

        /*
         * Write relocation.
         */
        re->r_vaddr = 0;
        re->r_symndx = 11;  /* "__imp_load__symbol" */
        re->r_type = COFF_R_I386_DIR32;

        sh++;
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
     * Write the symbol table.
     */
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

    strncpy(se->n_name, COFF_DIDAT5, 8);
    se->n_value = 0;
    se->n_scnum = 2;
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_STAT;
    se->n_numaux = 1;
    se++;  /* 3++ */
    /**/
    ae = (COFF_AUXENT *)se;
    ae->x_ssize = sizeof(long);
    ae->x_nreloc = 1;
    ae->x_nlnno = 0;
    ae->x_chksum = 0;
    ae->x_scnum = 0;
    ae->x_sel = COFF_COMDAT_NODUPS;
    se++;  /* 4++ */

    sprintf(name, "__imp_%s", expname);
    write_symbol_name(se, name, &ip);
    se->n_value = 0;
    se->n_scnum = 2;
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_EXT;
    se->n_numaux = 0;
    se++;  /* 5++ */

    strncpy(se->n_name, COFF_DIDAT4, 8);
    se->n_value = 0;
    se->n_scnum = 3;
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_STAT;
    se->n_numaux = 1;
    se++;  /* 6++ */
    /**/
    ae = (COFF_AUXENT *)se;
    ae->x_ssize = sizeof(long);
    ae->x_nreloc = 1;
    ae->x_nlnno = 0;
    ae->x_chksum = 0;
    ae->x_scnum = 2;
    ae->x_sel = COFF_COMDAT_ASSOC;
    se++;  /* 7++ */

    strncpy(se->n_name, COFF_DIDAT6, 8);
    se->n_value = 0;
    se->n_scnum = 4;
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_STAT;
    se->n_numaux = 1;
    se++;  /* 8++ */
    /**/
    ae = (COFF_AUXENT *)se;
    ae->x_ssize = sh->s_size;  /* oops! */
    ae->x_nreloc = 0;
    ae->x_nlnno = 0;
    ae->x_chksum = 0;
    ae->x_scnum = 2;
    ae->x_sel = COFF_COMDAT_ASSOC;
    se++;  /* 9++ */

    sprintf(name, "__DELAY_IMPORT_DESCRIPTOR_%.*s",
        (dot) ? dot-modname : strlen(modname), modname);

    write_symbol_name(se, name, &ip);
    se->n_value = 0;
    se->n_scnum = COFF_N_UNDEF;
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_EXT;
    se->n_numaux = 0;
    se++;  /* 10++ */

    sprintf(name, "__imp_load_%s", expname);
    write_symbol_name(se, name, &ip);
    se->n_value = 6;
    se->n_scnum = 1;
    /* se->n_type = COFF_T_NULL; */
    se->n_type = (COFF_DT_FCN << COFF_N_BTSHFT);
    se->n_sclass = COFF_C_EXT;
    se->n_numaux = 0;
    se++;  /* 11++ */

    sprintf(name, "__tailMerge_%.*s",
        (dot) ? dot-modname : strlen(modname), modname);

    write_symbol_name(se, name, &ip);
    se->n_value = 0;
    se->n_scnum = COFF_N_UNDEF;
    se->n_type = COFF_T_NULL;
    se->n_sclass = COFF_C_EXT;
    se->n_numaux = 0;
    se++;  /* 12++ */

    if (options.delay_bind)
    {
        strncpy(se->n_name, COFF_DIDAT7, 8);
        se->n_value = 0;
        se->n_scnum = 5;
        se->n_type = COFF_T_NULL;
        se->n_sclass = COFF_C_STAT;
        se->n_numaux = 1;
        se++;  /* 13++ */
        /**/
        ae = (COFF_AUXENT *)se;
        ae->x_ssize = sizeof(long);
        ae->x_nreloc = 0;
        ae->x_nlnno = 0;
        ae->x_chksum = 0;
        ae->x_scnum = 0;
        ae->x_sel = COFF_COMDAT_NODUPS;
        se++;  /* 14++ */
    }

    if (options.delay_unload)
    {
        strncpy(se->n_name, COFF_DIDAT8, 8);
        se->n_value = 0;
        se->n_scnum = (options.delay_bind) ? 6 : 5;
        se->n_type = COFF_T_NULL;
        se->n_sclass = COFF_C_STAT;
        se->n_numaux = 1;
        se++;  /* 13++ / 15++ */
        /**/
        ae = (COFF_AUXENT *)se;
        ae->x_ssize = sizeof(long);
        ae->x_nreloc = 1;
        ae->x_nlnno = 0;
        ae->x_chksum = 0;
        ae->x_scnum = 0;
        ae->x_sel = COFF_COMDAT_NODUPS;
        se++;  /* 14++ / 16++ */
    }

    *(long *)strptr = ((char *)ip - strptr);
}

/****************************************************************************
 *                                                                          *
 * Function: write_symbol_name                                              *
 *                                                                          *
 * Purpose : Write the name of a symbol table entry.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           99-02-14  Created                                              *
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

