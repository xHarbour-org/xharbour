/****************************************************************************
 *                                                                          *
 * File    : execfile.c                                                     *
 *                                                                          *
 * Purpose : Win32 Linker; executable file management.                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           98-11-24  Support for debug info added.                        *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "link.h"

#define MAX_EXECUTABLE_SIZE     0x2000000   /* 32 MB */

/* Default DOS mode stub program */
static uchar_t stub[0x40] =
{
    0x0E,               /* push cs */
    0x1F,               /* pop  ds */
    0xBA, 0x0E, 0x00,   /* mov  dx, offset cs:<msg> */
    0xB4, 0x09,         /* mov  ah, 09h */
    0xCD, 0x21,         /* int  21h */
    0xB8, 0x01, 0x4C,   /* mov  ax, 4C01h */
    0xCD, 0x21,         /* int  21h */

    /* "This program cannot be run in DOS mode.\r\r\n$" */

    'T','h','i','s',' ','p','r','o','g','r','a','m',' ',
    'c','a','n','n','o','t',' ','b','e',' ','r','u','n',' ',
    'i','n',' ','D','O','S',' ','m','o','d','e','.',
    '\r','\r','\n','$'

};

/* Static function prototypes */
static void *write_executable_file(void *);
static void *write_stub(void *, void *);
static ushort_t executable_attributes(void);
static void *create_executable_map(void);
static void close_executable_map(void *);
static __inline void *align_executable_ptr(void *, void *);
static void *open_stub_map(FILEINFO *);
static void close_stub_map(FILEINFO *);

/****************************************************************************
 *                                                                          *
 * Function: write_executable_image                                         *
 *                                                                          *
 * Purpose : Write the executable file; main function.                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           98-12-05  Call to CVPACK added.                                *
 *           03-06-11  Call to CVPACK removed (we do it ourself!).          *
 *                                                                          *
 ****************************************************************************/

void write_executable_image(void)
{
    WINERR err = 0;
    void *ip;

    ip = create_executable_map();

    __try
    {
        ip = write_executable_file(ip);
    }
    __except (my_exception_filter(GetExceptionCode(), &err))
    {
        apperror(RCFATAL(err), "write_executable_image");
    }

    close_executable_map(ip);
}

/****************************************************************************
 *                                                                          *
 * Function: write_executable_file                                          *
 *                                                                          *
 * Purpose : Write the executable file.                                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           98-03-22  Bugfix: set filesize before checksum calculation!    *
 *           98-09-10  Variable file alignment option added.                *
 *           98-11-24  Support for debug info added.                        *
 *           99-02-13  Support for delayed imports added.                   *
 *           99-02-13  Simplified code for image directory processing.      *
 *           00-05-01  Option terminal_server_aware added.                  *
 *           00-11-16  Support for ARM machine added.                       *
 *           01-05-13  Support for external stub files added.               *
 *           02-01-14  Update image directory for exception info (PDATA).   *
 *                                                                          *
 ****************************************************************************/

static void *write_executable_file(void *ip)
{
    COFF_FILHDR *fhdr;
    COFF_OPTHDR *ohdr;
    COFF_SCNHDR *sh;
    COFF_DBGDIR *dbgdir;
    SCNENTRY *scn;
    GRPENTRY *grp;
    ushort_t section_count;
    void *base;

    base = ip;

    section_count = 0;
    for (grp = group_list; grp != NULL; grp = grp->next)
        section_count++;

    /*
     * Write the DOS stub.
     */
    ip = write_stub(ip, base);

    /*
     * Write the PE signature.
     */
    *(long *)ip = EXE_NT_MAGIC;
    ip = ((long *)ip + 1);

    /*
     * Write the file header.
     */
    fhdr = (COFF_FILHDR *)ip;
    ip = ((COFF_FILHDR *)ip + 1);

    fhdr->f_magic = (options.machine == MACHINE_ARM) ? COFF_F_MAG_ARM : COFF_F_MAG_I386;
    fhdr->f_nscns = section_count;
    fhdr->f_timdat = time_stamp;
    fhdr->f_symptr = 0;
    fhdr->f_nsyms = 0;
    fhdr->f_opthdr = COFF_OPTHSZ;
    fhdr->f_flags = executable_attributes();

    /*
     * Write the "optional" header.
     */
    ohdr = (COFF_OPTHDR *)ip;
    ip = ((COFF_OPTHDR *)ip + 1);

    memset(ohdr, 0, sizeof(*ohdr));  /* wipe out reserved fields */

    ohdr->o_magic = COFF_O_MAG_NT;
    ohdr->o_ld_majver = MAJOR_LINKER_VERSION;
    ohdr->o_ld_minver = MINOR_LINKER_VERSION;
    /* ohdr->o_tsize = 0; */
    /* ohdr->o_dsize = 0; */
    /* ohdr->o_bsize = 0; */
    ohdr->o_entry = (pub_entry) ? pub_entry->value : 0;
    /* ohdr->o_text_start = 0; */
    /* ohdr->o_data_start = 0; */
    ohdr->o_pebase = options.image_base;
    ohdr->o_salign = options.section_alignment;
    ohdr->o_falign = options.file_alignment;
    ohdr->o_os_majver = options.major_os_version;
    ohdr->o_os_minver = options.minor_os_version;
    /* ohdr->o_pe_majver = 0; */
    /* ohdr->o_pe_minver = 0; */
    ohdr->o_ss_majver = options.major_subsystem_version;
    ohdr->o_ss_minver = options.minor_subsystem_version;
    /* ohdr->o_w32ver = 0; */
    ohdr->o_pesize = stub_size;
    /* ohdr->o_hdrsize = 0; */
    /* ohdr->o_chksum = 0; */
    ohdr->o_sstype = options.subsystem;
    ohdr->o_dllflags = (options.terminal_server_aware) ? COFF_DLL_TSAWARE : 0;  /* only for EXE files!! */
    ohdr->o_stack_reserve = options.stack_reserve;
    ohdr->o_stack_commit = options.stack_commit;
    ohdr->o_heap_reserve = options.heap_reserve;
    ohdr->o_heap_commit = options.heap_commit;
    /* ohdr->o_loader = 0; */
    ohdr->o_npedir = COFF_NPEDIR;
    /*
     * ohdr->o_pedir[COFF_PE_EXPORT].pd_vaddr = 0;
     * ohdr->o_pedir[COFF_PE_EXPORT].pd_size = 0;
     * ohdr->o_pedir[COFF_PE_IMPORT].pd_vaddr = 0;
     * ohdr->o_pedir[COFF_PE_IMPORT].pd_size = 0;
     * ohdr->o_pedir[COFF_PE_RESOURCE].pd_vaddr = 0;
     * ohdr->o_pedir[COFF_PE_RESOURCE].pd_size = 0;
     * ohdr->o_pedir[COFF_PE_EXCEPTION].pd_vaddr = 0;
     * ohdr->o_pedir[COFF_PE_EXCEPTION].pd_size = 0;
     * ohdr->o_pedir[COFF_PE_SECURITY].pd_vaddr = 0;
     * ohdr->o_pedir[COFF_PE_SECURITY].pd_size = 0;
     * ohdr->o_pedir[COFF_PE_BASERELOC].pd_vaddr = 0;
     * ohdr->o_pedir[COFF_PE_BASERELOC].pd_size = 0;
     * ohdr->o_pedir[COFF_PE_DEBUG].pd_vaddr = 0;
     * ohdr->o_pedir[COFF_PE_DEBUG].pd_size = 0;
     * ohdr->o_pedir[COFF_PE_COPYRIGHT].pd_vaddr = 0;
     * ohdr->o_pedir[COFF_PE_COPYRIGHT].pd_size = 0;
     * ohdr->o_pedir[COFF_PE_GLOBALPTR].pd_vaddr = 0;
     * ohdr->o_pedir[COFF_PE_GLOBALPTR].pd_size = 0;
     * ohdr->o_pedir[COFF_PE_TLS].pd_vaddr = 0;
     * ohdr->o_pedir[COFF_PE_TLS].pd_size = 0;
     * ohdr->o_pedir[COFF_PE_LOADCONF].pd_vaddr = 0;
     * ohdr->o_pedir[COFF_PE_LOADCONF].pd_size = 0;
     * ohdr->o_pedir[COFF_PE_BOUNDIMP].pd_vaddr = 0;
     * ohdr->o_pedir[COFF_PE_BOUNDIMP].pd_size = 0;
     * ohdr->o_pedir[COFF_PE_IAT].pd_vaddr = 0;
     * ohdr->o_pedir[COFF_PE_IAT].pd_size = 0;
     * ohdr->o_pedir[COFF_PE_DELAYIMP].pd_vaddr = 0;
     * ohdr->o_pedir[COFF_PE_DELAYIMP].pd_size = 0;
     * ohdr->o_pedir[COFF_PE_COM].pd_vaddr = 0;
     * ohdr->o_pedir[COFF_PE_COM].pd_size = 0;
     */

    /*
     * Reserve room for the section headers.
     */
    sh = (COFF_SCNHDR *)ip;
    ip = ((COFF_SCNHDR *)ip + section_count);

    ip = align_executable_ptr(ip, base);
    ohdr->o_hdrsize = ((char *)ip - (char *)base);

    /*
     * Write all sections.
     */
    dbgdir = NULL;
    for (grp = group_list; grp != NULL; grp = grp->next)
    {
        long vsize;
        long size;
        bool_t bss;

        /*
         * Write the section header.
         */
        strncpy(sh->s_name, grp->name, 8);
        sh->s_vaddr = grp->vaddr;
        sh->s_scnptr = ((char *)ip - (char *)base);
        sh->s_relptr = 0;
        sh->s_lnnoptr = 0;
        sh->s_nreloc = 0;
        sh->s_nlnno = 0;
        sh->s_flags = grp->flags;

        /*
         * Write the raw data.
         */
        vsize = size = 0; bss = FALSE;
        for (scn = grp->scngrp; scn != NULL; scn = scn->scngrp)
        {
            SEGENTRY *seg;

            for (seg = scn->segs; seg != NULL; seg = seg->next)
            {
                if (seg->data != NULL)  /* anything but .bss */
                {
                    if (bss) apperror(RCFATAL(ERROR_INTERNAL), "bss sequence");

                    memset(ip, (grp->flags & COFF_STYP_TEXT) ? 0xCC : 0x00, seg->alignment);
                    ip = (char *)ip + seg->alignment;

                    if (seg_dbgdir == seg)  /* debug directory */
                        dbgdir = (COFF_DBGDIR *)ip;

                    memcpy(ip, seg->data, seg->size);
                    ip = (char *)ip + seg->size;

                    size += seg->alignment;
                    size += seg->size;
                }
                else
                {
                    bss = TRUE;  /* for self test */
                }

                vsize += seg->alignment;
                vsize += seg->size;
            }

            /*
             * Update the image directory.
             */
            if (strcmp(scn->name, COFF_EDATA) == 0)
            {
                ohdr->o_pedir[COFF_PE_EXPORT].pd_vaddr = address_of_section(scn);
                ohdr->o_pedir[COFF_PE_EXPORT].pd_size = size_of_section(scn);
            }
            else if (strcmp(scn->name, COFF_IDATA2) == 0)  /* #1 */
            {
                ohdr->o_pedir[COFF_PE_IMPORT].pd_vaddr = address_of_section(scn);
                ohdr->o_pedir[COFF_PE_IMPORT].pd_size = size_of_section(scn);
            }
            else if (strcmp(scn->name, COFF_IDATA3) == 0)  /* #2 */
            {
                ohdr->o_pedir[COFF_PE_IMPORT].pd_size += size_of_section(scn);
            }
            else if (strcmp(scn->name, COFF_RSRC) == 0)
            {
                ohdr->o_pedir[COFF_PE_RESOURCE].pd_vaddr = address_of_section(scn);
                ohdr->o_pedir[COFF_PE_RESOURCE].pd_size = size_of_section(scn);
            }
            else if (strcmp(scn->name, COFF_RELOCS) == 0)
            {
                ohdr->o_pedir[COFF_PE_BASERELOC].pd_vaddr = address_of_section(scn);
                ohdr->o_pedir[COFF_PE_BASERELOC].pd_size = size_of_section(scn);
            }
            else if (strcmp(scn->name, COFF_IDATA5) == 0)
            {
                ohdr->o_pedir[COFF_PE_IAT].pd_vaddr = address_of_section(scn);
                ohdr->o_pedir[COFF_PE_IAT].pd_size = size_of_section(scn);
            }
            else if (strcmp(scn->name, COFF_DIDAT2) == 0)  /* #1 */
            {
                ohdr->o_pedir[COFF_PE_DELAYIMP].pd_vaddr = address_of_section(scn);
                ohdr->o_pedir[COFF_PE_DELAYIMP].pd_size = size_of_section(scn);
            }
            else if (strcmp(scn->name, COFF_DIDAT3) == 0)  /* #2 */
            {
                ohdr->o_pedir[COFF_PE_DELAYIMP].pd_size += size_of_section(scn);
            }
            else if (strcmp(scn->name, COFF_PDATA) == 0)
            {
                ohdr->o_pedir[COFF_PE_EXCEPTION].pd_vaddr = address_of_section(scn);
                ohdr->o_pedir[COFF_PE_EXCEPTION].pd_size = size_of_section(scn);
            }
        }

        /*
         * Update the section header.
         */
        sh->s_vsize = vsize;
        sh->s_size = ROUNDUP(size, options.file_alignment);

        /*
         * Update the optional header.
         */
        ohdr->o_pesize += ROUNDUP(vsize, ohdr->o_salign);

        if (grp->flags & COFF_STYP_TEXT)
        {
            ohdr->o_tsize += sh->s_size;
            if (ohdr->o_text_start == 0)
                ohdr->o_text_start = grp->vaddr;
        }
        else if (grp->flags & COFF_STYP_DATA)
        {
            ohdr->o_dsize += ROUNDUP(vsize, options.file_alignment);
            if (ohdr->o_data_start == 0)
                ohdr->o_data_start = grp->vaddr;
        }
        else if (grp->flags & COFF_STYP_BSS)
        {
            ohdr->o_bsize += sh->s_size;
            if (ohdr->o_data_start == 0)
                ohdr->o_data_start = grp->vaddr;
        }

        sh++;

        /* Always align pointer (even for the last section!) */
        ip = align_executable_ptr(ip, base);
    }

    /*
     * Write debug information.
     */
    if (options.debug && seg_dbgdir != NULL)
    {
        ohdr->o_pedir[COFF_PE_DEBUG].pd_vaddr = seg_dbgdir->vaddr;
        ohdr->o_pedir[COFF_PE_DEBUG].pd_size = seg_dbgdir->size;

        /* Update debug directory and write all debug information */
        ip = write_debug_information(ip, base, dbgdir, fhdr);

        /* Always align pointer (even for the last section!) */
        ip = align_executable_ptr(ip, base);
    }

    /*
     * Calculate image checksum. Not needed, but recommended by Microsoft.
     */
    exe_file->size = file_offset(exe_file, ip);
    if (!calculate_executable_checksum(exe_file, &ohdr->o_chksum))
        apperror(RCWARNING(ERROR_CANT_DO_CHECKSUM));

    return ip;
}

/****************************************************************************
 *                                                                          *
 * Function: write_stub                                                     *
 *                                                                          *
 * Purpose : Write the DOS stub.                                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-05-13  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void *write_stub(void *ip, void *base)
{
    EXE_DOSHDR *mzhdr = (EXE_DOSHDR *)ip;
    size_t alignment;

    if (stub_file != NULL)
    {
        /*
         * Use an external stub image.
         */
        WINERR err = 0;
        void *sp;

        sp = open_stub_map(stub_file);

        __try
        {
            memcpy(ip, sp, stub_file->size);
            ip = (char *)ip + stub_file->size;
        }
        __except (my_exception_filter(GetExceptionCode(), &err))
        {
            apperror(RCFATAL(err), "write_stub");
        }

        close_stub_map(stub_file);
    }
    else
    {
        /*
         * Use the internal stub image (default).
         */
        memset(mzhdr, 0, sizeof(*mzhdr));
        ip = ((EXE_DOSHDR *)ip + 1);

        memcpy(ip, stub, sizeof(stub));
        ip = (char *)ip + sizeof(stub);

        mzhdr->mz_magic = EXE_DOS_MAGIC;
        mzhdr->mz_extrabytes = 0x90;
        mzhdr->mz_npages = 0x03;
        mzhdr->mz_nrelocs = 0;
        mzhdr->mz_hdrsz = 0x04;
        mzhdr->mz_minalloc = 0;
        mzhdr->mz_maxalloc = 0xFFFF;
        mzhdr->mz_ss = 0;
        mzhdr->mz_sp = 0x00B8;
        mzhdr->mz_csum = 0;
        mzhdr->mz_ip = 0;
        mzhdr->mz_cs = 0;
        mzhdr->mz_reloctab = 0x40;
        mzhdr->mz_ovno = 0;
        mzhdr->mz_oemid = 0;
        mzhdr->mz_oeminfo = 0;
    }

    /* PE header should be aligned on 8 byte boundary */
    alignment = ((char *)ip - (char *)base) % 8;
    if (alignment != 0)
    {
        alignment = (8 - alignment);
        memset(ip, 0, alignment);
        ip = (char *)ip + alignment;
    }

    /* set address to the PE signature */
    mzhdr->mz_lfanew = ((char *)ip - (char *)base);

    return ip;
}

/****************************************************************************
 *                                                                          *
 * Function: executable_attributes                                          *
 *                                                                          *
 * Purpose : Return image flags for the file header.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           99-08-20  Option large_address_aware added.                    *
 *                                                                          *
 ****************************************************************************/

static ushort_t executable_attributes(void)
{
    ushort_t flags;

    flags = COFF_F_EXEC|COFF_F_32BIT;

    if (!options.debug_coff)
        flags |= COFF_F_LSYMS|COFF_F_LNNO;

    if (options.DLL)
        flags |= COFF_F_DLL;

    if (options.fixed)
        flags |= COFF_F_RELFLG;

    if (options.swaprun_net)
        flags |= COFF_F_NETSWAP;

    if (options.swaprun_cd)
        flags |= COFF_F_REMSWAP;

    if (options.aggressive_ws_trim)
        flags |= COFF_F_TRIMWS;

    if (options.large_address_aware)
        flags |= COFF_F_3GB;

    return flags;
}

/****************************************************************************
 *                                                                          *
 * Function: create_executable_map                                          *
 *                                                                          *
 * Purpose : Create a read-write file mapping of the executable file.       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void *create_executable_map(void)
{
    WINERR err;

    err = my_createmap(exe_file, MAX_EXECUTABLE_SIZE);
    if (err) apperror(RCFATAL(err), exe_file->name);

    return exe_file->base;
}

/****************************************************************************
 *                                                                          *
 * Function: close_executable_map                                           *
 *                                                                          *
 * Purpose : Close a file-mapping of the executable file.                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void close_executable_map(void *next_byte)
{
    WINERR err;

    /* Set size; before we call my_closemap() */
    exe_file->size = file_offset(exe_file, next_byte);

    err = my_closemap(exe_file, TRUE);
    if (err) apperror(RCFATAL(err));
}

/****************************************************************************
 *                                                                          *
 * Function: align_executable_ptr                                           *
 *                                                                          *
 * Purpose : Align the file pointer and write any necessary padding.        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           98-09-10  Variable file alignment option added.                *
 *                                                                          *
 ****************************************************************************/

static __inline void *align_executable_ptr(void *next_byte, void *base)
{
    size_t alignment;

    alignment = ((char *)next_byte - (char *)base) % options.file_alignment;
    if (alignment != 0)
    {
        alignment = (options.file_alignment - alignment);
        memset(next_byte, 0, alignment);
    }

    return ((char *)next_byte + alignment);
}

/****************************************************************************
 *                                                                          *
 * Function: open_stub_map                                                  *
 *                                                                          *
 * Purpose : Create a file-mapping of a stub file.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-05-13  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void *open_stub_map(FILEINFO *stub_file)
{
    EXE_DOSHDR *mzhdr;
    WINERR err;

    err = my_openmap(stub_file);
    if (err) apperror(MYOPENERROR(RCFATAL(err)), stub_file->name);

    mzhdr = (EXE_DOSHDR *)stub_file->base;

    if (mzhdr->mz_magic != EXE_DOS_MAGIC)
        apperror(RCFATAL(ERROR_INVALID_STUB_FILE), stub_file->name);

    if (mzhdr->mz_reloctab < sizeof(*mzhdr))
        apperror(RCFATAL(ERROR_WEIRD_STUB_FILE), stub_file->name);

    return stub_file->base;
}

/****************************************************************************
 *                                                                          *
 * Function: close_stub_map                                                 *
 *                                                                          *
 * Purpose : Close a file-mapping of a stub file.                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-05-13  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void close_stub_map(FILEINFO *stub_file)
{
    WINERR err;

    err = my_closemap(stub_file, FALSE);
    if (err) apperror(RCFATAL(err));
}

