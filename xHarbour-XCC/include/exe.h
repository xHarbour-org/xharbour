/****************************************************************************
 *                                                                          *
 * File    : exe.h                                                          *
 *                                                                          *
 * Purpose : Executable file formats.                                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           99-11-29  Created by Pelle Orinius                             *
 *                                                                          *
 ****************************************************************************/

#ifndef _EXE_H
#define _EXE_H

#ifdef __cplusplus
extern "C" {
#endif

#include <potypes.h>

#ifndef NO_COFF_H_EXE_H
#include "coff.h"
#endif

#define EXE_DOS_MAGIC   0x5A4D      /* MZ */
#define EXE_OS2_MAGIC   0x454E      /* NE */
#define EXE_VXD_MAGIC   0x454C      /* LE */
#define EXE_NT_MAGIC    0x00004550  /* PE00 */

#include "pshpack2.h"

/*
 * DOS EXE header.
 */
struct EXE_doshdr {
    ushort_t mz_magic;              /* magic number */
    ushort_t mz_extrabytes;         /* bytes on last (partial) page of file */
    ushort_t mz_npages;             /* number of pages in file */
    ushort_t mz_nrelocs;            /* number of relocations */
    ushort_t mz_hdrsz;              /* size of header in paragraphs */
    ushort_t mz_minalloc;           /* minimum extra paragraphs needed */
    ushort_t mz_maxalloc;           /* maximum extra paragraphs needed */
    ushort_t mz_ss;                 /* initial SS value */
    ushort_t mz_sp;                 /* initial SP value */
    ushort_t mz_csum;               /* complemented checksum */
    ushort_t mz_ip;                 /* initial IP value */
    ushort_t mz_cs;                 /* initial CS value */
    ushort_t mz_reloctab;           /* byte offset to relocation table */
    ushort_t mz_ovno;               /* overlay number */
    ushort_t mz_res[4];             /* reserved words */
    ushort_t mz_oemid;              /* OEM identifier (for mz_oeminfo) */
    ushort_t mz_oeminfo;            /* OEM information; mz_oemid specific */
    ushort_t mz_res2[10];           /* reserved words */
    long mz_lfanew;                 /* byte offset to new exe header */
};

#define EXE_DOSHDR  struct EXE_doshdr

/*
 * New EXE header (OS/2, Win16).
 */
struct EXE_os2hdr {
    ushort_t ne_magic;              /* magic number */
    char ne_ver;                    /* linker version number */
    char ne_rev;                    /* linker revision number */
    ushort_t ne_enttab;             /* offset of Entry Table */
    ushort_t ne_enttabsz;           /* number of bytes in Entry Table */
    long ne_crc;                    /* checksum of file */
    ushort_t ne_flags;              /* flags */
    ushort_t ne_dgroupno;           /* automatic data segment number */
    ushort_t ne_heapsz;             /* initial heap allocation */
    ushort_t ne_stacksz;            /* initial stack allocation */
    long ne_csip;                   /* initial CS:IP */
    long ne_sssp;                   /* initial SS:SP */
    ushort_t ne_nseg;               /* count of file segments */
    ushort_t ne_nmod;               /* entries in Module Reference Table */
    ushort_t ne_nrestabsz;          /* size of non-resident name table */
    ushort_t ne_segtab;             /* offset of Segment Table */
    ushort_t ne_rsrctab;            /* offset of Resource Table */
    ushort_t ne_restab;             /* offset of resident name table */
    ushort_t ne_modtab;             /* offset of Module Reference Table */
    ushort_t ne_imptab;             /* offset of Imported Names Table */
    long ne_nrestab;                /* offset of Non-resident Names Table */
    ushort_t ne_nmovent;            /* count of movable entries */
    ushort_t ne_align;              /* segment alignment shift count */
    ushort_t ne_nres;               /* count of resource segments */
    uchar_t ne_targetos;            /* target Operating system */
    uchar_t ne_flagsothers;         /* other .EXE flags */
    ushort_t ne_gangload;           /* offset to return thunks */
    ushort_t ne_gangloadsz;         /* offset to segment ref. bytes */
    ushort_t ne_swaparea;           /* minimum code swap area size */
    ushort_t ne_expver;             /* expected Windows version number */
};

#define EXE_OS2HDR  struct EXE_os2hdr

#include "poppack.h"  /* back to default */

#endif /* _EXE_H */
