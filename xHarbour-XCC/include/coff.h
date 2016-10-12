/****************************************************************************
 *                                                                          *
 * File    : coff.h                                                         *
 *                                                                          *
 * Purpose : COFF (Common Object File Format) definitions.                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           99-11-24  Created by Pelle Orinius                             *
 *           00-11-08  Added relocation types for ARM processors.           *
 *           00-11-26  Added structure COFF_ARMFCN.                         *
 *           03-08-01  Added section name COFF_CRT.                         *
 *                                                                          *
 ****************************************************************************/

#ifndef _COFF_H
#define _COFF_H

#ifdef __cplusplus
extern "C" {
#endif

#include <potypes.h>

#ifndef _WCHAR_T_DEFINED
#include <wchar.h>
#endif

#include "pshpack4.h"  /* set default COFF packing */

/*
 * COFF file header.
 */
struct COFF_filehdr {
    ushort_t f_magic;           /* magic number */
    ushort_t f_nscns;           /* number of sections */
    long f_timdat;              /* time & date stamp */
    long f_symptr;              /* file ptr to start of symbol table */
    long f_nsyms;               /* number of symbol table entries */
    ushort_t f_opthdr;          /* size of optional header in bytes */
    ushort_t f_flags;           /* file status flags */
};

#define COFF_FILHDR  struct COFF_filehdr
#define COFF_FILHSZ  20         /* sizeof(COFF_FILHDR) */

/* f_magic values (Win32) */
#ifndef NOWIN32
#define COFF_F_MAG_UNKNOWN      0x0000  /* unknown */
#define COFF_F_MAG_I386         0x014c  /* Intel 386 */
#define COFF_F_MAG_R3000        0x0162  /* MIPS Mark I */
#define COFF_F_MAG_R4000        0x0166  /* MIPS Mark III */
#define COFF_F_MAG_R10000       0x0168  /* MIPS */
#define COFF_F_MAG_WCEMIPSV2    0x0169  /* MIPS WCE v2 */
#define COFF_F_MAG_ALPHA        0x0184  /* Alpha_AXP */
#define COFF_F_MAG_SH3          0x01a2  /* SH3 */
#define COFF_F_MAG_SH3E         0x01a4  /* SH3E */
#define COFF_F_MAG_SH4          0x01a6  /* SH4 */
#define COFF_F_MAG_ARM          0x01c0  /* ARM */
#define COFF_F_MAG_THUMB        0x01c2  /* Thumb */
#define COFF_F_MAG_POWERPC      0x01F0  /* IBM PowerPC */
#define COFF_F_MAG_IA64         0x0200  /* Intel 64 */
#define COFF_F_MAG_MIPS16       0x0266  /* MIPS */
#define COFF_F_MAG_ALPHA64      0x0284  /* ALPHA64 */
#define COFF_F_MAG_MIPSFPU      0x0366  /* MIPS */
#define COFF_F_MAG_MIPSFPU16    0x0466  /* MIPS */
#endif

/* f_flags values */
#define COFF_F_RELFLG   0x0001  /* relocations have been stripped */
#define COFF_F_EXEC     0x0002  /* the file is executable (i.e. no undefined externals) */
#define COFF_F_LNNO     0x0004  /* line numbers have been stripped */
#define COFF_F_LSYMS    0x0008  /* local symbols have been stripped */
#ifndef NOWIN32
/* Win32 additional s_flags values */
#define COFF_F_TRIMWS   0x0010  /* agressively trim working set */
#define COFF_F_3GB      0x0020  /* large address aware (>2GB) */
#define COFF_F_REVLO    0x0080  /* bytes of machine word are reversed */
#define COFF_F_REVHI    0x8000  /* bytes of machine word are reversed */
#define COFF_F_32BIT    0x0100  /* 32-bit word machine */
#define COFF_F_DBG      0x0200  /* debugging info stripped, now in DBG file */
#define COFF_F_REMSWAP  0x0400  /* run image on removable media from swap file */
#define COFF_F_NETSWAP  0x0800  /* run image on net from swap file */
#define COFF_F_SYSTEM   0x1000  /* system file */
#define COFF_F_DLL      0x2000  /* dynamic link library */
#define COFF_F_UPONLY   0x4000  /* run only on uni-processor machine */
#endif

/*
 * COFF optional header.
 */
#ifdef NOWIN32
struct COFF_aouthdr {
    short magic;                /* magic number */
    short vstamp;               /* version stamp */
    long tsize;                 /* text size in bytes */
    long dsize;                 /* initialized data size in bytes */
    long bsize;                 /* uninitialized data size in bytes */
    addr_t entry;               /* entry point */
    addr_t text_start;          /* base of text used for this file */
    addr_t data_start;          /* base of data used for this file */
};

#define COFF_AOUTHDR struct COFF_aouthdr
#endif

/*
 * COFF section header.
 */
struct COFF_scnhdr {
    char s_name[8];             /* section name */
    union {
        addr_t s_paddr;         /* physical address */
        long s_vsize;           /* virtual size */
    } _u;
    addr_t s_vaddr;             /* virtual address */
    long s_size;                /* section size in bytes */
    long s_scnptr;              /* file ptr to raw data for section */
    long s_relptr;              /* file ptr to relocations for section */
    long s_lnnoptr;             /* file ptr to line numbers for section */
    ushort_t s_nreloc;          /* number of relocation entries */
    ushort_t s_nlnno;           /* number of line number entries */
    ulong_t s_flags;            /* type and content flags */
};

#define COFF_SCNHDR  struct COFF_scnhdr
#define COFF_SCNHSZ  40         /* sizeof(COFF_SCNHDR) */

#define s_paddr _u.s_paddr
#define s_vsize _u.s_vsize

/* s_name values */
#define COFF_TEXT     ".text"       /* executable code */
#define COFF_RDATA    ".rdata"      /* read-only data */
#define COFF_DATA     ".data"       /* initialized read-write data */
#define COFF_BSS      ".bss"        /* uninitialized read-write data */
#define COFF_CRT      ".CRT"        /* C runtime initializers and terminators */
#ifndef NOWIN32
#define COFF_ARCH     ".arch"       /* Alpha architecture information */
#define COFF_EDATA    ".edata"      /* export */
#define COFF_IDATA    ".idata"      /* import */
#define COFF_IDATA2   ".idata$2"    /* import descriptor (/module) */
#define COFF_IDATA3   ".idata$3"    /* null import descriptor */
#define COFF_IDATA4   ".idata$4"    /* import lookup entry */
#define COFF_IDATA5   ".idata$5"    /* import address table (IAT) */
#define COFF_IDATA6   ".idata$6"    /* hint/name or module name */
#define COFF_RSRC     ".rsrc"       /* resources */
#define COFF_RELOCS   ".reloc"      /* base relocations */
#define COFF_TLS      ".tls"        /* thread local storage */
#define COFF_DRECTVE  ".drectve"    /* linker directives */
#define COFF_DIDAT    ".didat"
#define COFF_DIDAT2   ".didat$2"    /* delay import descriptor */
#define COFF_DIDAT3   ".didat$3"    /* null delay import descriptor */
#define COFF_DIDAT4   ".didat$4"    /* import lookup entry */
#define COFF_DIDAT5   ".didat$5"    /* import address table (IAT) */
#define COFF_DIDAT6   ".didat$6"    /* hint/name or module name */
#define COFF_DIDAT7   ".didat$7"    /* bound import address table (BIAT) */
#define COFF_DIDAT8   ".didat$8"    /* unload import address table (UIAT) */
#define COFF_DEBUG    ".debug"
#define COFF_DEBUGF   ".debug$F"    /* frame pointer omission */
#define COFF_DEBUGS   ".debug$S"    /* stream of CV4 symbols */
#define COFF_DEBUGT   ".debug$T"    /* stream of CV4 types */
#define COFF_DEBUGP   ".debug$P"    /* stream of CV4 (precompiled) types */
#define COFF_PDATA    ".pdata"      /* exception information (MIPS/Alpha) */
#define COFF_XDATA    ".xdata"      /* exception information */
#endif

/* s_flags values */
#define COFF_STYP_REG       0x00000000L
#define COFF_STYP_TEXT      0x00000020L  /* section contains code */
#define COFF_STYP_DATA      0x00000040L  /* section contains data */
#define COFF_STYP_BSS       0x00000080L  /* section contains bss */
#define COFF_STYP_INFO      0x00000200L  /* section contains comment or other info */
/* Win32 additional s_flags values */
#ifndef NOWIN32
#define COFF_STYP_OTHER     0x00000100L
#define COFF_STYP_NOPAD     0x00000008L
#define COFF_STYP_REMOVE    0x00000800L  /* section will not become part of image */
#define COFF_STYP_COMDAT    0x00001000L  /* section contents comdat */
#define COFF_STYP_FARDATA   0x00008000L
#define COFF_STYP_PURGE     0x00020000L  /* purgeable */
#define COFF_STYP_LOCKED    0x00040000L
#define COFF_STYP_PRELOAD   0x00080000L
#define COFF_STYP_A1        0x00100000L  /* alignment; 1 byte */
#define COFF_STYP_A2        0x00200000L  /* alignment; 2 bytes */
#define COFF_STYP_A4        0x00300000L  /* alignment; 4 bytes */
#define COFF_STYP_A8        0x00400000L  /* alignment; 8 bytes */
#define COFF_STYP_A16       0x00500000L  /* alignment; 16 bytes (default) */
#define COFF_STYP_A32       0x00600000L  /* alignment; 32 bytes */
#define COFF_STYP_A64       0x00700000L  /* alignment; 64 bytes */
#define COFF_STYP_A128      0x00800000L  /* alignment; 128 bytes */
#define COFF_STYP_A256      0x00900000L  /* alignment; 256 bytes */
#define COFF_STYP_A512      0x00A00000L  /* alignment; 512 bytes */
#define COFF_STYP_A1024     0x00B00000L  /* alignment; 1024 bytes */
#define COFF_STYP_A2048     0x00C00000L  /* alignment; 2048 bytes */
#define COFF_STYP_A4096     0x00D00000L  /* alignment; 4096 bytes */
#define COFF_STYP_A8192     0x00E00000L  /* alignment; 8192 bytes */
#define COFF_STYP_ALIGN     0x00F00000L
#define COFF_STYP_XRELOC    0x01000000L  /* section contains extended relocations */
#define COFF_STYP_DISCARD   0x02000000L  /* section can be discarded */
#define COFF_STYP_NOCACHE   0x04000000L  /* section is not cachable */
#define COFF_STYP_NOPAGE    0x08000000L  /* section is not pageable */
#define COFF_STYP_SHARE     0x10000000L  /* section is shareable */
#define COFF_STYP_EXEC      0x20000000L  /* section is executable */
#define COFF_STYP_READ      0x40000000L  /* section is readable */
#define COFF_STYP_WRITE     0x80000000L  /* section is writeable */
#endif

/* executable section flags */
#define COFF_TEXT_FLAGS         (COFF_STYP_TEXT | COFF_STYP_READ | COFF_STYP_EXEC)
#define COFF_RDATA_FLAGS        (COFF_STYP_DATA | COFF_STYP_READ)
#define COFF_DATA_FLAGS         (COFF_STYP_DATA | COFF_STYP_READ | COFF_STYP_WRITE)
#define COFF_BSS_FLAGS          (COFF_STYP_BSS | COFF_STYP_READ | COFF_STYP_WRITE)
#ifndef NOWIN32
#define COFF_EDATA_FLAGS        (COFF_RDATA_FLAGS)
#define COFF_RELOCS_FLAGS       (COFF_STYP_DATA | COFF_STYP_READ | COFF_STYP_DISCARD)
#define COFF_RSRC_FLAGS         (COFF_RDATA_FLAGS)
#define COFF_TLS_FLAGS          (COFF_DATA_FLAGS)
#endif

#include "pshpack2.h"   /* set packing for symbols, relocs & linenumbers */

/*
 * COFF relocation entry.
 */
struct COFF_reloc {
    addr_t r_vaddr;             /* address of reference */
    long r_symndx;              /* index into symbol table */
    ushort_t r_type;            /* relocation type */
};

#define COFF_RELOC   struct COFF_reloc
#define COFF_RELSZ   10             /* sizeof(COFF_RELOC) */

#ifndef NOWIN32
/* r_type values for X86 */
#define COFF_R_I386_ABS     0x0000  /* absolute, no relocation necessary */
#define COFF_R_I386_DIR16   0x0001  /* direct 16-bit reference */
#define COFF_R_I386_REL16   0x0002  /* pc-relative 16-bit reference */
#define COFF_R_I386_DIR32   0x0006  /* direct 32-bit reference */
#define COFF_R_I386_DIR32NB 0x0007  /* direct 32-bit reference, no base */
#define COFF_R_I386_SEG12   0x0009  /* direct 16-bit reference to seg selector */
#define COFF_R_I386_SECTION 0x000A
#define COFF_R_I386_SECREL  0x000B
#define COFF_R_I386_REL32   0x0014  /* pc-relative 32-bit reference */

/* r_type values for ARM */
#define COFF_R_ARM_ABS      0x0000  /* absolute, no relocation necessary */
#define COFF_R_ARM_ADDR32   0x0001  /* 32 bit address */
#define COFF_R_ARM_ADDR32NB 0x0002  /* 32 bit address w/o image base */
#define COFF_R_ARM_BRANCH24 0x0003  /* 24 bit offset << 2 & sign ext */
#define COFF_R_ARM_BRANCH11 0x0004  /* Thumb: 2 11 bit offsets */
#define COFF_R_ARM_SECTION  0x000E  /* section table index */
#define COFF_R_ARM_SECREL   0x000F  /* offset within section */
#endif

/*
 * COFF line number entry.
 */
struct COFF_lineno {
    union {                     /* if l_lnno == 0 */
        long l_symndx;          /* then l_symndx */
        addr_t l_paddr;         /* else l_paddr */
    } _addr;
    ushort_t l_lnno;            /* line number */
};

#define COFF_LINENO  struct COFF_lineno
#define COFF_LINESZ  6          /* sizeof(COFF_LINENO) */

#define l_symndx    _addr.l_symndx
#define l_paddr     _addr.l_paddr

/*
 * COFF symbol table entry.
 */
#define COFF_SYMNMLEN   8

struct COFF_syment {
    union {
        char _n_name[COFF_SYMNMLEN]; /* symbol name */
        struct {                /* if _n_name[0-3] == 0 */
            long _n_zeroes;     /* then _n_name[4-7] is an */
            long _n_offset;     /* offset into string table */
        } _n_n;
        char *_n_nptr[2];       /* allows for overlaying */
    } _n;
    long n_value;               /* value of symbol */
    short n_scnum;              /* section number */
    ushort_t n_type;            /* type and derived type */
    char n_sclass;              /* storage class */
    char n_numaux;              /* number of aux. entries */
};

#define COFF_SYMENT  struct COFF_syment
#define COFF_SYMESZ  18         /* sizeof(COFF_SYMENT) */

/* useful symbol name defines */
#define n_name      _n._n_name
#define n_nptr      _n._n_nptr[1]
#define n_zeroes    _n._n_n._n_zeroes
#define n_offset    _n._n_n._n_offset

/* section numbers with special meanings */
#define COFF_N_DEBUG    (short)-2   /* symbol is a special debug item */
#define COFF_N_ABS      (short)-1   /* symbol is an absolute value */
#define COFF_N_UNDEF    (short)0    /* symbol is undefined or is common */

/* storage classes */
#ifndef NOWIN32
#define COFF_C_EFCN     (char)-1    /* physical end of a function */
#define COFF_C_NULL     0x0000
#endif
#define COFF_C_AUTO     0x0001      /* automatic (stack) variable */
#define COFF_C_EXT      0x0002      /* external symbol */
#define COFF_C_STAT     0x0003      /* static symbol */
#define COFF_C_REG      0x0004      /* register variable */
#define COFF_C_EXTDEF   0x0005      /* external definition */
#define COFF_C_LABEL    0x0006      /* label */
#define COFF_C_ULABEL   0x0007      /* undefined label */
#define COFF_C_MOS      0x0008      /* nth member of structure */
#define COFF_C_ARG      0x0009      /* nth function argument */
#define COFF_C_STRTAG   0x000A      /* structure tagname */
#define COFF_C_MOU      0x000B      /* nth member of union */
#define COFF_C_UNTAG    0x000C      /* union tagname */
#define COFF_C_TPDEF    0x000D      /* type definition entry */
#define COFF_C_USTATIC  0x000E      /* uninitialized static */
#define COFF_C_ENTAG    0x000F      /* enumeration tagname */
#define COFF_C_MOE      0x0010      /* nth member of enumeration */
#define COFF_C_REGPARM  0x0011      /* register parameter */
#define COFF_C_FIELD    0x0012      /* nth bit in a bit field */
/*               0x0013-0x0063*/
#define COFF_C_BLOCK    0x0064      /* beginning (.bb) or end (.eb) of block */
#define COFF_C_FCN      0x0065      /* beginning (.bf) or end (.ef) of function */
#define COFF_C_EOS      0x0066      /* end of structure */
#define COFF_C_FILE     0x0067      /* file name */
#ifndef NOWIN32
#define COFF_C_SECT     0x0068      /* section */
#define COFF_C_WEAKEXT  0x0069      /* like C_EXT, but with weak linkage */
#endif

/* fundamental types */
#define COFF_T_NULL     0x0000
#define COFF_T_VOID     0x0001
#define COFF_T_CHAR     0x0002
#define COFF_T_SHORT    0x0003
#define COFF_T_INT      0x0004
#define COFF_T_LONG     0x0005
#define COFF_T_FLOAT    0x0006
#define COFF_T_DOUBLE   0x0007
#define COFF_T_STRUCT   0x0008
#define COFF_T_UNION    0x0009
#define COFF_T_ENUM     0x000A      /* enumeration */
#define COFF_T_MOE      0x000B      /* member of enumeration */
#define COFF_T_UCHAR    0x000C
#define COFF_T_USHORT   0x000D
#define COFF_T_UINT     0x000E
#define COFF_T_ULONG    0x000F
#ifndef NOWIN32
#define COFF_T_PCODE    0x8000
#endif

/* derived types */
#define COFF_DT_NON     0           /* no derived type */
#define COFF_DT_PTR     1           /* pointer */
#define COFF_DT_FCN     2           /* function */
#define COFF_DT_ARY     3           /* array */

#define COFF_N_BTMASK   0x000f
#define COFF_N_TMASK    0x0030
#define COFF_N_BTSHFT   4
#define COFF_N_TSHIFT   2

/* basic type of x */
#define COFF_BTYPE(x)   ((x) & COFF_N_BTMASK)

/* is x a pointer, a function, an array or a struct/union/enum tag? */
#define COFF_ISPTR(x)   (((x) & COFF_N_TMASK) == (COFF_DT_PTR << COFF_N_BTSHFT))
#define COFF_ISFCN(x)   (((x) & COFF_N_TMASK) == (COFF_DT_FCN << COFF_N_BTSHFT))
#define COFF_ISARY(x)   (((x) & COFF_N_TMASK) == (COFF_DT_ARY << COFF_N_BTSHFT))
#define COFF_ISTAG(x)   ((x)==COFF_C_STRTAG || (x)==COFF_C_UNTAG || (x)==COFF_C_ENTAG)

#define COFF_DECREF(x)  ((((x) >> COFF_N_TSHIFT) & ~COFF_N_BTMASK) | ((x) & COFF_N_BTMASK))

union COFF_auxent {
    struct {
        long x_tagndx;              /* struct, union, enum or function tag index */
        union {
            struct {
                ushort_t x_lnno;        /* declaration line number */
                ushort_t x_size;        /* size of struct, union or enum */
            } _lsz;
            ulong_t x_tsize;
        } _msc;
        union {
            struct {
                long x_lnnoptr;     /* pointer to func line number entry */
                long x_endndx;      /* pointer to next function */
            } _fcn;
            struct {
                short x_dim[4];     /* up to 4 dimensions */
            } _ary;
        } _foa;
        ushort_t x_tvndx;           /* tv index */
    } _sym;
    struct {
        char x_fname[COFF_SYMESZ];  /* filename */
    } _pgm;
    struct {
        long x_ssize;               /* section length */
        ushort_t x_nreloc;          /* number of relocation entries */
        ushort_t x_nlnno;           /* number of line number entries */
        long x_chksum;              /* checksum */
        short x_scnum;              /* section number */
        char x_sel;                 /* communal selection type */
    } _scn;
};

#define COFF_AUXENT  union COFF_auxent
#define COFF_AUXESZ  COFF_SYMESZ    /* sizeof(COFF_AUXENT) */

/* useful aux symbol defines */
#define x_tagndx    _sym.x_tagndx
#define x_lnno      _sym._msc._lsz.x_lnno
#define x_size      _sym._msc._lsz.x_size
#define x_tsize     _sym._msc.x_tsize
#define x_lnnoptr   _sym._foa._fcn.x_lnnoptr
#define x_endndx    _sym._foa._fcn.x_endndx
#define x_dim       _sym._foa._ary.x_dim
#define x_tvndx     _sym.x_tvndx
#define x_fname     _pgm.x_fname
#define x_ssize     _scn.x_ssize
#define x_nreloc    _scn.x_nreloc
#define x_nlnno     _scn.x_nlnno
#define x_chksum    _scn.x_chksum
#define x_scnum     _scn.x_scnum
#define x_sel       _scn.x_sel

#ifndef NOWIN32
/* communal selection types */
#define COFF_COMDAT_NODUPS  1       /* select no duplicates */
#define COFF_COMDAT_ANY     2       /* select any */
#define COFF_COMDAT_SAMESZ  3       /* select same size */
#define COFF_COMDAT_EXACT   4       /* select exact match */
#define COFF_COMDAT_ASSOC   5       /* select associative */
#define COFF_COMDAT_LARGEST 6       /* select largest */
#define COFF_COMDAT_NEWEST  7       /* select newest */

#define COFF_WEAKEXT_NOLIB  1       /* don't search libraries */
#define COFF_WEAKEXT_LIB    2       /* search libraries */
#define COFF_WEAKEXT_ALIAS  3       /* search alias */
#endif

#include "poppack.h"    /* back to default COFF packing */

/*
 * COFF archive definitions.
 */
#define COFF_ARMAG      "!<arch>\n"
#define COFF_ARMSZ      8
#define COFF_ARFMAG     "`\n"
#define COFF_ARPAD      "\n"

/* archive file member header is printable ASCII */
struct COFF_ar_hdr
{
    char ar_name[16];   /* name is '/' terminated */
    char ar_date[12];   /* date in decimal */
    char ar_uid[6];     /* user id in decimal */
    char ar_gid[6];     /* group id in decimal */
    char ar_mode[8];    /* mode in octal */
    char ar_size[10];   /* size in decimal */
    char ar_fmag[2];    /* COFF_ARFMAG, the string to end header */
};

#define COFF_ARHDR   struct COFF_ar_hdr
#define COFF_ARHSZ   60         /* sizeof(COFF_ARHDR) */

/************************************************************************/
/*
 * Additional Win32 PE structures and definitions.
 */

#ifndef NOWIN32

/*
 * Win32 PE data directory entry.
 */
struct COFF_pedir {
    addr_t pd_vaddr;    /* virtual address of section */
    long pd_size;       /* section size in bytes */
};

#define COFF_PEDIR  struct COFF_pedir
#define COFF_PEDSZ  8           /* sizeof(COFF_PEDIR) */

#define COFF_NPEDIR  16

/* indexes into o_pedir (below) */
#define COFF_PE_EXPORT      0   /* export directory */
#define COFF_PE_IMPORT      1   /* import directory */
#define COFF_PE_RESOURCE    2   /* resource directory */
#define COFF_PE_EXCEPTION   3   /* exception directory */
#define COFF_PE_SECURITY    4   /* security directory */
#define COFF_PE_BASERELOC   5   /* base relocation table */
#define COFF_PE_DEBUG       6   /* debug directory */
#define COFF_PE_COPYRIGHT   7   /* architecture specific data */
#define COFF_PE_GLOBALPTR   8   /* RVA of GP */
#define COFF_PE_TLS         9   /* Thread Local Storage directory */
#define COFF_PE_LOADCONF    10  /* load configuration directory */
#define COFF_PE_BOUNDIMP    11  /* bound import directory */
#define COFF_PE_IAT         12  /* import address table */
#define COFF_PE_DELAYIMP    13  /* delay load import descriptors */
#define COFF_PE_COM         14  /* COM runtime descriptor */
#define COFF_PE_RESERVED    15  /* reserved by Mr Gates */

/*
 * Win32 optional header.
 */
struct COFF_opthdr {
    short o_magic;              /* magic number */
    uchar_t o_ld_majver;        /* major linker version */
    uchar_t o_ld_minver;        /* minor linker version */
    long o_tsize;               /* text size in bytes */
    long o_dsize;               /* initialized data size in bytes */
    long o_bsize;               /* uninitialized data size in bytes */
    addr_t o_entry;             /* entry point */
    addr_t o_text_start;        /* base of text used for this file */
    addr_t o_data_start;        /* base of data used for this file */
    /**/
    addr_t o_pebase;            /* image base */
    long o_salign;              /* section alignment */
    long o_falign;              /* file alignment */
    ushort_t o_os_majver;       /* major operating system version */
    ushort_t o_os_minver;       /* minor operating system version */
    ushort_t o_pe_majver;       /* major image version */
    ushort_t o_pe_minver;       /* minor image version */
    ushort_t o_ss_majver;       /* major subsystem version */
    ushort_t o_ss_minver;       /* minor subsystem version */
    ulong_t o_w32ver;           /* Win32 version */
    long o_pesize;              /* image size in bytes */
    long o_hdrsize;             /* header size in bytes */
    long o_chksum;              /* image checksum */
    ushort_t o_sstype;          /* type of subsystem */
    ushort_t o_dllflags;        /* DLL characteristics */
    long o_stack_reserve;       /* stack reserve size in bytes (o_maxstack) */
    long o_stack_commit;        /* stack commit size in bytes (o_orgstack) */
    long o_heap_reserve;        /* heap reserve size in bytes */
    long o_heap_commit;         /* heap commit size in bytes */
    ulong_t o_loader;           /* loader flags */
    long o_npedir;              /* number of data directory entries */
    COFF_PEDIR o_pedir[COFF_NPEDIR]; /* data directory */
};

#define COFF_OPTHDR struct COFF_opthdr
#define COFF_OPTHSZ 224         /* sizeof(COFF_OPTHDR) */

/* o_magic value */
#define COFF_O_MAG_NT   0x010b  /* PE32 */
#define COFF_O_MAG_NTX  0x020b  /* PE32+ (64-bit address space; 32-bit image size) */

/* o_sstype values */
#define COFF_SS_UNKNOWN     0   /* unknown */
#define COFF_SS_NATIVE      1   /* image doesn't require a subsystem */
#define COFF_SS_WINGUI      2   /* image runs in Windows GUI subsystem */
#define COFF_SS_WINCUI      3   /* image runs in Windows character subsystem */
#define COFF_SS_OS2CUI      5   /* image runs in OS/2 character subsystem */
#define COFF_SS_POSIXCUI    7   /* image runs in Posix character subsystem */
#define COFF_SS_NATIVEWIN   8   /* image is a native Win9X driver */
#define COFF_SS_WCEGUI      9   /* image runs in the Windows CE subsystem */
#define COFF_SS_EFIAPP      10  /* image is an EFI application */
#define COFF_SS_EFIBOOT     11  /* image is an EFI driver that provides boot services */
#define COFF_SS_EFIRUN      12  /* image is an EFI driver that provides runtime services */

/* o_dllflags value */
#define COFF_DLL_NOBIND     0x0800  /* do not bind image */
#define COFF_DLL_WDMDRV     0x2000  /* driver uses WDM model */
#define COFF_DLL_TSAWARE    0x8000  /* image is Terminal Server aware */

/*
 * Win32 PE headers.
 */
struct COFF_pehdrs {
    ulong_t pe_magic;           /* magic number (EXE_WNT_MAGIC) */
    COFF_FILHDR pe_fhdr;        /* file header */
    COFF_OPTHDR pe_ohdr;        /* optional header */
};

#define COFF_PEHDRS struct COFF_pehdrs

#define COFF_PEHDRS_FIRST_SCNHDR(pehdrs) \
    (COFF_SCNHDR *)((char *)(pehdrs) + \
    offsetof(struct COFF_pehdrs, pe_ohdr) + \
    ((COFF_PEHDRS *)(pehdrs))->pe_fhdr.f_opthdr)

/*
 * Win32 based relocation entry.
 */
struct COFF_breloc {
    addr_t b_vaddr;             /* virtual address */
    long b_size;                /* size of block in bytes */
/*  ushort_t b_type;  */        /* relocation type */
};

#define COFF_BRELOC  struct COFF_breloc
#define COFF_BRELSZ  8          /* sizeof(COFF_BRELOC) */

/* b_type values */
#define COFF_B_ABSOLUTE         0
#define COFF_B_HIGH             1
#define COFF_B_LOW              2
#define COFF_B_HIGHLOW          3
#define COFF_B_HIGHADJ          4
#define COFF_B_MIPS_JADDR       5
#define COFF_B_SECTION          6
#define COFF_B_REL32            7
#define COFF_B_MIPS_JADDR16     9
#define COFF_B_IA64_IMM64       9
#define COFF_B_DIR64            10
#define COFF_B_HIGH3ADJ         11

/*
 * Win32 export information.
 */
struct COFF_export {
    ulong_t e_flags;            /* flags */
    long e_timdat;              /* time & date stamp */
    ushort_t e_majver;          /* major version number */
    ushort_t e_minver;          /* minor version number */
    addr_t e_modname;           /* RVA to module name */
    long e_ordbase;             /* ordinal base */
    long e_naddrs;              /* number of entries in EAT */
    long e_nnames;              /* number of entries in name/ordinal table */
    addr_t e_addrtab;           /* RVA to start of EAT */
    addr_t e_nametab;           /* RVA to start of name table */
    addr_t e_ordtab;            /* RVA to start of ordinal table */
};

#define COFF_EXPORT  struct COFF_export
#define COFF_EXPOSZ  40         /* sizeof(COFF_EXPORT) */

/*
 * Win32 import junk.
 */
struct COFF_import_by_name {
    short in_hint;              /* hint */
    char in_name[1];            /* name */
};

#define COFF_IMPORT_BY_NAME  struct COFF_import_by_name

struct COFF_import_thunk {
    union {
        char *forwarder_string;                 /*FIXME*/
        long *function;                         /*FIXME*/
        long ordinal;                           /*FIXME*/
        COFF_IMPORT_BY_NAME *address_of_data;   /*FIXME*/
    } _u;
};

#define COFF_IMPORT_THUNK   struct COFF_import_thunk

#define COFF_IMP_ORDINAL    0x80000000

#define COFF_ORDINAL(ord)       (ord & 0xffff)
#define COFF_BY_ORDINAL(ord)    ((ord & COFF_IMP_ORDINAL) != 0)

/*
 * Win32 import information.
 */
struct COFF_import {
    union {
        ulong_t i_flags;        /* zero for terminating descriptor */
        addr_t i_oaddrtab;      /* RVA to original unbound IAT (COFF_import_thunk) */
    } _u;
    long i_timdat;              /* time & date stamp (bound DLL) */
    long i_fwdndx;              /* index of first forwarder reference */
    addr_t i_modname;           /* RVA to module name */
    addr_t i_addrtab;           /* RVA to IAT (if bound this IAT has actual addresses) */
};

#define COFF_IMPORT  struct COFF_import
#define COFF_IMPOSZ  20         /* sizeof(COFF_IMPORT) */

#define i_flags    _u.i_flags
#define i_oaddrtab _u.i_oaddrtab

/*
 * Win32 bound import information.
 */
struct COFF_bound {
    long bi_timdat;             /* time & date stamp */
    short bi_modnptr;           /* file ptr to module name */
    short bi_nrefs;             /* number of module forwarder refs (first record, otherwise reserved) */
};

#define COFF_BOUND   struct COFF_bound
#define COFF_BOUSZ   8

/*
 * Win32 new import *object* information.
 */
struct COFF_newimp {
    ushort_t ni_sig1;           /* magic #1: must be COFF_F_MAG_UNKNOWN */
    ushort_t ni_sig2;           /* magic #2: must be COFF_I_MAG_SIG2 */
    short ni_vstamp;            /* version stamp */
    ushort_t ni_magic;          /* magic number (COFF_F_MAG_???) */
    long ni_timdat;             /* time & date stamp */
    long ni_dsize;              /* size of data */
    union {
        short ni_ordinal;       /* ordinal number */
        short ni_hint;          /* hint */
    } _u;
    ushort_t ni_flags;          /* import type */
};

#define COFF_NEWIMP  struct COFF_newimp
#define COFF_NEWISZ  20         /* sizeof(COFF_NEWIMP) */

#define ni_ordinal _u.ni_ordinal
#define ni_hint    _u.ni_hint

/* ni_sig2 value */
#define COFF_I_MAG_SIG2     0xffff

/* ni_flags values */
#define COFF_I_CODE         0x0000
#define COFF_I_DATA         0x0001
#define COFF_I_CONST        0x0002
#define COFF_I_TMASK        0x0003

#define COFF_I_ORDINAL      0x0000
#define COFF_I_NAME         0x0004
#define COFF_I_NAME_NOPREF  0x0008
#define COFF_I_NAME_UNDEC   0x000C
#define COFF_I_NMASK        0x001C

#define COFF_I_NEWTYPE(flags)   (((flags) & COFF_I_TMASK))
#define COFF_I_NEWNAME(flags)   (((flags) & COFF_I_NMASK) >> 2)

/*
 * Win32 delay import information.
 */
struct COFF_delay {
    ulong_t di_flags;           /* file status flags */
    addr_t di_modname;          /* linear address of module name */
    addr_t di_hmod;             /* linear address of module handle */
    addr_t di_addrtab;          /* linear address of IAT (import address table) */
    addr_t di_nametab;          /* linear address of INT (import name table) */
    addr_t di_baddrtab;         /* linear address of BIAT (bound import address table) */
    addr_t di_uaddrtab;         /* linear address of UIAT (unload import address table) */
    long di_timdat;             /* time & date stamp of bound module */
};

#define COFF_DELAY   struct COFF_delay
#define COFF_DELSZ   32         /* sizeof(COFF_DELAY) */

/*
 * Win32 Thread Local Storage.
 */
typedef void (__stdcall *tls_callback)(void *handle, ulong_t reason, void *reserved);

struct COFF_tlsdir {
    addr_t t_start;             /* virtual address of start of TLS block */
    addr_t t_end;               /* virtual address of end of TLS block */
    addr_t t_index;             /* virtual address of index variable used to access TLS block */
    tls_callback *t_calltab;    /* virtual address of callback table */
    ulong_t t_zfillsz;          /* size of zero fill */
    ulong_t t_flags;            /* flags */
};

#define COFF_TLSDIR  struct COFF_tlsdir
#define COFF_TLSDSZ  24

/* t_flags values */
#define COFF_T_SCN_SCALE_INDEX  0x00000001  /* TLS index is scaled */

/*
 * Win32 resource directory header.
 */
struct COFF_reshdr {
    ulong_t rh_flags;           /* flags */
    long rh_timdat;             /* time & date stamp */
    ushort_t rh_majver;         /* major version number */
    ushort_t rh_minver;         /* minor version number */
    short rh_nnames;            /* number of named entries */
    short rh_nids;              /* number of numbered entries */
};

#define COFF_RESHDR  struct COFF_reshdr
#define COFF_RESHSZ  16         /* sizeof(COFF_RESHDR) */

/*
 * Win32 resource directory entry.
 */
struct COFF_resdir {
    union {
        struct {
            int _n_strpos:31;   /* rsrc ptr to name string... */
            int _n_strent:1;    /* ...if this bit is set */
        } _n_n;
        long _n_name;
        short _n_id;            /* integer id */
    } _n;
    union {
        long _d_offset;         /* rsrc ptr to data */
        struct {
            int _d_dirpos:31;   /* rsrc ptr to resource directory... */
            int _d_dirent:1;    /* ...if this bit is set */
        } _d_d;
    } _d;
};

#define COFF_RESDIR  struct COFF_resdir
#define COFF_RESDSZ  8          /* sizeof(COFF_RESDIR) */

/* useful resource directory defines */
#define rd_name     _n._n_name
#define rd_id       _n._n_id
#define rd_strpos   _n._n_n._n_strpos
#define rd_strent   _n._n_n._n_strent
#define rd_offset   _d._d_offset
#define rd_dirpos   _d._d_d._d_dirpos
#define rd_dirent   _d._d_d._d_dirent

/*
 * Win32 resource directory string (pointed to by _n_strpos).
 */
struct COFF_resstr {
    ushort_t rs_size;           /* length of string */
    wchar_t rs_name[1];         /* value of string */
};

#define COFF_RESSTR  struct COFF_resstr

/*
 * Win32 resource data entry.
 */
struct COFF_resent {
    long re_offset;             /* offset to data */
    long re_size;               /* size of data */
    long re_codepage;           /* codepage of the data */
    long re_reserved;           /* reserved */
};

#define COFF_RESENT  struct COFF_resent
#define COFF_RESESZ  16         /* sizeof(COFF_RESENT) */

/*
 * WinCE exception record (ARM).
 */
struct COFF_armfcn {
    addr_t af_fcnaddr;          /* offset to first byte of function code */
    int af_prolog:8;            /* size of prolog in instructions */
    int af_fcnsize:22;          /* size of function in instructions */
    int af_32bit:1;             /* 32-bit instructions? */
    int af_except:1;            /* function has SEH? */
};

#define COFF_ARMFCN  struct COFF_armfcn
#define COFF_ARMFSZ  8          /* sizeof(COFF_ARMFCN) */

/*
 * Win32 debug directory entry.
 */
struct COFF_dbgdir {
    ulong_t dd_flags;           /* flags */
    long dd_timdat;             /* time & date stamp */
    ushort_t dd_majver;         /* major version number */
    ushort_t dd_minver;         /* minor version number */
    long dd_type;               /* information type */
    long dd_size;               /* size of debug data */
    addr_t dd_addr;             /* RVA of debug data */
    long dd_dbgptr;             /* file ptr to debug data */
};

#define COFF_DBGDIR  struct COFF_dbgdir
#define COFF_DBGDSZ  28         /* sizeof(COFF_DBGDIR) */

/* d_type values */
#define COFF_DTYP_UNKNOWN   0
#define COFF_DTYP_COFF      1
#define COFF_DTYP_CODEVIEW  2
#define COFF_DTYP_FPO       3
#define COFF_DTYP_MISC      4
#define COFF_DTYP_EXCEPTION 5
#define COFF_DTYP_FIXUP     6
#define COFF_DTYP_OMAPTOSRC 7
#define COFF_DTYP_OMAPFRSRC 8
#define COFF_DTYP_BORLAND   9

/*
 * Win32 COFF symbols header.
 */
struct COFF_dbgcoff {
    long dc_nsyms;              /* number of symbols */
    long dc_symptr;             /* debug ptr to first symbol */
    long dc_nlnno;              /* number of linenumbers */
    long dc_lnnoptr;            /* debug ptr to first linenumber */
    addr_t dc_text_start;       /* RVA to first byte of code */
    addr_t dc_text_end;         /* RVA to last byte of code */
    addr_t dc_data_start;       /* RVA to first byte of data */
    addr_t dc_data_end;         /* RVA to last byte of data */
};

#define COFF_DBGCOFF struct COFF_dbgcoff
#define COFF_DBGCOSZ 32         /* sizeof(COFF_DBGCOFF) */

/*
 * Win32 Frame Pointer Omission debugging record.
 */
struct COFF_dbgfpo {
    addr_t df_fcnaddr;          /* offset to first byte of function code */
    long df_fcnsize;            /* size of function in bytes */
    long df_locals;             /* size of locals in bytes */
    short df_params;            /* size of parameters in bytes / 4 */
    char df_prolog;             /* size of prolog in bytes */
    uchar_t df_flags;           /* bits and pieces */
};

#define COFF_DBGFPO struct COFF_dbgfpo
#define COFF_DBGFSZ 16          /* sizeof(COFF_DBGFPO) */

/* df_flags values */
#define COFF_DF_REGSMASK    0x07
#define COFF_DF_HASSEH      0x08
#define COFF_DF_USESBP      0x10
#define COFF_DF_RESERVED    0x20    /* reserved bit */
#define COFF_DF_TYPE_FPO    0x00
#define COFF_DF_TYPE_TRAP   0x40
#define COFF_DF_TYPE_TSS    0x80
#define COFF_DF_TYPE_NONFPO 0xc0
#define COFF_DF_TYPEMASK    0xc0

/*
 * Win32 Misc debugging record.
 */
struct COFF_dbgmisc {
    ulong_t dm_type;            /* type of misc data */
    long dm_size;               /* total size of record, aligned to 4 byte */
    char dm_unicode;            /* true if data is unicode string */
    char dm_reserved[3];        /* see manual */
    char dm_data[1];            /* actual data (variable) */
};

#define COFF_DBGMISC  struct COFF_dbgmisc

/* dm_type values */
#define COFF_DM_EXENAME     1

/*
 * Debugging information can be stripped from an image file and placed
 * in a separate .DBG file, whose file name part is the same as the
 * image file name part (e.g. symbols for CMD.EXE could be stripped and
 * placed in CMD.DBG).  This is indicated by the COFF_F_DBG flag in the
 * f_flags field of the file header.  The beginning of the .DBG file
 * contains the following structure which captures certain information
 * from the image file.  This allows a debug to proceed even if the
 * original image file is not accessable.  This header is followed by
 * zero of more COFF_SCNHDR structures, followed by zero or more
 * COFF_DBGDIR structures.  The latter structures and those in the image
 * file contain file offsets relative to the beginning of the .DBG file.
 *
 * If symbols have been stripped from an image, the COFF_DBGMISC structure
 * is left in the image file, but not mapped.  This allows a debugger to
 * compute the name of the .DBG file, from the name of the image in the
 * COFF_DBGMISC structure.
 */

/*
 * Win32 Separate debug header.
 */
struct COFF_dbgsep {
    ushort_t ds_sig;            /* magic number (COFF_DS_MAG) */
    ushort_t ds_flags;          /* flags */
    ushort_t ds_magic;          /* magic number (machine) */
    ushort_t ds_flags2;         /* "characteristics" */
    long ds_timdat;             /* time & date stamp */
    long ds_chksum;             /* checksum */
    addr_t ds_pebase;           /* image base */
    long ds_pesize;             /* image size in bytes */
    long ds_nscns;              /* number of sections */
    long ds_expnamsz;           /* size of exported names */
    long ds_dbgdirsz;           /* size of debug directory */
    long ds_salign;             /* section alignment */
    long ds_reserved[2];
};

#define COFF_DBGSEP struct COFF_dbgsep
#define COFF_DBGSSZ 48          /* sizeof(COFF_DBGSEP) */

/* ds_magic values */
#define COFF_DS_MAG_SIG     0x4944

/* ds_flags values */
#define COFF_DS_MISMATCH    0x8000

#endif /* NOWIN32 */

#include "poppack.h"  /* back to default */

#endif /* _COFF_H */
