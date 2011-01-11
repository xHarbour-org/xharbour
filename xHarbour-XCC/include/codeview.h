/****************************************************************************
 *                                                                          *
 * File    : codeview.h                                                     *
 *                                                                          *
 * Purpose : Win32 CodeView (CV4) debug information defines.                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-12-02  Created by Pelle Orinius.                            *
 *           99-11-29  Changed from Windows to standard data types.         *
 *                                                                          *
 ****************************************************************************/

#ifndef _CODEVIEW_H
#define _CODEVIEW_H

#ifdef __cplusplus
extern "C" {
#endif

#include <potypes.h>

typedef ulong_t cv_typ32_t;   /* (CV_typ32_t) */
typedef ushort_t cv_typ16_t;  /* (CV_typ16_t) */
typedef ushort_t cv_typ_t;    /* (CV_typ_t) */

/*
 * CodeView Debug signature.
 *
 * At the base address is a signature whose filepos field points to the
 * first CODEVIEW_DIRECTORY_HEADER in a chain of directories.
 */
typedef struct _CODEVIEW_SIGNATURE {
    char signature[4];              /* "NBxx" */
    long offset;                    /* offset in file */
} CODEVIEW_SIGNATURE;

#define CODEVIEW_SIGNATURE_NB02  "NB02"     /* linked by MS link 5.10 or equivalent OEM linker */
#define CODEVIEW_SIGNATURE_NB05  "NB05"     /* from linker 5.20 or later; *not* packed */
#define CODEVIEW_SIGNATURE_NB07  "NB07"     /* Quick C for Windows 1.0 only */
#define CODEVIEW_SIGNATURE_NB08  "NB08"     /* CV 4.00 - 4.05, packed */
#define CODEVIEW_SIGNATURE_NB09  "NB09"     /* CV 4.10, packed */
#define CODEVIEW_SIGNATURE_NB10  "NB10"     /* Separate PDB file; format as NB09/NB11 */
#define CODEVIEW_SIGNATURE_NB11  "NB11"     /* VC++ 5.0 or later, packed & bonded */

typedef struct _CODEVIEW_SEPARATE_FILE {
    long timdat;                    /* time/date stamp */
    long version;                   /* version number */
    char name[1];                   /* NUL-terminated name of PDB file */
} CODEVIEW_SEPARATE_FILE;

/*
 * Directory information structure.
 *
 * This structure contains the information describing the directory.
 * It is pointed to by the signature at the base address or the directory
 * link field of a preceeding directory.  The directory entries immediately
 * follow this structure.
 */
typedef struct _CODEVIEW_DIRECTORY_HEADER {
    ushort_t sizeof_dirhdr;             /* length of this structure */
    ushort_t sizeof_dirent;             /* number of bytes in each directory entry */
    long ndirents;                      /* number of directory entries */
    long offset_nextdir;                /* offset to next directory */
    ulong_t flags;                      /* flags (not used) */
} CODEVIEW_DIRECTORY_HEADER;

/*
 * Directory structure.
 *
 * The data in this structure is used to reference the data for each
 * subsection of the CodeView Debug information.  Tables that are
 * not associated with a specific module will have a module index of
 * 0xffff.  These tables are the global types table, the global symbol
 * table, the global public table and the library table.
 */
#define CODEVIEW_MODULE_INDEX_NONE  ((short)0xFFFFU)

typedef struct _CODEVIEW_DIRECTORY_ENTRY {
    ushort_t subsect_type;              /* subsection type, see below */
    short modindex;                     /* module index */
    long offset_subsect;                /* offset to subsection data */
    long sizeof_subsect;                /* size of subsection data */
} CODEVIEW_DIRECTORY_ENTRY;

/*
 * Subsection types.
 */
#define CODEVIEW_SUBSECTION_MODULE      0x120
#define CODEVIEW_SUBSECTION_TYPES       0x121
#define CODEVIEW_SUBSECTION_PUBLIC      0x122
#define CODEVIEW_SUBSECTION_PUBLICSYM   0x123   /* publics as symbol (waiting for link) */
#define CODEVIEW_SUBSECTION_SYMBOLS     0x124
#define CODEVIEW_SUBSECTION_ALIGNSYM    0x125
#define CODEVIEW_SUBSECTION_SRCLNSEG    0x126   /* because link doesn't emit SrcModule */
#define CODEVIEW_SUBSECTION_SRCMODULE   0x127
#define CODEVIEW_SUBSECTION_LIBRARIES   0x128
#define CODEVIEW_SUBSECTION_GLOBALSYM   0x129
#define CODEVIEW_SUBSECTION_GLOBALPUB   0x12A
#define CODEVIEW_SUBSECTION_GLOBALTYPES 0x12B
#define CODEVIEW_SUBSECTION_MPC         0x12C
#define CODEVIEW_SUBSECTION_SEGMAP      0x12D
#define CODEVIEW_SUBSECTION_SEGNAME     0x12E
#define CODEVIEW_SUBSECTION_PRECOMP     0x12F   /* precompiled types */
#define CODEVIEW_SUBSECTION_PRECOMPMAP  0x130   /* map precompiled types in global types */
#define CODEVIEW_SUBSECTION_OFFSETMAP16 0x131
#define CODEVIEW_SUBSECTION_OFFSETMAP32 0x132
#define CODEVIEW_SUBSECTION_FILEINDEX   0x133
#define CODEVIEW_SUBSECTION_STATICSYM   0x134

/* Information describing each segment in a module */

typedef struct _CODEVIEW_SEGMENT {
    short segindex;                     /* segment index */
    short pad;                          /* padding to maintain alignment */
    long offset_code;                   /* offset of code in segment */
    long sizeof_code;                   /* number of bytes in segment */
} CODEVIEW_SEGMENT;

/*
 * Per module information.
 *
 * There is one of these subsection entries for each module
 * in the executable.  The entry is generated by link.
 * This table will probably require padding because of the
 * variable length module name.
 */
#define CODEVIEW_MODULE_DEBUG_STYLE     "CV"

typedef struct _CODEVIEW_MODULE {
    short overlay_number;               /* overlay number */
    short libindex;                     /* library that the module was linked from */
    ushort_t nsegs;                     /* count of number of segments in module */
    char style[2];                      /* debugging style "CV" */
    CODEVIEW_SEGMENT segment[1];        /* describes segments in module */
    uchar_t name[1];                    /* length-prefixed name */
} CODEVIEW_MODULE;

/*
 * Public symbol information.
 */

#define CODEVIEW_PUBLIC_SYMBOL_VERSION  2
#define CODEVIEW_SYMBOLS_VERSION  1

typedef struct _CODEVIEW_PUBLIC_SYMBOL {
    ushort_t length;                    /* length of symbol info */
    ushort_t type;                      /* symbol type: PUBLIC_SYMTYPE_PUB32 */
    cv_typ32_t type_index;              /* *** Not documented by MS, this is a guess! *** */
    long offset;                        /* offset portion of the address */
    short segment;                      /* segment portion of the address */
    uchar_t name[1];                    /* length-prefixed name */
} CODEVIEW_PUBLIC_SYMBOL;

/*
 * Symbol hash table format.
 *
 * This structure immediately preceeds the global publics table
 * and global symbol tables.
 */
typedef struct _CODEVIEW_SYMBOL_HASH {
    short sym_hash_index;               /* symbol hash function index */
    short addr_hash_index;              /* address hash function index */
    long sizeof_syminfo;                /* length of symbol information */
    long sizeof_sym_hash_data;          /* length of symbol hash data */
    long sizeof_addr_hash_data;         /* length of address hash data */
} CODEVIEW_SYMBOL_HASH;

/*
 * Segment mapping subsection format.
 *
 * This table contains the mapping between the logical segment
 * indices used in the symbol table and the physical segments
 * where the program was loaded.
 */
typedef struct _CODEVIEW_SEGMENT_DESCRIPTOR {
    ushort_t read: 1;                   /* the segment is readable */
    ushort_t write: 1;                  /* the segment is writable */
    ushort_t execute: 1;                /* the segment is executable */
    ushort_t bits32: 1;                 /* the descriptor describes a 32-bit linear address */
    ushort_t reserved1: 4;              /* gee, i dunno */
    ushort_t selector: 1;               /* frame represents a selector */
    ushort_t absolute: 1;               /* frame represents an absolute address */
    ushort_t reserved2: 2;
    ushort_t group: 1;                  /* if set, the descriptor represents a group */
    ushort_t reserved3: 3;
    short overlay_number;               /* overlay number */
    short group_number;                 /* index into descriptor array. Must be 0 or cSegLog <= group < cSeg */
    short frame;                        /* frame */
    short segname_index;                /* byte index of the segment in the sstSegName table (or 0xFFFF) */
    short clsname_index;                /* byte index of the class in the sstSegName table (or 0xFFFF) */
    long offset_segment;                /* byte offset of the logical segment within the physical segment */
    long sizeof_segment;                /* byte count of the logical segment */
} CODEVIEW_SEGMENT_DESCRIPTOR;

typedef struct _CODEVIEW_SEGMENT_MAP {
    ushort_t nsegs;                     /* number of segments */
    ushort_t nlogsegs;                  /* number of logical segments */
    CODEVIEW_SEGMENT_DESCRIPTOR segment[1];
} CODEVIEW_SEGMENT_MAP;

/*
 * Precompiled types mapping table.
 *
 * This table should be ignored by all consumers except the
 * incremental packer.
 */
typedef struct _CODEVIEW_PRECOMPILED_MAP {
    ushort_t first_type_index;          /* first precompiled type index */
    ushort_t ntypes;                    /* number of precompiled types */
    long signature;                     /* precompiled types signature */
    short pad;                          /* padding to maintain alignment */
/*   cv_typ_t map[];  */                /* mapping of precompiled types */
} CODEVIEW_PRECOMPILED_MAP;

/* Start-end offset pair */

typedef struct _CODEVIEW_START_END {
    long offset_start;
    long offset_end;
} CODEVIEW_START_END;

/*
 * Source line to address mapping table.
 *
 * This table is generated by the link/ilink utility from line number
 * information contained in the object file OMF data.  This table contains
 * only the code contribution for one segment from one source file.
 */
typedef struct _CODEVIEW_SOURCELINE {
    short segindex;                     /* linker segment index */
    ushort_t nlines;                    /* number of line/offset pairs */
    long offset[1];                     /* array of offsets in segment (* nlines) */
    ushort_t lnno[1];                   /* array of line numbers (* nlines) */
} CODEVIEW_SOURCELINE;

/*
 * Source file description.
 *
 * This table is generated by the linker.
 */
typedef struct _CODEVIEW_SOURCEFILE {
    ushort_t nsegs;                     /* number of segments that receive code from this source file */
    short pad;                          /* pad field used to maintain alignment */
    long base_source_line[1];           /* base of CODEVIEW_SOURCELINE tables (* number_of_segments) */
    CODEVIEW_START_END offsets[1];      /* array of CODEVIEW_START_END (* number_of_segments) */
    uchar_t name[1];                    /* length-prefixed name of source file */
} CODEVIEW_SOURCEFILE;

/*
 * Source line to address mapping header structure.
 *
 * This structure describes the number and location of the
 * OMFAddrLine tables for a module.  The offSrcLine entries are
 * relative to the beginning of this structure.
 */
typedef struct _CODEVIEW_SOURCEMODULE {
    ushort_t nfiles;                    /* number of source files contributing code to segments */
    ushort_t nsegs;                     /* number of code segments receiving code from this module */
    long base_source_file[1];           /* base of CODEVIEW_SOURCEFILE table. (* nfiles) */
    CODEVIEW_START_END offsets[1];      /* array of CODEVIEW_START_END (* nsegs) */
    short segindex[1];                  /* array of segment indices that receive code from this module */
} CODEVIEW_SOURCEMODULE;

typedef struct _CODEVIEW_FILEINDEX {
    ushort_t nfiles;                    /* count of the number of modules in the executable */
    ushort_t nrefs;                     /* count of the total number of file name references */
    short name_indexes[1];              /* array of indices into name_offsets table (* nfiles) */
    short reference_count[1];           /* number of file name references per module (* nfiles) */
    long name_offsets[1];               /* array of offsets into the names table (* nrefs) */
    uchar_t names[1];                   /* list of length prefixed file names */
} CODEVIEW_FILEINDEX;

/*
 * Symbol types.
 */
#define CODEVIEW_SYMTYPE_COMPILE    0x0001  /* compile flags symbol */
#define CODEVIEW_SYMTYPE_REGISTER   0x0002  /* register variable */
#define CODEVIEW_SYMTYPE_CONSTANT   0x0003  /* constant symbol */
#define CODEVIEW_SYMTYPE_UDT        0x0004  /* user-defined type */
#define CODEVIEW_SYMTYPE_SSEARCH    0x0005  /* start search */
#define CODEVIEW_SYMTYPE_END        0x0006  /* end block, proc, with, or thunk */
#define CODEVIEW_SYMTYPE_SKIP       0x0007  /* skip - Reserve symbol space */
#define CODEVIEW_SYMTYPE_CVRESERVE  0x0008  /* reserved for CodeView use */
#define CODEVIEW_SYMTYPE_OBJNAME    0x0009  /* specify name of object file */
#define CODEVIEW_SYMTYPE_ENDARG     0x000A  /* specify end of arguments in function symbols */
#define CODEVIEW_SYMTYPE_COBOLUDT   0x000B  /* Microfocus COBOL user-defined type */
#define CODEVIEW_SYMTYPE_MANYREG    0x000C  /* many register symbol */
#define CODEVIEW_SYMTYPE_RETURN     0x000D  /* function return description */
#define CODEVIEW_SYMTYPE_ENTRYTHIS  0x000E  /* description of this pointer at entry */
#define CODEVIEW_SYMTYPE_BPREL16    0x0100  /* BP relative 16:16 */
#define CODEVIEW_SYMTYPE_LDATA16    0x0101  /* local data 16:16 */
#define CODEVIEW_SYMTYPE_GDATA16    0x0102  /* global data 16:16 */
#define CODEVIEW_SYMTYPE_PUB16      0x0103  /* public symbol 16:16 */
#define CODEVIEW_SYMTYPE_LPROC16    0x0104  /* local procedure start 16:16 */
#define CODEVIEW_SYMTYPE_GPROC16    0x0105  /* global procedure start 16:16 */
#define CODEVIEW_SYMTYPE_THUNK16    0x0106  /* thunk start 16:16 */
#define CODEVIEW_SYMTYPE_BLOCK16    0x0107  /* block start 16:16 */
#define CODEVIEW_SYMTYPE_WITH16     0x0108  /* with start 16:16 */
#define CODEVIEW_SYMTYPE_LABEL16    0x0109  /* code label 16:16 */
#define CODEVIEW_SYMTYPE_CEXMODEL16 0x010A  /* change execution model 16:16 */
#define CODEVIEW_SYMTYPE_VFTPATH16  0x010B  /* virtual function table path descriptor 16:16 */
#define CODEVIEW_SYMTYPE_REGREL16   0x010C  /* specify 16:16 offset relative to arbitrary register */
#define CODEVIEW_SYMTYPE_BPREL32    0x0200  /* BP relative 16:32 */
#define CODEVIEW_SYMTYPE_LDATA32    0x0201  /* local data 16:32 */
#define CODEVIEW_SYMTYPE_GDATA32    0x0202  /* global data 16:32 */
#define CODEVIEW_SYMTYPE_PUB32      0x0203  /* public symbol 16:32 */
#define CODEVIEW_SYMTYPE_LPROC32    0x0204  /* local procedure start 16:32 */
#define CODEVIEW_SYMTYPE_GPROC32    0x0205  /* global procedure start 16:32 */
#define CODEVIEW_SYMTYPE_THUNK32    0x0206  /* thunk start 16:32 */
#define CODEVIEW_SYMTYPE_BLOCK32    0x0207  /* block start 16:32 */
#define CODEVIEW_SYMTYPE_WITH32     0x0208  /* with start 16:32 */
#define CODEVIEW_SYMTYPE_LABEL32    0x0209  /* code label 16:32 */
#define CODEVIEW_SYMTYPE_CEXMODEL32 0x020A  /* Change execution model 16:32 */
#define CODEVIEW_SYMTYPE_VFTPATH32  0x020B  /* virtual function table path descriptor 16:32 */
#define CODEVIEW_SYMTYPE_REGREL32   0x020C  /* specify 16:32 offset relative to arbitrary register */
#define CODEVIEW_SYMTYPE_LTHREAD32  0x020D  /* Local Thread Storage data */
#define CODEVIEW_SYMTYPE_GTHREAD32  0x020E  /* Global Thread Storage data */
#define CODEVIEW_SYMTYPE_LPROCMIPS  0x0300  /* local procedure start MIPS */
#define CODEVIEW_SYMTYPE_GPROCMIPS  0x0301  /* global procedure start MIPS */
#define CODEVIEW_SYMTYPE_PROCREF    0x0400  /* reference to a procedure */
#define CODEVIEW_SYMTYPE_DATAREF    0x0401  /* reference to data */
#define CODEVIEW_SYMTYPE_ALIGN      0x0402  /* page align symbols */

/*
 * All symbol records that included one or more CV_typ_t
 * fields have been renamed by having "_16t" appended to their
 * original names. For instance, the symbol record for S_BPREL32
 * that included a 16-bit CV_typ_t (now known as CV_typ16_t) is
 * now S_BPREL32_16t. The records that were so changed all have
 * indices (record type fields) with the 0x1000 bit set. Records
 * that did not contain CV_typ_t fields were left unchanged.
 */

#define CODEVIEW_SYMTYPE_REGISTER_16T   0x1001  /* register variable */
#define CODEVIEW_SYMTYPE_CONSTANT_16T   0x1002  /* constant symbol */
#define CODEVIEW_SYMTYPE_UDT_16T        0x1003  /* user-defined type */
#define CODEVIEW_SYMTYPE_COBOLUDT_16T   0x1004  /* Microfocus COBOL user-defined type */
#define CODEVIEW_SYMTYPE_MANYREG_16T    0x1005  /* many register symbol */
#define CODEVIEW_SYMTYPE_BPREL32_16T    0x1006  /* BP relative 16:32 */
#define CODEVIEW_SYMTYPE_LDATA32_16T    0x1007  /* local data 16:32 */
#define CODEVIEW_SYMTYPE_GDATA32_16T    0x1008  /* global data 16:32 */
#define CODEVIEW_SYMTYPE_PUB32_16T      0x1009  /* public symbol 16:32 */
#define CODEVIEW_SYMTYPE_LPROC32_16T    0x100A  /* local procedure start 16:32 */
#define CODEVIEW_SYMTYPE_GPROC32_16T    0x100B  /* global procedure start 16:32 */
#define CODEVIEW_SYMTYPE_VFTPATH32_16T  0x100C  /* virtual function table path descriptor 16:32 */
#define CODEVIEW_SYMTYPE_REGREL32_16T   0x100D  /* specify 16:32 offset relative to arbitrary register */
#define CODEVIEW_SYMTYPE_LTHREAD32_16T  0x100E  /* Local Thread Storage data */
#define CODEVIEW_SYMTYPE_GTHREAD32_16T  0x100F  /* Global Thread Storage data */
#define CODEVIEW_SYMTYPE_LPROCMIPS_16T  0x1010  /* local procedure start MIPS */
#define CODEVIEW_SYMTYPE_GPROCMIPS_16T  0x1011  /* global procedure start MIPS */

typedef struct _CODEVIEW_SYMBOL_PROCFLAGS {
    uchar_t fpo: 1;                 /* true if function has frame pointer omitted */
    uchar_t interrupt: 1;           /* true if function performs return from interrupt */
    uchar_t retfar: 1;              /* true if function performs far return */
    uchar_t retnever: 1;            /* true if function never returns */
    uchar_t unused: 4;
} CODEVIEW_SYMBOL_PROCFLAGS;

typedef struct _CODEVIEW_SYMBOL_GENERIC {
    ushort_t length;                /* length of symbol info */
    ushort_t type;                  /* symbol type: CODEVIEW_SYMTYPE_??? */
} CODEVIEW_SYMBOL_GENERIC;

typedef struct _CODEVIEW_SYMBOL_COMPILE {
    ushort_t length;                /* length of symbol info */
    ushort_t type;                  /* symbol type: CODEVIEW_SYMTYPE_COMPILE */
    uchar_t machine;                /* machine ID */
    uchar_t language: 8;            /* development language */
    uchar_t code_present: 1;
    uchar_t float_precision: 2;
    uchar_t float_package: 2;
    uchar_t ambient_data: 3;
    uchar_t ambient_code: 3;
    uchar_t mode32: 1;              /* compiled for 32-bit addresses */
    uchar_t reserved: 4;
    uchar_t version[1];             /* length prefixed version string */
} CODEVIEW_SYMBOL_COMPILE;

/* Machine enumeration */
#define CODEVIEW_SYMBOL_MACHINE_I80         0x00    /* Intel 8080 */
#define CODEVIEW_SYMBOL_MACHINE_I86         0x01    /* Intel 8086 */
#define CODEVIEW_SYMBOL_MACHINE_I286        0x02    /* Intel 80286 */
#define CODEVIEW_SYMBOL_MACHINE_I386        0x03    /* Intel 80386 */
#define CODEVIEW_SYMBOL_MACHINE_I486        0x04    /* Intel 80486 */
#define CODEVIEW_SYMBOL_MACHINE_PENTIUM     0x05    /* Intel Pentium */
#define CODEVIEW_SYMBOL_MACHINE_PENTIUMPRO  0x06    /* Intel Pentium Pro */
#define CODEVIEW_SYMBOL_MACHINE_R4000       0x10    /* MIPS R4000 */
#define CODEVIEW_SYMBOL_MACHINE_MC68000     0x20    /* Motorola 68000 */
#define CODEVIEW_SYMBOL_MACHINE_MC68010     0x21    /* Motorola 68010 */
#define CODEVIEW_SYMBOL_MACHINE_MC68020     0x22    /* Motorola 68020 */
#define CODEVIEW_SYMBOL_MACHINE_MC68030     0x23    /* Motorola 68030 */
#define CODEVIEW_SYMBOL_MACHINE_MC68040     0x24    /* Motorola 68040 */
#define CODEVIEW_SYMBOL_MACHINE_ALPHA       0x30    /* Alpha */
#define CODEVIEW_SYMBOL_MACHINE_PPC601      0x40
#define CODEVIEW_SYMBOL_MACHINE_PPC603      0x41
#define CODEVIEW_SYMBOL_MACHINE_PPC604      0x42
#define CODEVIEW_SYMBOL_MACHINE_PPC620      0x43

/* Language enumeration */
#define CODEVIEW_SYMBOL_LANGUAGE_C          0       /* C */
#define CODEVIEW_SYMBOL_LANGUAGE_CPP        1       /* C++ */
#define CODEVIEW_SYMBOL_LANGUAGE_FORTRAN    2       /* Fortran */
#define CODEVIEW_SYMBOL_LANGUAGE_MASM       3       /* MASM */
#define CODEVIEW_SYMBOL_LANGUAGE_PASCAL     4       /* Pascal */
#define CODEVIEW_SYMBOL_LANGUAGE_BASIC      5       /* Basic */
#define CODEVIEW_SYMBOL_LANGUAGE_COBOL      6       /* Cobol */
#define CODEVIEW_SYMBOL_LANGUAGE_X          7       /* Malcolm X */

/* Ambient code and data memory model enumeration */
#define CODEVIEW_SYMBOL_AMBIENT_NEAR        0
#define CODEVIEW_SYMBOL_AMBIENT_FAR         1
#define CODEVIEW_SYMBOL_AMBIENT_HUGE        2

/* Float package enumeration */
#define CODEVIEW_SYMBOL_FLOATPACK_HARDWARE  0       /* Hardware processor */
#define CODEVIEW_SYMBOL_FLOATPACK_EMUL      1       /* Emulator */
#define CODEVIEW_SYMBOL_FLOATPACK_ALTMATH   2       /* Alternate math */

/*
 * The FloatPrecision flag is set to 1 if the compiler follows the ANSI C
 * floating point precision rules. This is specified to Microsoft C compilers
 * by setting the -Op option.
 */

typedef struct _CODEVIEW_SYMBOL_REGISTER {
    ushort_t length;            /* length of symbol info */
    ushort_t type;              /* symbol type: CODEVIEW_SYMTYPE_REGISTER */
    cv_typ_t type_index;        /* type index (CV_typ_t) */
    short register_e;           /* register enumerate */
    uchar_t name[1];            /* length-prefixed name of symbol */
} CODEVIEW_SYMBOL_REGISTER;

typedef struct _CODEVIEW_SYMBOL_REGISTER_16T {
    ushort_t length;            /* length of symbol info */
    ushort_t type;              /* symbol type: CODEVIEW_SYMTYPE_REGISTER_16T */
    cv_typ16_t type_index;      /* type index (CV_typ16_t) */
    ushort_t register_e;        /* register enumerate */
    uchar_t name[1];            /* length-prefixed name of symbol */
} CODEVIEW_SYMBOL_REGISTER_16T;

typedef struct _CODEVIEW_SYMBOL_MANYREG {
    ushort_t length;            /* length of symbol info */
    ushort_t type;              /* symbol type: CODEVIEW_SYMTYPE_MANYREG */
    cv_typ_t type_index;        /* type index of the symbol (CV_typ_t) */
    uchar_t nregs;              /* count of register enumerates that follows */
    uchar_t reglist[1];         /* number_of_regs register enumerates followed by
                                   length-prefixed name. Registers are most significant first */
} CODEVIEW_SYMBOL_MANYREG;

typedef struct _CODEVIEW_SYMBOL_MANYREG_16T {
    ushort_t length;            /* length of symbol info */
    ushort_t type;              /* symbol type: CODEVIEW_SYMTYPE_MANYREG_16T */
    cv_typ16_t type_index;      /* type index of the symbol (CV_typ16_t) */
    uchar_t nregs;              /* count of register enumerates that follows */
    uchar_t reglist[1];         /* number_of_regs register enumerates followed by
                                   length-prefixed name. Registers are most significant first */
} CODEVIEW_SYMBOL_MANYREG_16T;

/* Register enumerations for Intel Architecture */
#define CODEVIEW_SYMBOL_REGISTER_NONE       0
#define CODEVIEW_SYMBOL_REGISTER_AL         1
#define CODEVIEW_SYMBOL_REGISTER_CL         2
#define CODEVIEW_SYMBOL_REGISTER_DL         3
#define CODEVIEW_SYMBOL_REGISTER_BL         4
#define CODEVIEW_SYMBOL_REGISTER_AH         5
#define CODEVIEW_SYMBOL_REGISTER_CH         6
#define CODEVIEW_SYMBOL_REGISTER_DH         7
#define CODEVIEW_SYMBOL_REGISTER_BH         8
#define CODEVIEW_SYMBOL_REGISTER_AX         9
#define CODEVIEW_SYMBOL_REGISTER_CX         10
#define CODEVIEW_SYMBOL_REGISTER_DX         11
#define CODEVIEW_SYMBOL_REGISTER_BX         12
#define CODEVIEW_SYMBOL_REGISTER_SP         13
#define CODEVIEW_SYMBOL_REGISTER_BP         14
#define CODEVIEW_SYMBOL_REGISTER_SI         15
#define CODEVIEW_SYMBOL_REGISTER_DI         16
#define CODEVIEW_SYMBOL_REGISTER_EAX        17
#define CODEVIEW_SYMBOL_REGISTER_ECX        18
#define CODEVIEW_SYMBOL_REGISTER_EDX        19
#define CODEVIEW_SYMBOL_REGISTER_EBX        20
#define CODEVIEW_SYMBOL_REGISTER_ESP        21
#define CODEVIEW_SYMBOL_REGISTER_EBP        22
#define CODEVIEW_SYMBOL_REGISTER_ESI        23
#define CODEVIEW_SYMBOL_REGISTER_EDI        24
#define CODEVIEW_SYMBOL_REGISTER_ES         25
#define CODEVIEW_SYMBOL_REGISTER_CS         26
#define CODEVIEW_SYMBOL_REGISTER_SS         27
#define CODEVIEW_SYMBOL_REGISTER_DS         28
#define CODEVIEW_SYMBOL_REGISTER_FS         29
#define CODEVIEW_SYMBOL_REGISTER_GS         30
#define CODEVIEW_SYMBOL_REGISTER_IP         31
#define CODEVIEW_SYMBOL_REGISTER_FLAGS      32
#define CODEVIEW_SYMBOL_REGISTER_EIP        33
#define CODEVIEW_SYMBOL_REGISTER_EFLAGS     34
#define CODEVIEW_SYMBOL_REGISTER_TEMP       40  /* PCODE register */
#define CODEVIEW_SYMBOL_REGISTER_TEMPH      41  /* PCODE register */
#define CODEVIEW_SYMBOL_REGISTER_QUOTE      42  /* PCODE register */
#define CODEVIEW_SYMBOL_REGISTER_CR0        80
#define CODEVIEW_SYMBOL_REGISTER_CR1        81
#define CODEVIEW_SYMBOL_REGISTER_CR2        82
#define CODEVIEW_SYMBOL_REGISTER_CR3        83
#define CODEVIEW_SYMBOL_REGISTER_DR0        90
#define CODEVIEW_SYMBOL_REGISTER_DR1        91
#define CODEVIEW_SYMBOL_REGISTER_DR2        92
#define CODEVIEW_SYMBOL_REGISTER_DR3        93
#define CODEVIEW_SYMBOL_REGISTER_DR4        94
#define CODEVIEW_SYMBOL_REGISTER_DR5        95
#define CODEVIEW_SYMBOL_REGISTER_DR6        96
#define CODEVIEW_SYMBOL_REGISTER_DR7        97
#define CODEVIEW_SYMBOL_REGISTER_ST0        128
#define CODEVIEW_SYMBOL_REGISTER_ST2        130
#define CODEVIEW_SYMBOL_REGISTER_ST3        131
#define CODEVIEW_SYMBOL_REGISTER_ST4        132
#define CODEVIEW_SYMBOL_REGISTER_ST5        133
#define CODEVIEW_SYMBOL_REGISTER_ST6        134
#define CODEVIEW_SYMBOL_REGISTER_ST7        135
#define CODEVIEW_SYMBOL_REGISTER_CONTROL    136
#define CODEVIEW_SYMBOL_REGISTER_STATUS     137
#define CODEVIEW_SYMBOL_REGISTER_TAG        138
#define CODEVIEW_SYMBOL_REGISTER_FPIP       139
#define CODEVIEW_SYMBOL_REGISTER_FPCS       140
#define CODEVIEW_SYMBOL_REGISTER_FPDO       141
#define CODEVIEW_SYMBOL_REGISTER_FPDS       142
#define CODEVIEW_SYMBOL_REGISTER_ISEM       143
#define CODEVIEW_SYMBOL_REGISTER_FPEIP      144
#define CODEVIEW_SYMBOL_REGISTER_FPEDO      145

typedef struct _CODEVIEW_SYMBOL_CONSTANT {
    ushort_t length;            /* length of symbol info */
    ushort_t type;              /* symbol type: CODEVIEW_SYMTYPE_CONSTANT */
    cv_typ_t type_index;        /* type of symbol or containing enum (CV_typ_t) */
    ushort_t value;             /* numeric leaf containing the value of symbol */
    uchar_t name[1];            /* length-prefixed name of symbol */
} CODEVIEW_SYMBOL_CONSTANT;

typedef struct _CODEVIEW_SYMBOL_CONSTANT_16T {
    ushort_t length;            /* length of symbol info */
    ushort_t type;              /* symbol type: CODEVIEW_SYMTYPE_CONSTANT_16T */
    cv_typ16_t type_index;      /* type of symbol or containing enum (CV_typ16_t) */
    ushort_t value;             /* numeric leaf containing the value of symbol */
    uchar_t name[1];            /* length-prefixed name of symbol */
} CODEVIEW_SYMBOL_CONSTANT_16T;

typedef struct _CODEVIEW_SYMBOL_UDT {
    ushort_t length;            /* length of symbol info */
    ushort_t type;              /* symbol type: CODEVIEW_SYMTYPE_UDT */
    cv_typ_t type_index;        /* type index (CV_typ_t) */
    uchar_t name[1];            /* length-prefixed name or symbol */
} CODEVIEW_SYMBOL_UDT;

typedef struct _CODEVIEW_SYMBOL_UDT_16T {
    ushort_t length;            /* length of symbol info */
    ushort_t type;              /* symbol type: CODEVIEW_SYMTYPE_UDT_16T */
    cv_typ16_t type_index;      /* type index (CV_typ16_t) */
    uchar_t name[1];            /* length-prefixed name or symbol */
} CODEVIEW_SYMBOL_UDT_16T;

typedef struct _CODEVIEW_SYMBOL_SSEARCH {
    ushort_t length;            /* length of symbol info */
    ushort_t type;              /* symbol type: CODEVIEW_SYMTYPE_SSEARCH */
    long symoffset;             /* $$SYMBOL offset of the procedure or thunk
                                   record for this module that has the lowest
                                   offset for the specified segment */
    short segment;              /* segment of symbol */
} CODEVIEW_SYMBOL_SSEARCH;

typedef struct _CODEVIEW_SYMBOL_OBJNAME {
    ushort_t length;            /* length of symbol info */
    ushort_t type;              /* symbol type: CODEVIEW_SYMTYPE_OBJNAME */
    long signature;             /* signature */
    uchar_t name[1];            /* length-prefixed name of object file */
} CODEVIEW_SYMBOL_OBJNAME;

typedef struct _CODEVIEW_SYMBOL_RETURN {
    ushort_t length;            /* length of symbol info */
    ushort_t type;              /* symbol type: CODEVIEW_SYMTYPE_RETURN */
    ushort_t cstyle: 1;         /* true if push varargs right to left */
    ushort_t rsclean: 1;        /* true if returnee stack cleanup */
    ushort_t reserved: 14;
    uchar_t style;              /* function return style */
    uchar_t data[];             /* count + RegList[Count] if Style is 0x01 */
} CODEVIEW_SYMBOL_RETURN;

#define CODEVIEW_SYMBOL_RETURN_VOID             0x00    /* void return */
#define CODEVIEW_SYMBOL_RETURN_REGS             0x01    /* return value is in registers specified in data */
#define CODEVIEW_SYMBOL_RETURN_CALLER_NEAR      0x02    /* indirect caller allocated near */
#define CODEVIEW_SYMBOL_RETURN_CALLER_FAR       0x03    /* indirect caller allocated far */
#define CODEVIEW_SYMBOL_RETURN_RETURNEE_NEAR    0x04    /* indirect returnee allocated near */
#define CODEVIEW_SYMBOL_RETURN_RETURNEE_FAR     0x05    /* indirect returnee allocated far */

typedef struct _CODEVIEW_SYMBOL_BPREL32 {
    ushort_t length;            /* length of symbol info */
    ushort_t type;              /* symbol type: CODEVIEW_SYMTYPE_BPREL32 */
    long offset;                /* signed offset relative to EBP */
    cv_typ_t type_index;        /* type index of the symbol (CV_typ_t) */
    uchar_t name[1];            /* length-prefixed name of symbol */
} CODEVIEW_SYMBOL_BPREL32;

typedef struct _CODEVIEW_SYMBOL_BPREL32_16T {
    ushort_t length;            /* length of symbol info */
    ushort_t type;              /* symbol type: CODEVIEW_SYMTYPE_BPREL32_16T */
    long offset;                /* signed offset relative to EBP */
    cv_typ16_t type_index;      /* type index of the symbol (CV_typ16_t) */
    short pad;
    uchar_t name[1];            /* length-prefixed name of symbol */
} CODEVIEW_SYMBOL_BPREL32_16T;

typedef struct _CODEVIEW_SYMBOL_DATASYM32 {
    ushort_t length;            /* length of symbol info */
    ushort_t type;              /* symbol type: CODEVIEW_SYMTYPE_??? */
    long offset;                /* offset portion of the symbol address */
    short segment;              /* segment portion of the symbol address */
    cv_typ_t type_index;        /* type index of the symbol (CV_typ_t) */
    uchar_t name[1];            /* length-prefixed name of symbol */
} CODEVIEW_SYMBOL_LDATA32,
  CODEVIEW_SYMBOL_GDATA32,
  CODEVIEW_SYMBOL_PUB32;

typedef struct _CODEVIEW_SYMBOL_DATASYM32_16T {
    ushort_t length;            /* length of symbol info */
    ushort_t type;              /* symbol type: CODEVIEW_SYMTYPE_??? */
    cv_typ16_t type_index;      /* type index of the symbol (CV_typ16_t) */
    short pad;
    long offset;                /* offset portion of the symbol address */
    short segment;              /* segment portion of the symbol address */
    uchar_t name[1];            /* length-prefixed name of symbol */
} CODEVIEW_SYMBOL_LDATA32_16T,
  CODEVIEW_SYMBOL_GDATA32_16T,
  CODEVIEW_SYMBOL_PUB32_16T;

typedef struct _CODEVIEW_SYMBOL_PROCSYM32 {
    ushort_t length;                    /* length of symbol info */
    ushort_t type;                      /* symbol type: CODEVIEW_SYMTYPE_??? */
    long parentptr;                     /* pointer to the parent */
    long endptr;                        /* pointer to this blocks end */
    long nextptr;                       /* pointer to next symbol */
    long sizeof_proc;                   /* length in bytes of this procedure */
    long offset_debug_start;            /* offset in bytes from the start of the
                                           procedure to the point where the stack
                                           frame has been set up. Parameter and
                                           frame variables can be viewed at this point */
    long offset_debug_end;              /* offset in bytes from the start of the
                                           procedure to the point where the procedure
                                           is ready to return and has calculated
                                           its return value, if any. Frame and register
                                           variables can still be viewed */
    long offset;                        /* offset portion of the procedure address */
    short segment;                      /* segment portion of the procedure address */
    cv_typ_t type_index;                /* type index (CV_typ_t) */
    CODEVIEW_SYMBOL_PROCFLAGS flags;    /* procedure flags */
    uchar_t name[1];                    /* length-prefixed name of procedure */
} CODEVIEW_SYMBOL_LPROC32,
  CODEVIEW_SYMBOL_GPROC32;

typedef struct _CODEVIEW_SYMBOL_PROCSYM32_16T {
    ushort_t length;                    /* length of symbol info */
    ushort_t type;                      /* Symbol type: CODEVIEW_SYMTYPE_??? */
    long parentptr;                     /* pointer to the parent */
    long endptr;                        /* pointer to this blocks end */
    long nextptr;                       /* pointer to next symbol */
    long sizeof_proc;                   /* length in bytes of this procedure */
    long offset_debug_start;            /* offset in bytes from the start of the
                                           procedure to the point where the stack
                                           frame has been set up. Parameter and
                                           frame variables can be viewed at this point */
    long offset_debug_end;              /* offset in bytes from the start of the
                                           procedure to the point where the procedure
                                           is ready to return and has calculated
                                           its return value, if any. Frame and register
                                           variables can still be viewed */
    cv_typ16_t type_index;              /* type index (CV_typ16_t) */
    short pad;
    long offset;                        /* offset portion of the procedure address */
    short segment;                      /* segment portion of the procedure address */
    CODEVIEW_SYMBOL_PROCFLAGS flags;    /* procedure flags */
    uchar_t name[1];                    /* length-prefixed name of procedure */
} CODEVIEW_SYMBOL_LPROC32_16T,
  CODEVIEW_SYMBOL_GPROC32_16T;

typedef struct _CODEVIEW_SYMBOL_THUNK32 {
    ushort_t length;                    /* length of symbol info */
    ushort_t type;                      /* symbol type: CODEVIEW_SYMTYPE_??? */
    long parentptr;                     /* pointer to the parent */
    long endptr;                        /* pointer to this blocks end */
    long nextptr;                       /* pointer to next symbol */
    long offset;                        /* offset portion of the procedure address */
    short segment;                      /* segment portion of the procedure address */
    short sizeof_thunk;                 /* length in bytes of this thunk */
    uchar_t thunk_type;                 /* ordinal specifying the type of thunk */
    uchar_t name[1];                    /* length-prefixed name of procedure */
    /* Variant[] field */
} CODEVIEW_SYMBOL_THUNK32;

#define CODEVIEW_SYMBOL_THUNK32_NOTYPE      0
#define CODEVIEW_SYMBOL_THUNK32_ADJUSTOR    1
#define CODEVIEW_SYMBOL_THUNK32_VCALL       2
#define CODEVIEW_SYMBOL_THUNK32_PCODE       3

typedef struct _CODEVIEW_SYMBOL_BLOCK32 {
    ushort_t length;                    /* length of symbol info */
    ushort_t type;                      /* symbol type: CODEVIEW_SYMTYPE_BLOCK32 */
    long parentptr;                     /* pointer to the parent */
    long endptr;                        /* pointer to this blocks end */
    long sizeof_block;                  /* length in bytes of the scope of this block */
    long offset;                        /* offset portion of the block address */
    short segment;                      /* segment portion of the block address */
    uchar_t name[1];                    /* length-prefixed name of block */
} CODEVIEW_SYMBOL_BLOCK32;

typedef struct _CODEVIEW_SYMBOL_WITH32 {
    ushort_t length;                    /* length of symbol info */
    ushort_t type;                      /* symbol type: CODEVIEW_SYMTYPE_WITH32 */
    long parentptr;                     /* pointer to the parent */
    long endptr;                        /* pointer to this blocks end */
    long sizeof_block;                  /* length in bytes of the scope of with block */
    long offset;                        /* offset portion of the block address */
    short segment;                      /* segment portion of the block address */
    uchar_t expr[1];                    /* length-prefixed ASCII string, evaluated at run
                                           time, of the expression used in the with statement */
} CODEVIEW_SYMBOL_WITH32;

typedef struct _CODEVIEW_SYMBOL_LABEL32 {
    ushort_t length;                    /* length of symbol info */
    ushort_t type;                      /* symbol type: CODEVIEW_SYMTYPE_LABEL32 */
    long offset;                        /* offset portion of the label address */
    short segment;                      /* segment portion of the label address */
    CODEVIEW_SYMBOL_PROCFLAGS flags;    /* procedure flags */
    uchar_t name[1];                    /* length-prefixed name of label */
} CODEVIEW_SYMBOL_LABEL32;

/* CEXMODEL32 */

typedef struct _CODEVIEW_SYMBOL_VFTPATH32 {
    ushort_t length;                    /* length of symbol info */
    ushort_t type;                      /* symbol type: CODEVIEW_SYMTYPE_VFTPATH32 */
    long offset;                        /* offset portion of the vfunc address */
    short segment;                      /* segment portion of the vfunc address */
    cv_typ_t type_index_root;           /* type index of the root of path (CV_typ_t) */
    cv_typ_t type_index_path;           /* type index of the path record (CV_typ_t) */
} CODEVIEW_SYMBOL_VFTPATH32;

typedef struct _CODEVIEW_SYMBOL_VFTPATH32_16T {
    ushort_t length;                    /* length of symbol info */
    ushort_t type;                      /* symbol type: CODEVIEW_SYMTYPE_VFTPATH32 */
    long offset;                        /* offset portion of the vfunc address */
    short segment;                      /* segment portion of the vfunc address */
    cv_typ16_t type_index_root;         /* type index of the root of path (CV_typ16_t) */
    cv_typ16_t type_index_path;         /* type index of the path record (CV_typ16_t) */
} CODEVIEW_SYMBOL_VFTPATH32_16T;

typedef struct _CODEVIEW_SYMBOL_REGREL32 {
    ushort_t length;                    /* length of symbol info */
    ushort_t type;                      /* symbol type: CODEVIEW_SYMTYPE_REGREL32 */
    long offset;                        /* offset of symbol */
    short register_e;                   /* register index for symbol */
    cv_typ_t type_index;                /* type index (CV_typ_t) */
    uchar_t name[1];                    /* length-prefixed name of symbol */
} CODEVIEW_SYMBOL_REGREL32;

typedef struct _CODEVIEW_SYMBOL_REGREL32_16T {
    ushort_t length;                    /* length of symbol info */
    ushort_t type;                      /* symbol type: CODEVIEW_SYMTYPE_REGREL32 */
    long offset;                        /* offset of symbol */
    short register_e;                   /* register index for symbol */
    cv_typ16_t type_index;              /* type index (CV_typ16_t) */
    uchar_t name[1];                    /* length-prefixed name of symbol */
} CODEVIEW_SYMBOL_REGREL32_16T;

typedef struct _CODEVIEW_SYMBOL_THREAD32 {
    ushort_t length;                    /* length of symbol info */
    ushort_t type;                      /* symbol type: CODEVIEW_SYMTYPE_??? */
    long offset;                        /* offset portion of thread storage */
    short segment;                      /* segment portion of thread storage */
    cv_typ_t type_index;                /* type index (CV_typ_t) */
    uchar_t name[1];                    /* length-prefixed name of symbol */
} CODEVIEW_SYMBOL_LTHREAD32,
  CODEVIEW_SYMBOL_GTHREAD32;

typedef struct _CODEVIEW_SYMBOL_THREAD32_16T {
    ushort_t length;                    /* length of symbol info */
    ushort_t type;                      /* symbol type: CODEVIEW_SYMTYPE_??? */
    long offset;                        /* offset portion of thread storage */
    short segment;                      /* segment portion of thread storage */
    cv_typ16_t type_index;              /* type index (CV_typ16_t) */
    uchar_t name[1];                    /* length-prefixed name of symbol */
} CODEVIEW_SYMBOL_LTHREAD32_16T,
  CODEVIEW_SYMBOL_GTHREAD32_16T;

typedef struct _CODEVIEW_SYMBOL_REFSYM {
    ushort_t length;                    /* length of symbol info */
    ushort_t type;                      /* symbol type: CODEVIEW_SYMTYPE_??? */
    long chksum;                        /* checksum of the referenced symbol name */
    long symoffset;                     /* $$SYMBOL offset of the module */
    short modindex;                     /* Module index */
    ushort_t align;                     /* align this record */
} CODEVIEW_SYMBOL_PROCREF,
  CODEVIEW_SYMBOL_DATAREF;

typedef struct _CODEVIEW_SYMBOL_ALIGN {
    ushort_t length;                    /* length of symbol info */
    ushort_t type;                      /* symbol type: CODEVIEW_SYMTYPE_ALIGN */
} CODEVIEW_SYMBOL_ALIGN, *PCODEVIEW_SYMBOL_ALIGN;

/*
 * Types information.
 */

#define CODEVIEW_TYPES_OLD_VERSION  1   /* we don't support it */
#define CODEVIEW_TYPES_VERSION  2

/*
 * Primitive Type Listing.
 *
 * Primitive types have predefined meaning that is encoded in the
 * values of the various bit fields in the value.
 *
 *   A CodeView primitive type is defined as:
 *
 *   1 1
 *   1 098  7654  3  210
 *   r mode type  r  sub
 *
 *   Where
 *     mode is the pointer mode
 *     type is a type indicator
 *     sub  is a subtype enumeration
 *     r    is a reserved field
 */

/* Special types */
#define CODEVIEW_TYPE_NOTYPE        0x0000  /* no type */
#define CODEVIEW_TYPE_ABSOLUTE      0x0001  /* absolute symbol */
#define CODEVIEW_TYPE_SEGMENT       0x0002  /* segment type */
#define CODEVIEW_TYPE_VOID          0x0003  /* void */
#define CODEVIEW_TYPE_PVOID         0x0103  /* near pointer to void */
#define CODEVIEW_TYPE_PFVOID        0x0203  /* far pointer to void */
#define CODEVIEW_TYPE_PHVOID        0x0303  /* huge pointer to void */
#define CODEVIEW_TYPE_32PVOID       0x0403  /* 32 bit near pointer to void */
#define CODEVIEW_TYPE_32PFVOID      0x0503  /* 32 bit far pointer to void */
#define CODEVIEW_TYPE_CURRENCY      0x0004  /* Basic 8 byte currency */
#define CODEVIEW_TYPE_NBASICSTR     0x0005  /* near Basic string */
#define CODEVIEW_TYPE_FBASICSTR     0x0006  /* far Basic string */
#define CODEVIEW_TYPE_NOTTRANS      0x0007  /* type not translated by CVPACK */
#define CODEVIEW_TYPE_BIT           0x0060  /* bit */
#define CODEVIEW_TYPE_PASCHAR       0x0061  /* Pascal CHAR */

/* Character types */
#define CODEVIEW_TYPE_CHAR          0x0010  /* 8-bit signed */
#define CODEVIEW_TYPE_UCHAR         0x0020  /* 8-bit unsigned */
#define CODEVIEW_TYPE_PCHAR         0x0110  /* near pointer to 8-bit signed */
#define CODEVIEW_TYPE_PUCHAR        0x0120  /* near pointer to 8-bit unsigned */
#define CODEVIEW_TYPE_PFCHAR        0x0210  /* far pointer to 8-bit signed */
#define CODEVIEW_TYPE_PFUCHAR       0x0220  /* far pointer to 8-bit unsigned */
#define CODEVIEW_TYPE_PHCHAR        0x0310  /* huge pointer to 8-bit signed */
#define CODEVIEW_TYPE_PHUCHAR       0x0320  /* huge pointer to 8-bit unsigned */
#define CODEVIEW_TYPE_32PCHAR       0x0410  /* 16:32 near pointer to 8-bit signed */
#define CODEVIEW_TYPE_32PUCHAR      0x0420  /* 16:32 near pointer to 8-bit unsigned */
#define CODEVIEW_TYPE_32PFCHAR      0x0510  /* 16:32 far pointer to 8-bit signed */
#define CODEVIEW_TYPE_32PFUCHAR     0x0520  /* 16:32 far pointer to 8-bit unsigned */

/* Really a character types */
#define CODEVIEW_TYPE_RCHAR         0x0070  /* real char */
#define CODEVIEW_TYPE_PRCHAR        0x0170  /* near pointer to a real char */
#define CODEVIEW_TYPE_PFRCHAR       0x0270  /* far pointer to a real char */
#define CODEVIEW_TYPE_PHRCHAR       0x0370  /* huge pointer to a real char */
#define CODEVIEW_TYPE_32PRCHAR      0x0470  /* 16:32 near pointer to a real char */
#define CODEVIEW_TYPE_32PFRCHAR     0x0570  /* 16:32 far pointer to a real char */

/* Wide character types */
#define CODEVIEW_TYPE_WCHAR         0x0071  /* wide char */
#define CODEVIEW_TYPE_PWCHAR        0x0171  /* near pointer to a wide char */
#define CODEVIEW_TYPE_PFWCHAR       0x0271  /* far pointer to a wide char */
#define CODEVIEW_TYPE_PHWCHAR       0x0371  /* huge pointer to a wide char */
#define CODEVIEW_TYPE_32PWCHAR      0x0471  /* 16:32 near pointer to a wide char */
#define CODEVIEW_TYPE_32PFWCHAR     0x0571  /* 16:32 far pointer to a wide char */

/* Really 16 bit integer types */
#define CODEVIEW_TYPE_INT2          0x0072  /* really 16 bit signed int */
#define CODEVIEW_TYPE_UINT2         0x0073  /* really 16 bit unsigned int */
#define CODEVIEW_TYPE_PINT2         0x0172  /* near pointer to 16 bit signed int */
#define CODEVIEW_TYPE_PUINT2        0x0173  /* near pointer to 16 bit unsigned int */
#define CODEVIEW_TYPE_PFINT2        0x0272  /* far pointer to 16 bit signed int */
#define CODEVIEW_TYPE_PFUINT2       0x0273  /* far pointer to 16 bit unsigned int */
#define CODEVIEW_TYPE_PHINT2        0x0372  /* huge pointer to 16 bit signed int */
#define CODEVIEW_TYPE_PHUINT2       0x0373  /* huge pointer to 16 bit unsigned int */
#define CODEVIEW_TYPE_32PINT2       0x0472  /* 16:32 near pointer to 16 bit signed int */
#define CODEVIEW_TYPE_32PUINT2      0x0473  /* 16:32 near pointer to 16 bit unsigned int */
#define CODEVIEW_TYPE_32PFINT2      0x0572  /* 16:32 far pointer to 16 bit signed int */
#define CODEVIEW_TYPE_32PFUINT2     0x0573  /* 16:32 far pointer to 16 bit unsigned int */

/* 16-bit short types */
#define CODEVIEW_TYPE_SHORT         0x0011  /* 16-bit signed */
#define CODEVIEW_TYPE_USHORT        0x0021  /* 16-bit unsigned */
#define CODEVIEW_TYPE_PSHORT        0x0111  /* near pointer to 16-bit signed */
#define CODEVIEW_TYPE_PUSHORT       0x0121  /* near pointer to 16-bit unsigned */
#define CODEVIEW_TYPE_PFSHORT       0x0211  /* far pointer to 16-bit signed */
#define CODEVIEW_TYPE_PFUSHORT      0x0221  /* far pointer to 16-bit unsigned */
#define CODEVIEW_TYPE_PHSHORT       0x0311  /* huge pointer to 16-bit signed */
#define CODEVIEW_TYPE_PHUSHORT      0x0321  /* huge pointer to 16-bit unsigned */
#define CODEVIEW_TYPE_32PSHORT      0x0411  /* 16:32 near pointer to 16-bit signed */
#define CODEVIEW_TYPE_32PUSHORT     0x0421  /* 16:32 near pointer to 16-bit unsigned */
#define CODEVIEW_TYPE_32PFSHORT     0x0511  /* 16:32 far pointer to 16-bit signed */
#define CODEVIEW_TYPE_32PFUSHORT    0x0521  /* 16:32 far pointer to 16-bit unsigned */

/* Really 32 bit integer types */
#define CODEVIEW_TYPE_INT4          0x0074  /* really 32 bit signed int */
#define CODEVIEW_TYPE_UINT4         0x0075  /* really 32 bit unsigned int */
#define CODEVIEW_TYPE_PINT4         0x0174  /* near pointer to 32 bit signed int */
#define CODEVIEW_TYPE_PUINT4        0x0175  /* near pointer to 32 bit unsigned int */
#define CODEVIEW_TYPE_PFINT4        0x0274  /* far pointer to 32 bit signed int */
#define CODEVIEW_TYPE_PFUINT4       0x0275  /* far pointer to 32 bit unsigned int */
#define CODEVIEW_TYPE_PHINT4        0x0374  /* huge pointer to 32 bit signed int */
#define CODEVIEW_TYPE_PHUINT4       0x0375  /* huge pointer to 32 bit unsigned int */
#define CODEVIEW_TYPE_32PINT4       0x0474  /* 16:32 near pointer to 32 bit signed int */
#define CODEVIEW_TYPE_32PUINT4      0x0475  /* 16:32 near pointer to 32 bit unsigned int */
#define CODEVIEW_TYPE_32PFINT4      0x0574  /* 16:32 far pointer to 32 bit signed int */
#define CODEVIEW_TYPE_32PFUINT4     0x0575  /* 16:32 far pointer to 32 bit unsigned int */

/* 32-bit long types */
#define CODEVIEW_TYPE_LONG          0x0012  /* 32-bit signed */
#define CODEVIEW_TYPE_ULONG         0x0022  /* 32-bit unsigned */
#define CODEVIEW_TYPE_PLONG         0x0112  /* near pointer to 32-bit signed */
#define CODEVIEW_TYPE_PULONG        0x0122  /* near pointer to 32-bit unsigned */
#define CODEVIEW_TYPE_PFLONG        0x0212  /* far pointer to 32-bit signed */
#define CODEVIEW_TYPE_PFULONG       0x0222  /* far pointer to 32-bit unsigned */
#define CODEVIEW_TYPE_PHLONG        0x0312  /* huge pointer to 32-bit signed */
#define CODEVIEW_TYPE_PHULONG       0x0322  /* huge pointer to 32-bit unsigned */
#define CODEVIEW_TYPE_32PLONG       0x0412  /* 16:32 near pointer to 32-bit signed */
#define CODEVIEW_TYPE_32PULONG      0x0422  /* 16:32 near pointer to 32-bit unsigned */
#define CODEVIEW_TYPE_32PFLONG      0x0512  /* 16:32 far pointer to 32-bit signed */
#define CODEVIEW_TYPE_32PFULONG     0x0522  /* 16:32 far pointer to 32-bit unsigned */

/* Really 64 bit integer types */
#define CODEVIEW_TYPE_INT8          0x0076  /* really 64 bit signed int */
#define CODEVIEW_TYPE_UINT8         0x0077  /* really 64 bit unsigned int */
#define CODEVIEW_TYPE_PINT8         0x0176  /* near pointer to 64 bit signed int */
#define CODEVIEW_TYPE_PUINT8        0x0177  /* near pointer to 64 bit unsigned int */
#define CODEVIEW_TYPE_PFINT8        0x0276  /* far pointer to 64 bit signed int */
#define CODEVIEW_TYPE_PFUINT8       0x0277  /* far pointer to 64 bit unsigned int */
#define CODEVIEW_TYPE_PHINT8        0x0376  /* huge pointer to 64 bit signed int */
#define CODEVIEW_TYPE_PHUINT8       0x0377  /* huge pointer to 64 bit unsigned int */
#define CODEVIEW_TYPE_32PINT8       0x0476  /* 16:32 near pointer to 64 bit signed int */
#define CODEVIEW_TYPE_32PUINT8      0x0477  /* 16:32 near pointer to 64 bit unsigned int */
#define CODEVIEW_TYPE_32PFINT8      0x0576  /* 16:32 far pointer to 64 bit signed int */
#define CODEVIEW_TYPE_32PFUINT8     0x0577  /* 16:32 far pointer to 64 bit unsigned int */

/* 64-bit integral types */
#define CODEVIEW_TYPE_QUAD          0x0013  /* 64-bit signed */
#define CODEVIEW_TYPE_UQUAD         0x0023  /* 64-bit unsigned */
#define CODEVIEW_TYPE_PQUAD         0x0113  /* near pointer to 64-bit signed */
#define CODEVIEW_TYPE_PUQUAD        0x0123  /* near pointer to 64-bit unsigned */
#define CODEVIEW_TYPE_PFQUAD        0x0213  /* far pointer to 64-bit signed */
#define CODEVIEW_TYPE_PFUQUAD       0x0223  /* far pointer to 64-bit unsigned */
#define CODEVIEW_TYPE_PHQUAD        0x0313  /* huge pointer to 64-bit signed */
#define CODEVIEW_TYPE_PHUQUAD       0x0323  /* huge pointer to 64-bit unsigned */
#define CODEVIEW_TYPE_32PQUAD       0x0413  /* 16:32 near pointer to 64-bit signed */
#define CODEVIEW_TYPE_32PUQUAD      0x0423  /* 16:32 near pointer to 64-bit unsigned */
#define CODEVIEW_TYPE_32PFQUAD      0x0513  /* 16:32 far pointer to 64-bit signed */
#define CODEVIEW_TYPE_32PFUQUAD     0x0523  /* 16:32 far pointer to 64-bit unsigned */

/* 32-bit real types */
#define CODEVIEW_TYPE_REAL32        0x0040  /* 32-bit real */
#define CODEVIEW_TYPE_PREAL32       0x0140  /* near pointer to 32-bit real */
#define CODEVIEW_TYPE_PFREAL32      0x0240  /* far pointer to 32-bit real */
#define CODEVIEW_TYPE_PHREAL32      0x0340  /* huge pointer to 32-bit real */
#define CODEVIEW_TYPE_32PREAL32     0x0440  /* 16:32 near pointer to 32-bit real */
#define CODEVIEW_TYPE_32PFREAL32    0x0540  /* 16:32 far pointer to 32-bit real */

/* 48-bit real types */
#define CODEVIEW_TYPE_REAL48        0x0044  /* 48-bit real */
#define CODEVIEW_TYPE_PREAL48       0x0144  /* near pointer to 48-bit real */
#define CODEVIEW_TYPE_PFREAL48      0x0244  /* far pointer to 48-bit real */
#define CODEVIEW_TYPE_PHREAL48      0x0344  /* huge pointer to 48-bit real */
#define CODEVIEW_TYPE_32PREAL48     0x0444  /* 16:32 near pointer to 48-bit real */
#define CODEVIEW_TYPE_32PFREAL48    0x0544  /* 16:32 far pointer to 48-bit real */

/* 64-bit real types */
#define CODEVIEW_TYPE_REAL64        0x0041  /* 64-bit real */
#define CODEVIEW_TYPE_PREAL64       0x0141  /* near pointer to 64-bit real */
#define CODEVIEW_TYPE_PFREAL64      0x0241  /* far pointer to 64-bit real */
#define CODEVIEW_TYPE_PHREAL64      0x0341  /* huge pointer to 64-bit real */
#define CODEVIEW_TYPE_32PREAL64     0x0441  /* 16:32 near pointer to 64-bit real */
#define CODEVIEW_TYPE_32PFREAL64    0x0541  /* 16:32 far pointer to 64-bit real */

/* 80-bit real types */
#define CODEVIEW_TYPE_REAL80        0x0042  /* 80-bit real */
#define CODEVIEW_TYPE_PREAL80       0x0142  /* near pointer to 80-bit real */
#define CODEVIEW_TYPE_PFREAL80      0x0242  /* far pointer to 80-bit real */
#define CODEVIEW_TYPE_PHREAL80      0x0342  /* huge pointer to 80-bit real */
#define CODEVIEW_TYPE_32PREAL80     0x0442  /* 16:32 near pointer to 80-bit real */
#define CODEVIEW_TYPE_32PFREAL80    0x0542  /* 16:32 far pointer to 80-bit real */

/* 128-bit real types */
#define CODEVIEW_TYPE_REAL128       0x0043  /* 128-bit real */
#define CODEVIEW_TYPE_PREAL128      0x0143  /* near pointer to 128-bit real */
#define CODEVIEW_TYPE_PFREAL128     0x0243  /* far pointer to 128-bit real */
#define CODEVIEW_TYPE_PHREAL128     0x0343  /* huge pointer to 128-bit real */
#define CODEVIEW_TYPE_32PREAL128    0x0443  /* 16:32 near pointer to 128-bit real */
#define CODEVIEW_TYPE_32PFREAL128   0x0543  /* 16:32 far pointer to 128-bit real */

/* 32-bit complex types */
#define CODEVIEW_TYPE_CPLX32        0x0050  /* 32-bit complex */
#define CODEVIEW_TYPE_PCPLX32       0x0150  /* near pointer to 32-bit complex */
#define CODEVIEW_TYPE_PFCPLX32      0x0250  /* far pointer to 32-bit complex */
#define CODEVIEW_TYPE_PHCPLX32      0x0350  /* huge pointer to 32-bit complex */
#define CODEVIEW_TYPE_32PCPLX32     0x0450  /* 16:32 near pointer to 32-bit complex */
#define CODEVIEW_TYPE_32PFCPLX32    0x0550  /* 16:32 far pointer to 32-bit complex */

/* 64-bit complex types */
#define CODEVIEW_TYPE_CPLX64        0x0051  /* 64-bit complex */
#define CODEVIEW_TYPE_PCPLX64       0x0151  /* near pointer to 64-bit complex */
#define CODEVIEW_TYPE_PFCPLX64      0x0251  /* far pointer to 64-bit complex */
#define CODEVIEW_TYPE_PHCPLX64      0x0351  /* huge pointer to 64-bit complex */
#define CODEVIEW_TYPE_32PCPLX64     0x0451  /* 16:32 near pointer to 64-bit complex */
#define CODEVIEW_TYPE_32PFCPLX64    0x0551  /* 16:32 far pointer to 64-bit complex */

/* 80-bit complex types */
#define CODEVIEW_TYPE_CPLX80        0x0052  /* 80-bit complex */
#define CODEVIEW_TYPE_PCPLX80       0x0152  /* near pointer to 80-bit complex */
#define CODEVIEW_TYPE_PFCPLX80      0x0252  /* far pointer to 80-bit complex */
#define CODEVIEW_TYPE_PHCPLX80      0x0352  /* huge pointer to 80-bit complex */
#define CODEVIEW_TYPE_32PCPLX80     0x0452  /* 16:32 near pointer to 80-bit complex */
#define CODEVIEW_TYPE_32PFCPLX80    0x0552  /* 16:32 far pointer to 80-bit complex */

/* 128-bit complex types */
#define CODEVIEW_TYPE_CPLX128       0x0053  /* 128-bit complex */
#define CODEVIEW_TYPE_PCPLX128      0x0153  /* near pointer to 128-bit complex */
#define CODEVIEW_TYPE_PFCPLX128     0x0253  /* far pointer to 128-bit complex */
#define CODEVIEW_TYPE_PHCPLX128     0x0353  /* huge pointer to 128-bit complex */
#define CODEVIEW_TYPE_32PCPLX128    0x0453  /* 16:32 near pointer to 128-bit complex */
#define CODEVIEW_TYPE_32PFCPLX128   0x0553  /* 16:32 far pointer to 128-bit complex */

/* Boolean types */
#define CODEVIEW_TYPE_BOOL08        0x0030  /* 8-bit boolean */
#define CODEVIEW_TYPE_BOOL16        0x0031  /* 16-bit boolean */
#define CODEVIEW_TYPE_BOOL32        0x0032  /* 32-bit boolean */
#define CODEVIEW_TYPE_BOOL64        0x0033  /* 64-bit boolean */
#define CODEVIEW_TYPE_PBOOL08       0x0130  /* near pointer to 8-bit boolean */
#define CODEVIEW_TYPE_PBOOL16       0x0131  /* near pointer to 16-bit boolean */
#define CODEVIEW_TYPE_PBOOL32       0x0132  /* near pointer to 32-bit boolean */
#define CODEVIEW_TYPE_PBOOL64       0x0133  /* near pointer to 64-bit boolean */
#define CODEVIEW_TYPE_PFBOOL08      0x0230  /* far pointer to 8-bit boolean */
#define CODEVIEW_TYPE_PFBOOL16      0x0231  /* far pointer to 16-bit boolean */
#define CODEVIEW_TYPE_PFBOOL32      0x0232  /* far pointer to 32-bit boolean */
#define CODEVIEW_TYPE_PFBOOL64      0x0233  /* far pointer to 64-bit boolean */
#define CODEVIEW_TYPE_PHBOOL08      0x0330  /* huge pointer to 8-bit boolean */
#define CODEVIEW_TYPE_PHBOOL16      0x0331  /* huge pointer to 16-bit boolean */
#define CODEVIEW_TYPE_PHBOOL32      0x0332  /* huge pointer to 32-bit boolean */
#define CODEVIEW_TYPE_PHBOOL64      0x0333  /* huge pointer to 64-bit boolean */
#define CODEVIEW_TYPE_32PBOOL08     0x0430  /* 16:32 near pointer to 8-bit boolean */
#define CODEVIEW_TYPE_32PFBOOL08    0x0530  /* 16:32 far pointer to 8-bit boolean */
#define CODEVIEW_TYPE_32PBOOL16     0x0431  /* 16:32 near pointer to 16-bit boolean */
#define CODEVIEW_TYPE_32PFBOOL16    0x0531  /* 16:32 far pointer to 16-bit boolean */
#define CODEVIEW_TYPE_32PBOOL32     0x0432  /* 16:32 near pointer to 32-bit boolean */
#define CODEVIEW_TYPE_32PFBOOL32    0x0532  /* 16:32 far pointer to 32-bit boolean */
#define CODEVIEW_TYPE_32PBOOL64     0x0433  /* 16:32 near pointer to 64-bit boolean */
#define CODEVIEW_TYPE_32PFBOOL64    0x0533  /* 16:32 far pointer to 64-bit boolean */

/*
 * Type Leaf Listing.
 *
 * No leaf index can have a value of 0x0000.  The leaf indices are
 * separated into ranges depending upon the use of the type record.
 * The second range is for the type records that are directly referenced
 * in symbols. The first range is for type records that are not
 * referenced by symbols but instead are referenced by other type
 * records.  All type records must have a starting leaf index in these
 * first two ranges.  The third range of leaf indices are used to build
 * up complex lists such as the field list of a class type record.  No
 * type record can begin with one of the leaf indices. The fourth ranges
 * of type indices are used to represent numeric data in a symbol or
 * type record. These leaf indices are greater than 0x8000.  At the
 * point that type or symbol processor is expecting a numeric field, the
 * next two bytes in the type record are examined.  If the value is less
 * than 0x8000, then the two bytes contain the numeric value. If the
 * value is greater than 0x8000, then the data follows the leaf index in
 * a format specified by the leaf index. The final range of leaf indices
 * are used to force alignment of subfields within a complex type record.
 */

typedef struct _CODEVIEW_TYPE_RECORD {
    ushort_t length;                /* length of type info */
    ushort_t leaf_type;             /* leaf type */
    uchar_t variant[];              /* type info */
} CODEVIEW_TYPE_RECORD;

/*
 * Leaf indices starting records but referenced from symbol records.
 */
#define CODEVIEW_LEAFTYPE_MODIFIER      0x1001  /* type modifier */
#define CODEVIEW_LEAFTYPE_POINTER       0x1002  /* pointer */
#define CODEVIEW_LEAFTYPE_ARRAY         0x1003  /* simple array */
#define CODEVIEW_LEAFTYPE_CLASS         0x1004  /* class */
#define CODEVIEW_LEAFTYPE_STRUCTURE     0x1005  /* structure */
#define CODEVIEW_LEAFTYPE_UNION         0x1006  /* union */
#define CODEVIEW_LEAFTYPE_ENUM          0x1007  /* enumeration */
#define CODEVIEW_LEAFTYPE_PROCEDURE     0x1008  /* procedure */
#define CODEVIEW_LEAFTYPE_MFUNCTION     0x1009  /* member function */
#define CODEVIEW_LEAFTYPE_VTSHAPE       0x000A  /* virtual function table shape */
#define CODEVIEW_LEAFTYPE_COBOL0        0x100A
#define CODEVIEW_LEAFTYPE_COBOL1        0x000C
#define CODEVIEW_LEAFTYPE_BARRAY        0x100B  /* basic array */
#define CODEVIEW_LEAFTYPE_LABEL         0x000E  /* label */
#define CODEVIEW_LEAFTYPE_NULL          0x000F  /* null */
#define CODEVIEW_LEAFTYPE_NOTTRAN       0x0010  /* not translated */
#define CODEVIEW_LEAFTYPE_DIMARRAY      0x100C  /* multiply dimensioned array */
#define CODEVIEW_LEAFTYPE_VFTPATH       0x100D  /* path to virtual function table */
#define CODEVIEW_LEAFTYPE_PRECOMP       0x0013  /* reference precompiled types */
#define CODEVIEW_LEAFTYPE_ENDPRECOMP    0x0014  /* end of precompiled types */
#define CODEVIEW_LEAFTYPE_OEM           0x100F  /* OEM generic type */
#define CODEVIEW_LEAFTYPE_TYPESERVER    0x0016  /* reference typeserver */

/*
 * Leaf indices starting records but referenced only from type records.
 */
#define CODEVIEW_LEAFTYPE_SKIP          0x1200  /* skip */
#define CODEVIEW_LEAFTYPE_ARGLIST       0x1201  /* argument list */
#define CODEVIEW_LEAFTYPE_DEFARG        0x1202  /* default argument */
#define CODEVIEW_LEAFTYPE_FIELDLIST     0x1203  /* field list */
#define CODEVIEW_LEAFTYPE_DERIVED       0x1204  /* derived class */
#define CODEVIEW_LEAFTYPE_BITFIELD      0x1205  /* bit fields */
#define CODEVIEW_LEAFTYPE_METHODLIST    0x1206  /* method list */
#define CODEVIEW_LEAFTYPE_DIMCONU       0x1207  /* dimensioned array with constant upper bound */
#define CODEVIEW_LEAFTYPE_DIMCONLU      0x0209  /* dimensioned array with constant lower and upper bounds */
#define CODEVIEW_LEAFTYPE_DIMVARU       0x020A  /* dimensioned array with variable upper bound */
#define CODEVIEW_LEAFTYPE_DIMVARLU      0x020B  /* dimensioned array with variable upper and lower bound */
#define CODEVIEW_LEAFTYPE_REFSYM        0x020C  /* referenced symbol */

/*
 * Leaf indices for fields of complex lists.
 */
#define CODEVIEW_LEAFTYPE_BCLASS        0x1400  /* real base class */
#define CODEVIEW_LEAFTYPE_VBCLASS       0x1401  /* direct virtual base class */
#define CODEVIEW_LEAFTYPE_IVBCLASS      0x1402  /* indirect virtual base class */
#define CODEVIEW_LEAFTYPE_ENUMERATE     0x0403  /* enumeration */
#define CODEVIEW_LEAFTYPE_FRIENDFCN     0x1403  /* friend function */
#define CODEVIEW_LEAFTYPE_INDEX         0x1404  /* index to another type record */
#define CODEVIEW_LEAFTYPE_MEMBER        0x1405  /* data member */
#define CODEVIEW_LEAFTYPE_STMEMBER      0x1406  /* static data member */
#define CODEVIEW_LEAFTYPE_METHOD        0x1407  /* method */
#define CODEVIEW_LEAFTYPE_NESTTYPE      0x1408  /* nested type definition */
#define CODEVIEW_LEAFTYPE_VFUNCTAB      0x1409  /* virtual function table pointer */
#define CODEVIEW_LEAFTYPE_FRIENDCLS     0x040B  /* friend class */
#define CODEVIEW_LEAFTYPE_ONEMETHOD     0x140B  /* one method */
#define CODEVIEW_LEAFTYPE_VFUNCOFF      0x140C  /* virtual function offset */
#define CODEVIEW_LEAFTYPE_NESTTYPEEX    0x140D  /* nested type extended definition */
#define CODEVIEW_LEAFTYPE_MEMBERMODIFY  0x040F  /* member modification */

/*
 * Leaf indices for numeric fields of symbols and type records.
 */
#define CODEVIEW_LEAFTYPE_NUMERIC       0x8000
#define CODEVIEW_LEAFTYPE_CHAR          0x8000
#define CODEVIEW_LEAFTYPE_SHORT         0x8001
#define CODEVIEW_LEAFTYPE_USHORT        0x8002
#define CODEVIEW_LEAFTYPE_LONG          0x8003
#define CODEVIEW_LEAFTYPE_ULONG         0x8004
#define CODEVIEW_LEAFTYPE_REAL32        0x8005
#define CODEVIEW_LEAFTYPE_REAL64        0x8006
#define CODEVIEW_LEAFTYPE_REAL80        0x8007
#define CODEVIEW_LEAFTYPE_REAL128       0x8008
#define CODEVIEW_LEAFTYPE_QUADWORD      0x8009
#define CODEVIEW_LEAFTYPE_UQUADWORD     0x800A
#define CODEVIEW_LEAFTYPE_REAL48        0x800B
#define CODEVIEW_LEAFTYPE_COMPLEX32     0x800C
#define CODEVIEW_LEAFTYPE_COMPLEX64     0x800D
#define CODEVIEW_LEAFTYPE_COMPLEX80     0x800E
#define CODEVIEW_LEAFTYPE_COMPLEX128    0x800F
#define CODEVIEW_LEAFTYPE_VARSTRING     0x8010
#define CODEVIEW_LEAFTYPE_PAD0          0xF0
#define CODEVIEW_LEAFTYPE_PAD1          0xF1
#define CODEVIEW_LEAFTYPE_PAD2          0xF2
#define CODEVIEW_LEAFTYPE_PAD3          0xF3
#define CODEVIEW_LEAFTYPE_PAD4          0xF4
#define CODEVIEW_LEAFTYPE_PAD5          0xF5
#define CODEVIEW_LEAFTYPE_PAD6          0xF6
#define CODEVIEW_LEAFTYPE_PAD7          0xF7
#define CODEVIEW_LEAFTYPE_PAD8          0xF8
#define CODEVIEW_LEAFTYPE_PAD9          0xF9
#define CODEVIEW_LEAFTYPE_PAD10         0xFA
#define CODEVIEW_LEAFTYPE_PAD11         0xFB
#define CODEVIEW_LEAFTYPE_PAD12         0xFC
#define CODEVIEW_LEAFTYPE_PAD13         0xFD
#define CODEVIEW_LEAFTYPE_PAD14         0xFE
#define CODEVIEW_LEAFTYPE_PAD15         0xFF

/* Bit field structure describing class/struct/union/enum properties */
typedef struct _CODEVIEW_TYPELEAF_PROPS {
    ushort_t packed: 1;                 /* true if structure is packed */
    ushort_t ctor: 1;                   /* true if constructors or destructors present */
    ushort_t overops: 1;                /* true if overloaded operators present */
    ushort_t nested: 1;                 /* true if this is a nested class */
    ushort_t cnested: 1;                /* true if this class contains nested types */
    ushort_t opassign: 1;               /* true if overloaded assignment (=) */
    ushort_t opcast: 1;                 /* true if casting methods */
    ushort_t fwdref: 1;                 /* true if forward reference (incomplete defn) */
    ushort_t scoped: 1;                 /* scoped definition */
    ushort_t reserved: 7;
} CODEVIEW_TYPELEAF_PROPS;

typedef struct _CODEVIEW_TYPELEAF_FLDATTR {
    ushort_t access: 2;                 /* access protection (CV_access_t) */
    ushort_t m_prop: 3;                 /* method properties (CV_methodprop_t) */
    ushort_t pseudo: 1;                 /* compiler generated fcn and does not exist */
    ushort_t no_inherit: 1;             /* true if class cannot be inherited */
    ushort_t no_construct: 1;           /* true if class cannot be constructed */
    ushort_t reserved: 8;
} CODEVIEW_TYPELEAF_FLDATTR;

/*
 * Type records.
 */
typedef struct _CODEVIEW_TYPELEAF_MODIFIER {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_MODIFIER */
    ushort_t _const: 1;                 /* const attribute */
    ushort_t _volatile: 1;              /* volatile attribute */
    ushort_t unaligned: 1;              /* unaligned attribute */
    ushort_t reserved: 13;
    cv_typ_t type_index;                /* type index of the modified type (CV_typ_t) */
} CODEVIEW_TYPELEAF_MODIFIER;

typedef struct _CODEVIEW_TYPELEAF_POINTER32 {
    ushort_t length;                    /* length of type info (part of alignment) */
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_POINTER */
    cv_typ32_t type_index;              /* type index of object pointed to */
    uint_t pointer_type: 5;             /* ordinal specifying pointer type (ptrtype_t) */
    uint_t pointer_mode: 3;             /* ordinal specifying pointer mode (ptrmode_t) */
    uint_t flat32: 1;                   /* true if 16:32 pointer */
    uint_t _volatile: 1;                /* true if volatile pointer */
    uint_t _const: 1;                   /* true if const pointer */
    uint_t unaligned: 1;                /* true if unaligned pointer */
    uint_t restricted: 1;               /* true if restricted pointer */
    uint_t reserved: 19;
    uchar_t variant[];
} CODEVIEW_TYPELEAF_POINTER32;

typedef struct _CODEVIEW_TYPELEAF_ARRAY32 {
    ushort_t length;                    /* length of type info (part of alignment) */
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_ARRAY */
    cv_typ32_t type_index_element;      /* type index of element type */
    cv_typ32_t type_index_index;        /* type index of indexing type */
    uchar_t variant[];                  /* variable length data specifying size in bytes and name */
} CODEVIEW_TYPELEAF_ARRAY32;

typedef struct _CODEVIEW_TYPELEAF_CLSSTR32 {
    ushort_t length;                    /* length of type info (part of alignment) */
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_CLASS/STRUCTURE */
    ushort_t nelems;                    /* number of elements in class/struct */
    CODEVIEW_TYPELEAF_PROPS propattr;   /* property attribute field (prop_t) */
    cv_typ32_t type_index_field;        /* type index of LF_FIELD descriptor list */
    cv_typ32_t type_index_dlist;        /* type index of derived from list if not zero */
    cv_typ32_t type_index_vshape;       /* type index of vshape table for this class */
    uchar_t variant[];                  /* variable length data specifying size in bytes and name */
} CODEVIEW_TYPELEAF_CLASS32,
  CODEVIEW_TYPELEAF_STRUCTURE32;

typedef struct _CODEVIEW_TYPELEAF_UNION32 {
    ushort_t length;                    /* length of type info (part of alignment) */
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_UNION */
    ushort_t nelems;                    /* number of elements in union */
    CODEVIEW_TYPELEAF_PROPS propattr;   /* property attribute field (prop_t) */
    cv_typ32_t type_index_field;        /* type index of LF_FIELD descriptor list */
    uchar_t variant[];                  /* variable length data specifying size in bytes and name */
} CODEVIEW_TYPELEAF_UNION32;

typedef struct _CODEVIEW_TYPELEAF_ENUM32 {
    ushort_t length;                    /* length of type info (part of alignment) */
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_ENUM */
    ushort_t nelems;                    /* number of elements in enum */
    CODEVIEW_TYPELEAF_PROPS propattr;   /* property attribute field (prop_t) */
    cv_typ32_t type_index_enum;         /* type index of element type */
    cv_typ32_t type_index_field;        /* type index of LF_FIELD descriptor list */
    uchar_t name[1];                    /* length-prefixed name of enum */
} CODEVIEW_TYPELEAF_ENUM32;

typedef struct _CODEVIEW_TYPELEAF_PROCEDURE32 {
    ushort_t length;                    /* length of type info (part of alignment) */
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_PROCEDURE */
    cv_typ32_t type_index_return;       /* type index of return value  */
    uchar_t call_type;                  /* calling convention (CV_call_t) */
    uchar_t reserved;                   /* reserved for future use */
    ushort_t nargs;                     /* number of parameters */
    cv_typ32_t type_index_arglist;      /* type index of argument list */
} CODEVIEW_TYPELEAF_PROCEDURE32;

#if 0
typedef struct _CODEVIEW_TYPELEAF_MFUNCTION {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_MFUNCTION */
    cv_typ_t type_index_return;         /* type index of return value (CV_typ_t) */
    cv_typ_t type_index_class;          /* type index of containing class (CV_typ_t) */
    cv_typ_t type_index_this;           /* type index of this pointer (model specific) (CV_typ_t) */
    uchar_t call_type;                  /* calling convention (CV_call_t) */
    uchar_t reserved;                   /* reserved for future use */
    ushort_t nargs;                     /* number of parameters */
    cv_typ_t type_index_arglist;        /* type index of argument list (CV_typ_t) */
    long this_adjust;                   /* this adjuster (long because pad required anyway ?!) */
} CODEVIEW_TYPELEAF_MFUNCTION;
#endif

#if 0
typedef struct _CODEVIEW_TYPELEAF_VTSHAPE {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_VTSHAPE */
    ushort_t nentries;                  /* number of entries in vfunctable */
    uchar_t desc[];                     /* 4 bit (CV_VTS_desc) descriptors */
} CODEVIEW_TYPELEAF_VTSHAPE;
#endif

#if 0
typedef struct _CODEVIEW_TYPELEAF_COBOL0 {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_COBOL0 */
    cv_typ_t type_index;                /* parent type record index (CV_typ_t) */
    uchar_t variant[];
} CODEVIEW_TYPELEAF_COBOL0;
#endif

#if 0
typedef struct _CODEVIEW_TYPELEAF_COBOL1 {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_COBOL1 */
    char variant[];
} CODEVIEW_TYPELEAF_COBOL1;
#endif

#if 0
typedef struct _CODEVIEW_TYPELEAF_BARRAY {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_BARRAY */
    cv_typ_t type_index;                /* type index of underlying type (CV_typ_t) */
} CODEVIEW_TYPELEAF_BARRAY;
#endif

#if 0
typedef struct _CODEVIEW_TYPELEAF_LABEL {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_LABEL */
    ushort_t mode;                      /* addressing mode of label */
} CODEVIEW_TYPELEAF_LABEL;
#endif

#if 0
typedef struct _CODEVIEW_TYPELEAF_DIMARRAY {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_DIMARRAY */
    cv_typ_t type_index_array;          /* underlying type of the array (CV_typ_t) */
    cv_typ_t dim_info;                  /* dimension information (CV_typ_t) */
    uchar_t name[1];                    /* length-prefixed name */
} CODEVIEW_TYPELEAF_DIMARRAY;
#endif

#if 0
typedef struct _CODEVIEW_TYPELEAF_VFTPATH {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_VFTPATH */
    ushort_t nbases;                    /* number of bases in path */
    cv_typ_t type_index[1];             /* bases from root to leaf (CV_typ_t) */
} CODEVIEW_TYPELEAF_VFTPATH;
#endif

#if 0
typedef struct _CODEVIEW_TYPELEAF_PRECOMP {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_PRECOMP */
    ushort_t start_index;               /* starting type index included */
    ushort_t ntypes;                    /* number of types in inclusion */
    long signature;                     /* yep */
    uchar_t name[];                     /* length-prefixed name of included type file */
} CODEVIEW_TYPELEAF_PRECOMP;
#endif

#if 0
typedef struct _CODEVIEW_TYPELEAF_ENDPRECOMP {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_ENDPRECOMP */
    long signature;                     /* Yep */
} CODEVIEW_TYPELEAF_ENDPRECOMP;
#endif

#if 0
typedef struct _CODEVIEW_TYPELEAF_OEM {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_OEM */
    ushort_t oem_ident;                 /* MS assigned OEM identified */
    ushort_t oem_rec_ident;             /* OEM assigned record type identifier */
    ushort_t nindices;                  /* number of type indices to follow */
    cv_typ_t type_index[];              /* array of type indices followed by OEM defined data (CV_typ_t) */
} CODEVIEW_TYPELEAF_OEM;
#endif

typedef struct _CODEVIEW_TYPELEAF_TYPESERVER32 {
    ushort_t length;                    /* length of type info (part of alignment) */
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_TYPESERVER */
    long timdat;                        /* time/date stamp (signature) */
    long age;                           /* age of database used by this module */
    uchar_t name[1];                    /* length-prefixed name of PDB-file */
} CODEVIEW_TYPELEAF_TYPESERVER32;

/* ( ) */

#if 0
typedef struct _CODEVIEW_TYPELEAF_SKIP {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_SKIP */
    cv_typ_t type_index;                /* next valid index (CV_typ_t) */
    uchar_t data[];                     /* pad data */
} CODEVIEW_TYPELEAF_SKIP;
#endif

/* argument list leaf */
typedef struct _CODEVIEW_TYPELEAF_ARGLIST32 {
    ushort_t length;                    /* length of type info (part of alignment) */
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_ARGLIST */
    uint_t nargs;                       /* number of arguments */
    cv_typ32_t args[];                  /* type indices of argument list */
} CODEVIEW_TYPELEAF_ARGLIST32;

/* derived class list leaf */
#if 0
typedef struct _CODEVIEW_TYPELEAF_DERIVED {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_DERIVED */
    ushort_t nargs;                     /* number of arguments */
    cv_typ_t derived_class[];           /* type indices of derived classes (CV_typ_t) */
} CODEVIEW_TYPELEAF_DERIVED;
#endif

/* default argument leaf */
#if 0
typedef struct _CODEVIEW_TYPELEAF_DEFARG {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_DEFARG */
    cv_typ_t type_index;                /* type of resulting expression (CV_typ_t) */
    uchar_t expression[1];              /* length-prefixed expression string */
} CODEVIEW_TYPELEAF_DEFARG;
#endif

/* header leaf for complex list of class/structure subfields */
typedef struct _CODEVIEW_TYPELEAF_FIELDLIST32 {
    ushort_t length;                    /* length of type info (part of alignment) */
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_FIELDLIST */
    uchar_t data[];                     /* field list sub lists */
} CODEVIEW_TYPELEAF_FIELDLIST32;

#if 0
typedef struct _CODEVIEW_TYPELEAF_METHODLIST {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_METHODLIST */
    uchar_t data[];                     /* method list */
} CODEVIEW_TYPELEAF_METHODLIST;
#endif

typedef struct _CODEVIEW_TYPELEAF_BITFIELD32 {
    ushort_t length;                    /* length of type info (part of alignment) */
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_BITFIELD */
    cv_typ32_t type_index;              /* type index of bitfield */
    uchar_t nbits;                      /* length in bits */
    uchar_t position;                   /* starting position (from bit 0) */
} CODEVIEW_TYPELEAF_BITFIELD32;

/* dimensioned array with constant bounds */
#if 0
typedef struct _CODEVIEW_TYPELEAF_DIMCON {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_DIMCONU/CODEVIEW_LEAFTYPE_DIMCONLU */
    ushort_t ndims;                     /* number of dimensions */
    cv_typ_t type_index;                /* type of index (CV_typ_t) */
    uchar_t dim[];                      /* array of dimension information with either
                                           upper bounds or lower/upper bound */
} CODEVIEW_TYPELEAF_DIMCONU,
  CODEVIEW_TYPELEAF_DIMCONLU;
#endif

/* dimensioned array with variable bounds */
#if 0
typedef struct _CODEVIEW_TYPELEAF_DIMVAR {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_DIMVARU/CODEVIEW_LEAFTYPE_DIMVARLU */
    ushort_t ndims;                     /* number of dimensions */
    cv_typ_t type_index;                /* type of index (CV_typ_t) */
    uchar_t dim[];                      /* array of type indices for either variable upper bound or variable */
                                        /* lower/upper bound. The referenced types must be LF_REFSYM or T_VOID */
} CODEVIEW_TYPELEAF_DIMVARU,
  CODEVIEW_TYPELEAF_DIMVARLU;
#endif

#if 0
typedef struct _CODEVIEW_TYPELEAF_REFSYM {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_REFSYM */
    uchar_t data[];                     /* copy of referenced symbol record (including length) */
} CODEVIEW_TYPELEAF_REFSYM;
#endif

/* ( ) */

typedef struct _CODEVIEW_TYPELEAF_CHAR {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_CHAR */
    char val;                           /* signed 8-bit value */
} CODEVIEW_TYPELEAF_CHAR;

typedef struct _CODEVIEW_TYPELEAF_SHORT {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_SHORT */
    short val;                          /* signed 16-bit value */
} CODEVIEW_TYPELEAF_SHORT;

typedef struct _CODEVIEW_TYPELEAF_LONG {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_SHORT */
    long val;                           /* signed 32-bit value */
} CODEVIEW_TYPELEAF_LONG;

typedef struct _CODEVIEW_TYPELEAF_REAL32 {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_REAL32 */
    float val;                          /* 32-bit real value */
} CODEVIEW_TYPELEAF_REAL32, *PCODEVIEW_TYPELEAF_REAL32;

typedef struct _CODEVIEW_TYPELEAF_REAL48 {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_REAL48 */
    char val[6];                        /* 48-bit real value */
} CODEVIEW_TYPELEAF_REAL48;

typedef struct _CODEVIEW_TYPELEAF_REAL64 {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_REAL64 */
    double val;                         /* 64-bit real value */
} CODEVIEW_TYPELEAF_REAL64, *PCODEVIEW_TYPELEAF_REAL64;

typedef struct _CODEVIEW_TYPELEAF_REAL128 {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_REAL128 */
    char val[16];                       /* 128-bit real value */
} CODEVIEW_TYPELEAF_REAL128;

typedef struct _CODEVIEW_TYPELEAF_COMPLEX32 {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_COMPLEX32 */
    float real_val;                     /* real component */
    float imag_val;                     /* imaginary component */
} CODEVIEW_TYPELEAF_COMPLEX32;

typedef struct _CODEVIEW_TYPELEAF_COMPLEX64 {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_COMPLEX64 */
    double real_val;                    /* real component */
    double imag_val;                    /* imaginary component */
} CODEVIEW_TYPELEAF_COMPLEX64;

typedef struct _CODEVIEW_TYPELEAF_COMPLEX128 {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_COMPLEX128 */
    char real_val[16];                  /* real component */
    char imag_val[16];                  /* imaginary component */
} CODEVIEW_TYPELEAF_COMPLEX128;

typedef struct _CODEVIEW_TYPELEAF_VARSTRING {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_VARSTRING */
    ushort_t length;                    /* length of value in bytes */
    char value[];                       /* value */
} CODEVIEW_TYPELEAF_VARSTRING;

/* ( ) */

typedef struct _CODEVIEW_TYPELEAF_INDEX {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_INDEX */
    cv_typ_t type_index;                /* type index of referenced leaf (CV_typ_t) */
} CODEVIEW_TYPELEAF_INDEX;

typedef struct _CODEVIEW_TYPELEAF_BCLASS32 {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_BCLASS */
    CODEVIEW_TYPELEAF_FLDATTR fldattr;  /* attributes */
    cv_typ32_t type_index;              /* type index of base class */
    uchar_t offset[];                   /* variable length offset of base within class */
} CODEVIEW_TYPELEAF_BCLASS32;

typedef struct _CODEVIEW_TYPELEAF_VBCLASS32 {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_VBCLASS/CODEVIEW_LEAFTYPE_IVBCLASS */
    CODEVIEW_TYPELEAF_FLDATTR fldattr;  /* attributes */
    cv_typ32_t type_index;              /* type index of virtual base class */
    cv_typ32_t type_index_bptr;         /* type index of virtual base pointer */
    uchar_t offset[];                   /* virtual base pointer offset from address point */
                                        /* followed by virtual base offset from vbtable */
} CODEVIEW_TYPELEAF_VBCLASS32,
  CODEVIEW_TYPELEAF_IVBCLASS32;

#ifdef OLDTYPES
typedef struct _CODEVIEW_TYPELEAF_FRIENDCLS {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_FRIENDCLS */
    cv_typ_t type_index;                /* index to type record of friend class (CV_typ_t) */
} CODEVIEW_TYPELEAF_FRIENDCLS;
#endif

#ifdef OLDTYPES
typedef struct _CODEVIEW_TYPELEAF_FRIENDFCN {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_FRIENDCLS */
    cv_typ_t type_index;                /* index to type record of friend function (CV_typ_t) */
    uchar_t name[1];                    /* name of friend function */
} CODEVIEW_TYPELEAF_FRIENDFCN;
#endif

typedef struct _CODEVIEW_TYPELEAF_MEMBER32 {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_MEMBER */
    CODEVIEW_TYPELEAF_FLDATTR fldattr;  /* attributes */
    cv_typ32_t type_index;              /* index of type record for field */
    uchar_t variant[];                  /* variable length offset of field followed
                                           by length prefixed name of field */
} CODEVIEW_TYPELEAF_MEMBER32;

#ifdef OLDTYPES
typedef struct _CODEVIEW_TYPELEAF_STMEMBER {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_STMEMBER */
    cv_typ_t type_index;                /* index of type record for field (CV_typ_t) */
    CODEVIEW_TYPELEAF_FLDATTR fldattr;  /* attributes */
    uchar_t name[1];                    /* length-prefixed name of field */
} CODEVIEW_TYPELEAF_STMEMBER;
#endif

#ifdef OLDTYPES
typedef struct _CODEVIEW_TYPELEAF_VFUNCTAB {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_VFUNCTAB */
    cv_typ_t type_index;                /* type index of pointer (CV_typ_t) */
} CODEVIEW_TYPELEAF_VFUNCTAB;
#endif

#ifdef OLDTYPES
typedef struct _CODEVIEW_TYPELEAF_VFUNCOFF {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_VFUNCOFF */
    cv_typ_t type_index;                /* type index of pointer (CV_typ_t) */
    long offset;                        /* offset of virtual function table pointer */
} CODEVIEW_TYPELEAF_VFUNCOFF;
#endif

#ifdef OLDTYPES
typedef struct _CODEVIEW_TYPELEAF_METHOD {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_METHOD */
    ushort_t noccurances;               /* number of occurances of function */
    cv_typ_t type_index;                /* index to LF_METHODLIST record (CV_typ_t) */
    uchar_t name[1];                    /* length-prefixed name of method */
} CODEVIEW_TYPELEAF_METHOD;
#endif

#ifdef OLDTYPES
typedef struct _CODEVIEW_TYPELEAF_ONEMETHOD {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_ONEMETHOD */
    CODEVIEW_TYPELEAF_FLDATTR fldattr;  /* attributes */
    cv_typ_t type_index;                /* index to type record for procedure (CV_typ_t) */
    long vbaseoff[];                    /* offset in vfunctable if intro virtual followed
                                           by length prefixed name of method */
} CODEVIEW_TYPELEAF_ONEMETHOD;
#endif

typedef struct _CODEVIEW_TYPELEAF_ENUMERATE {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_ENUMERATE */
    CODEVIEW_TYPELEAF_FLDATTR fldattr;  /* attributes */
    uchar_t variant[];                  /* variable length value field followed by length prefixed name */
} CODEVIEW_TYPELEAF_ENUMERATE;

#ifdef OLDTYPES
typedef struct _CODEVIEW_TYPELEAF_NESTTYPE {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_NESTTYPE */
    cv_typ_t type_index;                /* index of nested type definition (CV_typ_t) */
    uchar_t name[1];                    /* length-prefixed type name */
} CODEVIEW_TYPELEAF_NESTTYPE;
#endif

typedef struct _CODEVIEW_TYPELEAF_PAD {
    ushort_t leaf_type;                 /* leaf type: CODEVIEW_LEAFTYPE_PAD? */
} CODEVIEW_TYPELEAF_PAD;

/*
 * Global types subsection format.
 *
 * This structure immediately preceeds the global types table.
 * The offsets in the offset array are relative to the address
 * of ntypes. Each type entry following the offset array must
 * begin on a long word boundary.
 */
typedef struct _CODEVIEW_TYPE_FLAGS {
    ulong_t sig: 8;
    ulong_t unused: 24;
} CODEVIEW_TYPE_FLAGS;

typedef struct _CODEVIEW_GLOBAL_TYPES {
    CODEVIEW_TYPE_FLAGS flags;          /* flags */
    long ntypes;                        /* number of types */
    long offset[];                      /* array of offsets to types */
} CODEVIEW_GLOBAL_TYPES;

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _CODEVIEW_H */
