/****************************************************************************
 *                                                                          *
 * File    : link.h                                                         *
 *                                                                          *
 * Purpose : Win32 Linker; constants and definitions.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           98-07-01  UNREFERENCED_PARAMETER macro added.                  *
 *           98-11-25  Added new global variable: module_list.              *
 *           98-11-25  Added function my_newprocess().                      *
 *           98-12-05  Added global variables to support debug info.        *
 *           99-01-22  Added align_dword_ptr() & align_word_ptr().          *
 *           99-01-29  Added function read_respfile().                      *
 *           99-02-10  Added delayed import support.                        *
 *           99-02-13  Added address_of_section() & size_of_section().      *
 *           00-11-16  Added machine classification (ARM).                  *
 *           01-05-13  Added new global variables: stub_file, stub_size.    *
 *           01-09-25  Added function lookup_section_attributes().          *
 *           01-09-25  Added new global variable: attrib_list.              *
 *           03-06-11  Removed function my_newprocess() with CVPACK.        *
 *           03-06-16  Added function lookup_libpath().                     *
 *           03-06-16  Added new global variable: lib_path_list.            *
 *           03-09-14  Made discard_segment() public.                       *
 *                                                                          *
 ****************************************************************************/

#ifndef _LINK_H
#define _LINK_H

#ifdef __cplusplus
extern "C" {
#endif

#include "msg.h"

/* Irritating warning in level 4: */
/* warning C4244: '=' : conversion from 'xxx ' to 'yyy ', possible loss of data */
#pragma warning(disable: 4244)

//#define UNREFERENCED_PARAMETER(P)   (P)

#include "defs.h"

/* Matches GetLastError() type */
typedef DWORD WINERR;

/* Function in IMAGEHLP.DLL; not in Windows 95 */
#ifndef CHECKSUMMAPPEDFILE
typedef IMAGE_NT_HEADERS *(WINAPI *CHECKSUMMAPPEDFILE)(void *, ulong_t, long *, long *);
#endif

#define SWAP(type,a,b)  { type t; t = (a); (a) = (b); (b) = t; }
#define NELEMS(arr)     (sizeof(arr) / sizeof(arr[0]))

#define ROUNDUP(n,m)      ((((n) + (m) - 1) / (m)) * (m))
#define ROUNDUP_DWORD(n)  ROUNDUP((n), sizeof(long))
#define ROUNDUP_WORD(n)   ROUNDUP((n), sizeof(short))

/*
 * For some reason does ReBaseImage(),
 * in IMAGEHLP.DLL, hate linker version 2.0.
 */
#define MAJOR_LINKER_VERSION  2     /* remember LIB.H in POLIB */
#define MINOR_LINKER_VERSION  50    /* remember LIB.H in POLIB */

/* Display name for this program */
#define PROGRAM_NAME  "xLINK"

#define EXT_OBJ  ".OBJ"
#define EXT_LIB  ".LIB"
#define EXT_MAP  ".MAP"
#define EXT_EXP  ".EXP"
#define EXT_EXE  ".EXE"
#define EXT_DLL  ".DLL"

/* Fixed values from the Microsoft linker */
#define INTEL_PAGE_SIZE         0x1000
#define FILE_ALIGNMENT_WIN32    0x200       /* 512 bytes */
#define FILE_ALIGNMENT_WIN98    0x1000      /* 4 KB */
#define IMAGE_BASE_ALIGNMENT    0x10000     /* 64 KB */

/* Default values from the Microsoft linker */
#define DEFAULT_STACK_RESERVE   0x100000
#define DEFAULT_STACK_COMMIT    0x1000
#define DEFAULT_HEAP_RESERVE    DEFAULT_STACK_RESERVE
#define DEFAULT_HEAP_COMMIT     DEFAULT_STACK_COMMIT
#define DEFAULT_ALIGNMENT       0x1000
#define DEFAULT_IMAGE_BASE_EXE  0x00400000
#define DEFAULT_IMAGE_BASE_DLL  0x10000000
#define DEFAULT_IMAGE_BASE_EXE_WINCE  0x00010000
#define DEFAULT_IMAGE_BASE_DLL_WINCE  0x00100000

/* Section constants */
#define COFF_STYP_CONTENTS  (COFF_STYP_TEXT|COFF_STYP_DATA|COFF_STYP_BSS)
#define COFF_STYP_MEMORY    (COFF_STYP_READ|COFF_STYP_WRITE|COFF_STYP_EXEC| \
                             COFF_STYP_SHARE|COFF_STYP_NOPAGE|COFF_STYP_NOCACHE|COFF_STYP_DISCARD)

enum machine_type
{
    MACHINE_UNKNOWN = 0,
    MACHINE_X86,
    MACHINE_ARM
};

struct options
{
    enum machine_type machine;          /* machine type */
    bool_t ignore_libraries;            /* ignore default libraries? */
    bool_t verbose;                     /* verbose output? */
    bool_t DLL;                         /* create a DLL? */
    bool_t mapfile;                     /* create a MAP file? */
    bool_t mapfile_fixups;              /* output fixups to MAP file? */
    bool_t mapfile_exports;             /* output exports to MAP file? */
    bool_t mapfile_lines;               /* output lines to MAP file? */
    bool_t debug;                       /* debug image? */
    bool_t debug_coff;                  /* generate COFF debug info? */
    bool_t debug_cv;                    /* generate CodeView debug info? */
    bool_t swaprun_net;                 /* run image from swap file? */
    bool_t swaprun_cd;                  /* run image from swap file? */
    bool_t aggressive_ws_trim;          /* trim working set aggressively? */
    bool_t fixed;                       /* fixed location; i.e no relocs? */
    bool_t no_entry;                    /* no entry point? */
    bool_t old_implib;                  /* create implib in 1.0 format? */
    bool_t delay_bind;                  /* create delay load BIAT? */
    bool_t delay_unload;                /* create delay load UIAT? */
    bool_t discard_unreferenced;        /* discard unreferenced symbols? */
    bool_t force_multiple;              /* force multiple symbols */
    bool_t unmangle_names;              /* unmangle C++ names? */
    bool_t large_address_aware;         /* support for addr above 2 GB? */
    bool_t terminal_server_aware;       /* support for Terminal Server? */
    bool_t no_export_object;            /* don't generate export object, even if exports exists! */
    bool_t no_import_archive;           /* don't generate import library, even if exports exists! */
    long stack_reserve;                 /* size to reserve for stack */
    long stack_commit;                  /* size to commit for stack */
    long heap_reserve;                  /* size to reserve for heap */
    long heap_commit;                   /* size to commit for heap */
    long section_alignment;             /* section alignment */
    long file_alignment;                /* file alignment */
    addr_t image_base;                  /* image base address */
    ushort_t major_os_version;          /* major OS version */
    ushort_t minor_os_version;          /* minor OS version */
    ushort_t major_subsystem_version;   /* major subsystem version */
    ushort_t minor_subsystem_version;   /* minor subsystem version */
    ushort_t subsystem;                 /* subsystem */
};

/* Error definitions */
#define ERROR_SEVERITY_FATAL    0x8000U

#define RCFATAL(err)    (ERROR_SEVERITY_FATAL|(err))
#define RCERROR(err)    (ERROR_SEVERITY_ERROR|(err))
#define RCWARNING(err)  (ERROR_SEVERITY_WARNING|(err))

#define ISFATAL(err)    (((err) & ERROR_SEVERITY_FATAL) ? 1 : 0)
#define ISERROR(err)    (((err) & 0xC0000000) == ERROR_SEVERITY_ERROR)
#define ISWARNING(err)  (((err) & 0xC0000000) == ERROR_SEVERITY_WARNING)

/* Private WINERR error codes */
#define ERROR_OPEN_FILE             (APPLICATION_ERROR_MASK|MSG_FILE_NOT_FOUND)
#define ERROR_INVALID_ARCHIVE       (APPLICATION_ERROR_MASK|MSG_INVALID_ARCHIVE)
#define ERROR_BAD_ARCHIVE_DIRECTORY (APPLICATION_ERROR_MASK|MSG_BAD_ARCHIVE_DIRECTORY)
#define ERROR_NO_LONGNAMES_MEMBER   (APPLICATION_ERROR_MASK|MSG_NO_LONGNAMES_MEMBER)
#define ERROR_NO_OBJECT_FILES       (APPLICATION_ERROR_MASK|MSG_NO_OBJECT_FILES)
#define ERROR_NO_OUTPUT_FILE        (APPLICATION_ERROR_MASK|MSG_NO_OUTPUT_FILE)
#define ERROR_INVALID_OBJECT        (APPLICATION_ERROR_MASK|MSG_INVALID_OBJECT)
#define ERROR_INVALID_MACHINE       (APPLICATION_ERROR_MASK|MSG_INVALID_MACHINE)
#define ERROR_SECTION_WITHOUT_TYPE  (APPLICATION_ERROR_MASK|MSG_SECTION_WITHOUT_TYPE)
#define ERROR_SECTION_ATTR_DIFF     (APPLICATION_ERROR_MASK|MSG_SECTION_ATTR_DIFF)
#define ERROR_MULTIPLE_COMDAT       (APPLICATION_ERROR_MASK|MSG_MULTIPLE_COMDAT)
#define ERROR_UNKNOWN_COMDAT_TYPE   (APPLICATION_ERROR_MASK|MSG_UNKNOWN_COMDAT_TYPE)
#define ERROR_UNRESOLVED_SYMBOL     (APPLICATION_ERROR_MASK|MSG_UNRESOLVED_SYMBOL)
#define ERROR_UNRESOLVED_COUNT      (APPLICATION_ERROR_MASK|MSG_UNRESOLVED_COUNT)
#define ERROR_MULTIPLE_SYMBOLS      (APPLICATION_ERROR_MASK|MSG_MULTIPLE_SYMBOLS)
#define ERROR_UNKNOWN_WEAK_TYPE     (APPLICATION_ERROR_MASK|MSG_UNKNOWN_WEAK_TYPE)
#define ERROR_DUPLICATE_ORDINAL     (APPLICATION_ERROR_MASK|MSG_DUPLICATE_ORDINAL)
#define ERROR_NO_ENTRY_POINT        (APPLICATION_ERROR_MASK|MSG_NO_ENTRY_POINT)
#define ERROR_ALIGN_TOO_SMALL       (APPLICATION_ERROR_MASK|MSG_ALIGN_TOO_SMALL)
#define ERROR_ALIGN_OPTION          (APPLICATION_ERROR_MASK|MSG_BAD_ALIGN_OPTION)
#define ERROR_BASE_OPTION           (APPLICATION_ERROR_MASK|MSG_BAD_BASE_OPTION)
#define ERROR_DEBUGTYPE_OPTION      (APPLICATION_ERROR_MASK|MSG_BAD_DEBUGTYPE_OPTION)
#define ERROR_DELAY_OPTION          (APPLICATION_ERROR_MASK|MSG_BAD_DELAY_OPTION)
#define ERROR_EXPORT_OPTION         (APPLICATION_ERROR_MASK|MSG_BAD_EXPORT_OPTION)
#define ERROR_FIXED_OPTION          (APPLICATION_ERROR_MASK|MSG_BAD_FIXED_OPTION)
#define ERROR_FORCE_OPTION          (APPLICATION_ERROR_MASK|MSG_BAD_FORCE_OPTION)
#define ERROR_HEAP_OPTION           (APPLICATION_ERROR_MASK|MSG_BAD_HEAP_OPTION)
#define ERROR_LARGEADDRESSAWARE_OPTION (APPLICATION_ERROR_MASK|MSG_BAD_LARGEADDRESSAWARE_OPTION)
#define ERROR_MACHINE_OPTION        (APPLICATION_ERROR_MASK|MSG_BAD_MACHINE_OPTION)
#define ERROR_MAPINFO_OPTION        (APPLICATION_ERROR_MASK|MSG_BAD_MAPINFO_OPTION)
#define ERROR_MERGE_OPTION          (APPLICATION_ERROR_MASK|MSG_BAD_MERGE_OPTION)
#define ERROR_OPT_OPTION            (APPLICATION_ERROR_MASK|MSG_BAD_OPT_OPTION)
#define ERROR_SECTION_OPTION        (APPLICATION_ERROR_MASK|MSG_BAD_SECTION_OPTION)
#define ERROR_STACK_OPTION          (APPLICATION_ERROR_MASK|MSG_BAD_STACK_OPTION)
#define ERROR_SUBSYSTEM_OPTION      (APPLICATION_ERROR_MASK|MSG_BAD_SUBSYSTEM_OPTION)
#define ERROR_SWAPRUN_OPTION        (APPLICATION_ERROR_MASK|MSG_BAD_SWAPRUN_OPTION)
#define ERROR_TSAWARE_OPTION        (APPLICATION_ERROR_MASK|MSG_BAD_TSAWARE_OPTION)
#define ERROR_VERSION_OPTION        (APPLICATION_ERROR_MASK|MSG_BAD_VERSION_OPTION)
#define ERROR_WS_OPTION             (APPLICATION_ERROR_MASK|MSG_BAD_WS_OPTION)
#define ERROR_CANT_READ_CMDFILE     (APPLICATION_ERROR_MASK|MSG_CANT_READ_CMDFILE)
#define ERROR_OPTION_ARG_MISSING    (APPLICATION_ERROR_MASK|MSG_OPTION_ARG_MISSING)
#define ERROR_UNKNOWN_OPTION        (APPLICATION_ERROR_MASK|MSG_UNKNOWN_OPTION)
#define ERROR_UNKNOWN_RELOC_MACHINE (APPLICATION_ERROR_MASK|MSG_UNKNOWN_RELOC_MACHINE)
#define ERROR_UNKNOWN_RELOC_TYPE    (APPLICATION_ERROR_MASK|MSG_UNKNOWN_RELOC_TYPE)
#define ERROR_BAD_RELOC_SYMCLASS    (APPLICATION_ERROR_MASK|MSG_BAD_RELOC_SYMCLASS)
#define ERROR_RELOC_IN_BSS_SECTION  (APPLICATION_ERROR_MASK|MSG_RELOC_IN_BSS_SECTION)
#define ERROR_ONLY_ONE_RESFILE      (APPLICATION_ERROR_MASK|MSG_ONLY_ONE_RESFILE)
#define ERROR_EMPTY_RESFILE         (APPLICATION_ERROR_MASK|MSG_EMPTY_RESFILE)
#define ERROR_CANT_DO_CHECKSUM      (APPLICATION_ERROR_MASK|MSG_CANT_DO_CHECKSUM)
#define ERROR_UNKNOWN_DELAY_MODULE  (APPLICATION_ERROR_MASK|MSG_UNKNOWN_DELAY_MODULE)
#define ERROR_IGNORED_DELAY_MODULE  (APPLICATION_ERROR_MASK|MSG_IGNORED_DELAY_MODULE)
#define ERROR_CANT_DELAY_DATA       (APPLICATION_ERROR_MASK|MSG_CANT_DELAY_DATA)
#define ERROR_CANT_DELAY_ARM        (APPLICATION_ERROR_MASK|MSG_CANT_DELAY_ARM)
#define ERROR_INVALID_STUB_FILE     (APPLICATION_ERROR_MASK|MSG_INVALID_STUB_FILE)
#define ERROR_WEIRD_STUB_FILE       (APPLICATION_ERROR_MASK|MSG_WEIRD_STUB_FILE)
#define ERROR_UNKNOWN_ATTRIB_SECTION (APPLICATION_ERROR_MASK|MSG_UNKNOWN_ATTRIB_SECTION)
#define ERROR_INTERNAL              (APPLICATION_ERROR_MASK|MSG_INTERNAL_ERROR)

#define MYOPENERROR(err) \
    ((((err) & APPLICATION_ERROR_MASK) == 0 && \
     (((err) & 0x00007FFF) == ERROR_FILE_NOT_FOUND || \
      ((err) & 0x00007FFF) == ERROR_PATH_NOT_FOUND)) ? \
     (((err) & 0xFFFF8000) | ERROR_OPEN_FILE) : (err))

/* main.c */
void link_args(int, char **, bool_t);
void apperror(WINERR, ...);
void printmsg(int, ...);
void errorexit(int);

/* archive.c */
void process_archives(void);

/* object.c */
void process_object_modules(void);
void process_object_module(const char *, FILEINFO *, const void *);
void post_process_common_symbols(void);

/* resource.c */
bool_t is_resource_file(FILEINFO *);
void process_resource_file(FILEINFO *);

/* debug.c */
void alloc_debug_directory(void);
void *write_debug_information(void *, void *, COFF_DBGDIR *, COFF_FILHDR *);

/* export.c */
void process_exports(void);
void build_new_import_object(void *, COFF_NEWIMP *, const char *, const char *);
void symbol_from_export_name(char *, const char *, ushort_t);

/* order.c */
void order_sections(void);
void discard_segment(SEGENTRY *);

/* fixup.c */
void fixup_relocations(void);

/* dictary.c */
SCNENTRY *lookup_section(const char *, ulong_t);
SEGENTRY *add_segment_to_section(SCNENTRY *);
SEGENTRY *add_segment_to_import_section(SCNENTRY *, FILEINFO *);
RELENTRY *add_relocation_to_segment(SEGENTRY *);
GRPENTRY *lookup_group(const char *, ulong_t);
void add_section_to_group(GRPENTRY *, SCNENTRY *);
RENENTRY *lookup_section_alias(const char *, const char *);
ATTENTRY *lookup_section_attributes(const char *);
EXPENTRY *lookup_export(const char *, const char *);
SYMENTRY *lookup_public(const char *);
void dump_public_table(void);
SYMENTRY *lookup_static(SEGENTRY *, const char *);
void add_public_to_segment(SEGENTRY *, SYMENTRY *);
void rename_weak_symbol(SYMENTRY *);
void replace_public_node(SYMENTRY *, SYMENTRY *);
FILEINFO *lookup_file(FILEINFO **, const char *);
void remove_file(FILEINFO **, FILEINFO *);
DLLENTRY *lookup_delay_import_module(const char *, bool_t);
LIBENTRY *lookup_libpath(const char *);
HASHNODE *lookup_name(const char *);
MODENTRY *add_module_to_list(void);
const char *unmangle_name(const char *);

/* delayimp.c */
void build_delay_import_module_object(void *, const char *);
void build_delay_import_null_thunk_object(void *);
void build_delay_import_object(void *, const char *, const char *, const char *);

/* execfile.c */
void write_executable_image(void);

/* map.c */
void write_map_image(void);

/* utils.c */
void *my_alloc(size_t);
void *my_realloc(void *, size_t);
void my_free(void *);
size_t my_size(void *);
WINERR my_openmap(FILEINFO *);
WINERR my_createmap(FILEINFO *, ulong_t);
WINERR my_closemap(FILEINFO *, bool_t);
bool_t calculate_executable_checksum(FILEINFO *, long *);
WINERR my_deletefile(const char *);
WINERR my_renamefile(const char *, const char *);
char *read_respfile(const char *);
void tokenize(char *, int *, char ***);
const char *basename(const char *);
void add_extension_to_file(char *, const char *);
void update_extension_in_file(char *, const char *);
int my_exception_filter(DWORD, WINERR *);

/* Globals */
extern struct options options;
extern int nerrs;
extern FILEINFO *obj_file_list;
extern FILEINFO *lib_file_list;
extern FILEINFO *exe_file;
extern FILEINFO *map_file;
extern FILEINFO *exp_file;
extern FILEINFO *lib_file;
extern FILEINFO *stub_file;
extern long stub_size;
extern LIBENTRY *lib_path_list;
extern MODENTRY *module_list;
extern SYMENTRY *public_list;
extern SCNENTRY *section_list;
extern ATTENTRY *attrib_list;
extern GRPENTRY *group_list;
extern RENENTRY *alias_list;
extern DLLENTRY *delay_list;
extern char *entry_point;
extern SYMENTRY *pub_entry;
extern size_t unresolved_count;
extern SEGENTRY *seg_dbgdir;
extern SCNENTRY *scn_sym;
extern SCNENTRY *scn_type;
extern SCNENTRY *scn_fpo;
extern time_t time_stamp;
extern EXPENTRY **export_list;
extern size_t export_count;
extern size_t export_maxcount;

__inline addr_t address_of_section(SCNENTRY *scn)
{
    return (scn->segs != NULL) ? scn->segs->vaddr : 0;
}

__inline long size_of_section(SCNENTRY *scn)
{
    long size = 0;

    if (scn->segs != NULL)
    {
        SEGENTRY *seg;

        seg = scn->segs;
        size = seg->size;

        while ((seg = seg->next) != NULL)
            size += seg->alignment + seg->size;
    }

    return size;
}

__inline void *align_dword_ptr(void *ip)
{
    while ((size_t)ip & 3)
    {
        *(char *)ip = 0;
        ip = (char *)ip + 1;
    }
    return ip;
}

__inline void *align_word_ptr(void *ip)
{
    if ((size_t)ip & 1)
    {
        *(char *)ip = 0;
        ip = (char *)ip + 1;
    }
    return ip;
}

#pragma warning(disable:4035)  /* no return statement */
__inline long read_big_endian(long *p)
{
    _asm mov eax,[p];
    _asm mov eax,[eax];
    _asm bswap eax;
    /* return value in eax */
}
#pragma warning(default:4035)

__inline void write_big_endian(long *p, long n)
{
    _asm mov eax,[n];
    _asm mov edx,[p];
    _asm bswap eax;
    _asm mov [edx],eax;
}

#ifdef PODEBUG
__inline void *my_alloc(size_t size)
{
    void *vp = malloc(size);

    if (vp == NULL)
        apperror(RCFATAL(ERROR_OUTOFMEMORY));

    /* Initialize to "known garbage" */
    memset(vp, 0xDD, size);

    return vp;
}

__inline void *my_realloc(void *vp, size_t newsize)
{
    vp = realloc(vp, newsize);

    if (vp == NULL)
        apperror(RCFATAL(ERROR_OUTOFMEMORY));

    return vp;
}

__inline void my_free(void *vp)
{
    if (vp) free(vp);
}
#endif /* PODEBUG */

#ifdef PRERELEASE
__inline void dump_groups(void)
{
    GRPENTRY *grp;

    for (grp = group_list; grp != NULL; grp = grp->next)
    {
        SCNENTRY *scn;

        printf("%s: [%x]\n", grp->name, grp->flags);

        for (scn = grp->scngrp; scn != NULL; scn = scn->scngrp)
            printf("\t%s\n", scn->name);
    }
}
#endif /* PRERELEASE */

#ifdef PRERELEASE
extern size_t public_count;
extern size_t static_count;
extern size_t cur_size;
extern size_t tot_size;
#endif /* PRERELEASE */

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _LINK_H */
