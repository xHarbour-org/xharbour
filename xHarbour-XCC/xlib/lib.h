/****************************************************************************
 *                                                                          *
 * File    : lib.h                                                          *
 *                                                                          *
 * Purpose : Win32 Library Manager; constants and definitions.              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           97-11-17  Created                                              *
 *           98-01-07  ERROR_UNKNOWN_OPTION added.                          *
 *           98-07-01  UNREFERENCED_PARAMETER macro added.                  *
 *           98-08-05  Removed unused(!) member 'next' from SYMENTRY.       *
 *           98-11-07  ERROR_CANT_CONVERT added.                            *
 *           99-01-29  Added function read_respfile().                      *
 *           99-05-29  Added function my_exception_filter().                *
 *           04-01-12  Added EXPENTRY, export_list, export_count.           *
 *           04-01-12  Added machine classification (ARM).                  *
 *                                                                          *
 ****************************************************************************/

#ifndef _LIB_H
#define _LIB_H

#ifdef __cplusplus
extern "C" {
#endif

// Why different ???
#ifdef __POCC__
#include "msg.h"
#else
#include "msg.h"
#endif

/* Irritating warning in level 4: */
/* warning C4244: '=' : conversion from 'xxx ' to 'yyy ', possible loss of data */
#pragma warning(disable: 4244)

typedef struct _SYMENTRY {
    long offset;                /* archive member offset */
    uint_t defined: 1;          /* is the symbol already defined? */
    char name[1];               /* symbol name */
} SYMENTRY;

typedef struct _EXPENTRY {
    int ordinal;                /* ordinal number */
    uint_t defined: 1;          /* is the symbol defined? */
    uint_t is_func: 1;          /* is the symbol a function? */
    char name[1];               /* symbol name */
} EXPENTRY;

typedef struct _NAMENTRY {
    struct _NAMENTRY *next;     /* next node */
    char name[1];               /* member name */
} NAMENTRY;

/* Generic file description (archive; object etc) */
typedef struct _FILEINFO {
    struct _FILEINFO *next;     /* next node */
    HANDLE hf;                  /* file handle */
    HANDLE hmap;                /* file mapping handle */
    void *base;                 /* file mapping base */
    time_t time;                /* file time */
    /*_fsize_t*/ unsigned long size;              /* file size */
    char name[1];               /* file name */
} FILEINFO;

#define tstrcpy(s)      (strlen(s) ? strcpy(my_alloc((strlen(s)+1) * sizeof(char)), (s)) : NULL)
#define tstrncpy(s,n)   (strncpy(my_alloc(((n)+1) * sizeof(char)), (s), (n)))
#define tstrcat(s,p)    (strcat(strcpy(my_alloc(strlen(s) + strlen(p) + 1), (s)), (p)))

#define file_offset(file,p) \
    ((char *)(p) - (char *)(file)->base)

#define end_of_file(file,p) \
    ((char *)(p) >= ((char *)(file)->base + (file)->size))

// Ron Pinkas commented #define UNREFERENCED_PARAMETER(P)   (P)

/* Matches GetLastError() type */
typedef DWORD WINERR;

#define SWAP(type,a,b)  { type t; t = (a); (a) = (b); (b) = t; }
#define NELEMS(arr)     (sizeof(arr) / sizeof(arr[0]))

/* Import archive header */
#define MAJOR_LINKER_VERSION    2   /* see ..\link32\link.h */
#define MINOR_LINKER_VERSION    50  /* see ..\link32\link.h */
#define MAJOR_OS_VERSION        4
#define MINOR_OS_VERSION        0
#define FILE_ALIGNMENT          0x200
#define SECTION_ALIGNMENT       0x1000
#define DEFAULT_STACK_RESERVE   0x100000
#define DEFAULT_STACK_COMMIT    0x1000
#define DEFAULT_HEAP_RESERVE    DEFAULT_STACK_RESERVE
#define DEFAULT_HEAP_COMMIT     DEFAULT_STACK_COMMIT

/* Display name for this program */
#ifdef XHARBOUR
#define PROGRAM_NAME  "xLIB"
#else
#define PROGRAM_NAME  "POLIB"
#endif /* XHARBOUR */

#define EXT_OBJ  ".OBJ"
#define EXT_LIB  ".LIB"
#define EXT_EXE  ".EXE"
#define EXT_DLL  ".DLL"

enum machine_type {
    MACHINE_UNKNOWN = 0,
    MACHINE_X86,
    MACHINE_ARM
};

struct options {
    enum machine_type machine;          /* machine type */
    bool_t verbose;                     /* verbose output? */
    bool_t list;                        /* list archive members? */
    bool_t explode;                     /* extract all members? */
    bool_t extract;                     /* extract given member(s)? */
    bool_t create;                      /* create archive? */
    bool_t old_implib;                  /* create implib in 1.0 format? */
    bool_t old_names;
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
#define ERROR_WRITE_FILE            (APPLICATION_ERROR_MASK|MSG_WRITE_ERROR)
#define ERROR_MACHINE_OPTION        (APPLICATION_ERROR_MASK|MSG_BAD_MACHINE_OPTION)
#define ERROR_CANT_READ_CMDFILE     (APPLICATION_ERROR_MASK|MSG_CANT_READ_CMDFILE)
#define ERROR_OPTION_ARG_MISSING    (APPLICATION_ERROR_MASK|MSG_OPTION_ARG_MISSING)
#define ERROR_UNKNOWN_OPTION        (APPLICATION_ERROR_MASK|MSG_UNKNOWN_OPTION)
#define ERROR_BAD_DEFFILE_KEYWORD   (APPLICATION_ERROR_MASK|MSG_BAD_DEFFILE_KEYWORD)
#define ERROR_BAD_DEFFILE_SYNTAX    (APPLICATION_ERROR_MASK|MSG_BAD_DEFFILE_SYNTAX)
#define ERROR_BAD_DEFFILE_EXPORT    (APPLICATION_ERROR_MASK|MSG_BAD_DEFFILE_EXPORT)
#define ERROR_CANT_READ_DEFFILE     (APPLICATION_ERROR_MASK|MSG_CANT_READ_DEFFILE)
#define ERROR_INVALID_ARCHIVE       (APPLICATION_ERROR_MASK|MSG_INVALID_ARCHIVE)
#define ERROR_NO_LONGNAMES_MEMBER   (APPLICATION_ERROR_MASK|MSG_NO_LONGNAMES_MEMBER)
#define ERROR_NO_ARCHIVE_FILE       (APPLICATION_ERROR_MASK|MSG_NO_ARCHIVE_FILE)
#define ERROR_NO_EXPORT_FILE        (APPLICATION_ERROR_MASK|MSG_NO_EXPORT_FILE)
#define ERROR_NO_MACHINE            (APPLICATION_ERROR_MASK|MSG_NO_MACHINE)
#define ERROR_INVALID_MACHINE       (APPLICATION_ERROR_MASK|MSG_INVALID_MACHINE)
#define ERROR_INVALID_EXECFILE      (APPLICATION_ERROR_MASK|MSG_INVALID_EXECFILE)
#define ERROR_CANT_FIND_EXPDIR      (APPLICATION_ERROR_MASK|MSG_CANT_FIND_EXPDIR)
#define ERROR_FORWARDED_SYMBOL      (APPLICATION_ERROR_MASK|MSG_FORWARDED_SYMBOL)
#define ERROR_ALREADY_DEFINED       (APPLICATION_ERROR_MASK|MSG_ALREADY_DEFINED)
#define ERROR_CANT_FIND_MEMBER      (APPLICATION_ERROR_MASK|MSG_CANT_FIND_MEMBER)
#define ERROR_NO_MEMBERS            (APPLICATION_ERROR_MASK|MSG_NO_MEMBERS)
#define ERROR_CANT_CONVERT          (APPLICATION_ERROR_MASK|MSG_CANT_CONVERT)
#define ERROR_INTERNAL              (APPLICATION_ERROR_MASK|MSG_INTERNAL_ERROR)

#define MYOPENERROR(err) \
    ((((err) & APPLICATION_ERROR_MASK) == 0 && \
     (((err) & 0x00007FFF) == ERROR_FILE_NOT_FOUND || \
      ((err) & 0x00007FFF) == ERROR_PATH_NOT_FOUND)) ? \
     (((err) & 0xFFFF8000)|ERROR_OPEN_FILE) : (err))

/* main.c */
void lib_args(int, char **);
void apperror(WINERR, ...);
void printmsg(int, ...);
void errorexit(int);

/* archive.c */
void list_archive_file(void);
void extract_archive_members(void);
void explode_archive_file(void);
void enum_archive_members(FILEINFO *, int (__stdcall *)(FILEINFO *, const char *, size_t, const void *, void *), void *);

/* export.c */
void process_executable_image(FILEINFO *);
void process_export_list(void);

/* object.c */
void process_object_module(const char *, size_t, const void *, time_t);
void create_archive_file(void);
const char *import_archive_member_name(const char *);

/* dictary.c */
SYMENTRY *lookup_symbol(const char *);
EXPENTRY *lookup_export(const char *, int, bool_t);
FILEINFO *lookup_file(FILEINFO **, const char *);
void remove_file_from_list(FILEINFO **, FILEINFO *);
NAMENTRY *lookup_name(NAMENTRY **, const char *);
void remove_name_from_list(NAMENTRY **, NAMENTRY *);

/* utils.c */
void *my_alloc(size_t);
void *my_realloc(void *, size_t);
void my_free(void *);
WINERR my_openmap(FILEINFO *);
WINERR my_createmap(FILEINFO *, ulong_t);
WINERR my_closemap(FILEINFO *, bool_t);
WINERR my_create_binfile(const char *, const void *, ulong_t);
WINERR my_deletefile(const char *);
WINERR my_renamefile(const char *, const char *);
char *read_respfile(const char *, char);
void tokenize(char *, int *, char ***);
const char *basename(const char *);
int my_exception_filter(DWORD, WINERR *);

/* Global variables */
extern struct options options;
extern int nerrs;
extern FILEINFO *obj_list;
extern FILEINFO *lib_file;
extern FILEINFO *tmp_file;
extern NAMENTRY *extract_list;
extern NAMENTRY *remove_list;
extern SYMENTRY **symbol_list;
extern size_t symbol_count;
extern EXPENTRY **export_list;
extern size_t export_count;
extern char export_fname[MAX_PATH];
extern time_t time_stamp;

__inline void write_big_endian(long *s, long n)
{
    _asm mov eax,[n];
    _asm mov edx,[s];
    _asm bswap eax;
    _asm mov [edx],eax;
}

#pragma warning(disable:4035)  /* no return statement */
__inline long read_big_endian(long *s)
{
    _asm mov eax,[s];
    _asm mov eax,[eax];
    _asm bswap eax;
}
#pragma warning(default:4035)

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

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _LIB_H */
