/****************************************************************************
 *                                                                          *
 * File    : defs.h                                                         *
 *                                                                          *
 * Purpose : Win32 Linker; constants and definitions.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           98-03-21  New fields in SYMENTRY: next_bucket, checksum.       *
 *           98-11-16  New field in SYMENTRY: class.                        *
 *           98-11-25  Added MODENTRY structure and modified SEGENTRY.      *
 *           98-11-29  Added LINENTRY structure.                            *
 *           98-12-05  New field in GRPENTRY, SYMENTRY: scnum.              *
 *           98-12-30  New field in SCNENTRY: seg_last.                     *
 *           99-02-10  Added DLLENTRY structure.                            *
 *           99-02-10  New field in FILEINFO: delay_dll.                    *
 *           99-04-02  New field in RENENTRY: visited.                      *
 *           99-08-18  New field in SYMENTRY: referenced.                   *
 *           01-09-25  Added ATTENTRY structure.                            *
 *           02-04-04  New field in SYMENTRY: index.                        *
 *           02-04-04  New field in LINENTRY: sym.                          *
 *           02-04-04  New field in SEGENTRY: nlines.                       *
 *           03-06-16  Added LIBENTRY structure.                            *
 *           03-09-14  New field in SYMENTRY: comseg.                       *
 *                                                                          *
 ****************************************************************************/

#ifndef _DEFS_H
#define _DEFS_H

#ifdef __cplusplus
extern "C" {
#endif

/* Relocation entry */
typedef struct _RELENTRY
{
    struct _RELENTRY *next;         /* next node */
    struct _SYMENTRY *sym;          /* relocation symbol */
    long offset;                    /* fixup offset in segment */
    ushort_t type;                  /* fixup type */
} RELENTRY;

/* Linenumber entry */
typedef struct _LINENTRY
{
    struct _SYMENTRY *sym;          /* function symbol or NULL */
    addr_t vaddr;                   /* line address */
    ushort_t lineno;                /* line number */
} LINENTRY;

/* Combined sections entry (executable file) */
typedef struct _GRPENTRY
{
    struct _GRPENTRY *next;         /* next node */
    struct _SCNENTRY *scngrp;       /* first section in group */
    ulong_t flags;                  /* group characteristics */
    addr_t vaddr;                   /* group address */
    short scnum;                    /* group (=section) number */
    char name[1];                   /* group name */
} GRPENTRY;

/* Section entry (executable file) */
typedef struct _SCNENTRY
{
    struct _SCNENTRY *next;         /* next node */
    struct _SCNENTRY *scngrp;       /* next section in group */
    struct _SEGENTRY *segs;         /* first segment in section */
    struct _SEGENTRY *seg_last;     /* last segment; cached for speed */
    ulong_t flags;                  /* section characteristics */
    char grouped;                   /* *internal flag* */
    char name[1];                   /* section name */
} SCNENTRY;

/* Section entry (object file) */
typedef struct _SEGENTRY
{
    struct _SEGENTRY *next;         /* next node */
    struct _SYMENTRY *pubs;         /* first public symbol defined here */
    struct _SYMENTRY *stcs;         /* first static symbol defined here */
    struct _MODENTRY *mod;          /* module description or NULL */
    addr_t vaddr;                   /* segment address */
    long alignment;                 /* alignment (or bytes to achieve it) */
    long size;                      /* raw data size */
    void *data;                     /* raw data pointer */
    RELENTRY *relocs;               /* first relocation entry */
    LINENTRY *lines;                /* linenumber array */
    short nlines;                   /* linenumber count */
    char keep_last;                 /* this is the ??_NULL_THUNK_DATA */
} SEGENTRY;

/* Symbol table entry */
typedef struct _SYMENTRY
{
    struct _SYMENTRY *next_bucket;  /* next hash bucket */
    struct _SYMENTRY *next;         /* next node */
    struct _SYMENTRY *syms;         /* next public/static in segment */
    char *name;                     /* symbol name */
    char *weak_name;                /* alternate name (weak externals) */
    struct _MODENTRY *def_mod;
    struct _MODENTRY *dec_mod;
    long value;                     /* symbol value */
    long checksum;                  /* comdat section checksum */
    SEGENTRY *comseg;               /* largest comdat section seen so far */
    enum
    {
        unused,                     /* symbol is unused */
        unresolved,                 /* symbol is unresolved */
        unresolved_weak,            /* symbol is unresolved (weak) */
        resolved                    /* symbol is resolved */
    } type;
    struct
    {
#ifdef PRERELEASE
        uint_t relocated: 1;        /* internal flag for debugging purposes */
#endif
        uint_t absolute: 1;         /* symbol is absolute */
        uint_t common: 1;           /* symbol is a communal */
        uint_t internal: 1;         /* symbol is internal */
        uint_t function: 1;         /* symbol is a function */
        uint_t referenced: 1;       /* symbol is needed by somebody */
        uint_t nosearch: 1;         /* don't search libraries for symbol */
    } flags;
    short scnum;                    /* section number */
    char comdat;                    /* comdat section selection type */
    char class;                     /* COFF symbol class */
    long index;                     /* COFF symbol table index */
} SYMENTRY;

/* Module description entry */
typedef struct _MODENTRY
{
    struct _MODENTRY *next;         /* next node */
    struct _FILEINFO *lib_file;     /* archive description (or NULL) */
    struct _HASHNODE *obj_file;     /* object description (or NULL) */
    struct _HASHNODE *src_file;     /* source description (or NULL) */
    short modnum;                   /* module number (CV) */
} MODENTRY;

/* File description (archive/object/executable/map) */
typedef struct _FILEINFO
{
    struct _FILEINFO *next;         /* next node */
    struct _DLLENTRY *delay_dll;    /* archive only: delay import or NULL */
    HANDLE hf;                      /* file handle */
    HANDLE hmap;                    /* file mapping handle */
    void *base;                     /* file mapping base */
    /*_fsize_t*/unsigned long size;                  /* file size */
    char name[1];                   /* file name */
} FILEINFO;

/* Delayed import entry */
typedef struct _DLLENTRY
{
    struct _DLLENTRY *next;         /* next node */
    struct
    {
        char referenced;            /* import symbols found */
    } flags;
    char name[1];                   /* file name */
} DLLENTRY;

/* Library path entry */
typedef struct _LIBENTRY
{
    struct _LIBENTRY *next;         /* next node */
    char name[1];                   /* name */
} LIBENTRY;

/* Section alias entry */
typedef struct _RENENTRY
{
    struct _RENENTRY *next;         /* next node */
    char *from_name;                /* from section name */
    char *to_name;                  /* to section name */
    char visited;                   /* visited before, i.e. circular link? */
} RENENTRY;

/* Section attributes entry */
typedef struct _ATTENTRY
{
    struct _ATTENTRY *next;         /* next node */
    ulong_t memflags;               /* executable | readable | writable */
    ulong_t addflags;               /* shared | discardable */
    char name[1];                   /* section name */
} ATTENTRY;

/* Export entry */
typedef struct _EXPENTRY
{
    struct _EXPENTRY *next;         /* next node */
    char *name;                     /* external name */
    SYMENTRY *sym;                  /* internal symbol */
    long offset;                    /* archive member offset */
    ushort_t ordinal;               /* ordinal number (or zero) */
} EXPENTRY;

/* Hash entry */
typedef struct _HASHNODE
{
    struct _HASHNODE *next_bucket;  /* next bucket */
    char name[1];                   /* name */
} HASHNODE;

#define tstrcpy(s)    (strlen(s) ? strcpy(my_alloc((strlen(s) + 1)), (s)) : NULL)
#define tstrcat(s,p)  (strcat(strcpy(my_alloc((strlen(s) + strlen(p) + 1)), (s)), (p)))

#define file_offset(file,p) \
    ((char *)(p) - (char *)(file)->base)

#define end_of_file(file,p) \
    ((char *)(p) >= ((char *)(file)->base + (file)->size))

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* _DEFS_H */
