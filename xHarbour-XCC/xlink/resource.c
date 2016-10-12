/****************************************************************************
 *                                                                          *
 * File    : resource.c                                                     *
 *                                                                          *
 * Purpose : Win32 Linker; resource file management.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include <resource.h>
#include "link.h"

/*
 * About the ".rsrc" section:
 *
 * Resources are indexed by a multiple level binary-sorted tree structure.
 * The general design can incorporate 2**31 levels. By convention, however,
 * Windows NT uses three levels:
 *
 * - Type
 * - Name
 * - Language
 *
 * A series of Resource Directory Tables relate all the levels in the
 * following way: each directory table is followed by a series of directory
 * entries, which give the name or ID for that level (Type, Name, or
 * Language level) and an address of either a data description or another
 * directory table. If a data description is pointed to, then the data is
 * a leaf in the tree. If another directory table is pointed to, then that
 * table lists directory entries at the next level down.
 *
 * A leaf's Type, Name, and Language IDs are determined by the path taken,
 * through directory tables, to reach the leaf. The first table determines
 * Type ID, the second table (pointed to by the directory entry in the first
 * table) determines Name ID, and the third table determines Language ID.
 */

__inline bool_t is_ordinal(const wchar_t *s)
{
    return (s[0] == (wchar_t)-1) ? TRUE : FALSE;
}

__inline ushort_t ordinal(const wchar_t *s)
{
    return (s[0] == (wchar_t)-1) ? s[1] : 0;
}

/* Resource node entry */
typedef struct _RESENTRY
{
    struct _RESENTRY *next;             /* next node */
    struct _RESENTRY *numbered_list;    /* first numbered resource */
    struct _RESENTRY *named_list;       /* first named resource */
    RSRC_HDR *hdr;                      /* actual resource */
    short count;                        /* total count (numbered + named) */
    short numbered;                     /* count of numbered resources */
    short named;                        /* count of named resources */
} RESENTRY;

/* Resource level functions */
typedef struct _RESFUNCS
{
    size_t (__stdcall *strsize_of_item)(const RSRC_HDR *);
    const wchar_t *(__stdcall *strptr_to_item)(const RSRC_HDR *);
} RESFUNCS;

/* Locals */
static RESENTRY resource_root = {0};
static size_t directory_size = 0;
static size_t string_size = 0;
static size_t data_size = 0;
static char *string_ptr = NULL;
static void *data_ptr = NULL;

/* Static function prototypes */
static RSRC_HDR *next_resource_header(RSRC_HDR *);
static RESENTRY *lookup_named_resource(RESENTRY *, RSRC_HDR *, int (__stdcall *)(const RSRC_HDR *, const RSRC_HDR *));
static RESENTRY *lookup_numbered_resource(RESENTRY *, RSRC_HDR *, int (__stdcall *)(const RSRC_HDR *, const RSRC_HDR *));
static int comp_numbered_type(const RSRC_HDR *, const RSRC_HDR *);
static int comp_named_type(const RSRC_HDR *, const RSRC_HDR *);
static int comp_numbered_name(const RSRC_HDR *, const RSRC_HDR *);
static int comp_named_name(const RSRC_HDR *, const RSRC_HDR *);
static int comp_language(const RSRC_HDR *, const RSRC_HDR *);
static void free_resource_tree(RESENTRY *);
static void make_resource_section(void);
static void sizeof_resource_tree(RESENTRY *, RESFUNCS *);
static size_t strsize_of_resource_type(const RSRC_HDR *);
static size_t strsize_of_resource_name(const RSRC_HDR *);
static long write_resource_tree(RESENTRY *, RESFUNCS *, SEGENTRY *, long);
static long write_string(SEGENTRY *, const wchar_t *);
static __inline const wchar_t *resource_type(const RSRC_HDR *);
static const wchar_t *resource_name(const RSRC_HDR *);
static ushort_t resource_language(const RSRC_HDR *);
static const wchar_t *resource_language_ord(const RSRC_HDR *);

/* After the function prototypes... */
static RESFUNCS resource_funcs[] =
{
    { strsize_of_resource_type, resource_type },
    { strsize_of_resource_name, resource_name },
    { NULL, resource_language_ord }
};

/****************************************************************************
 *                                                                          *
 * Function: is_resource_file                                               *
 *                                                                          *
 * Purpose : Return true if the file looks like a WIN32 resource file.      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           98-03-17  Changed code to support empty files.                 *
 *                                                                          *
 ****************************************************************************/

bool_t is_resource_file(FILEINFO *res_file)
{
    RSRC_HDR *hdr;
    bool_t is_resource;

    hdr = (RSRC_HDR *)res_file->base;

    /* The first record is an invalid record with predefined contents */
    is_resource = (res_file->size >= sizeof(RSRC_HDR) &&
        hdr->head.hdrsize == sizeof(RSRC_HDR) &&
        hdr->head.ressize == 0 &&
        is_ordinal(hdr->type) &&
        is_ordinal(hdr->name) &&
        hdr->tail.version_data == 0 &&
        hdr->tail.version == 0) ? TRUE : FALSE;

    return is_resource;
}

/****************************************************************************
 *                                                                          *
 * Function: process_resource_file                                          *
 *                                                                          *
 * Purpose : Process a single resource file.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

void process_resource_file(FILEINFO *res_file)
{
    RSRC_HDR *hdr;
    SCNENTRY *scn;

    /*
     * Have we already processed a resource file?
     */
    scn = lookup_section(COFF_RSRC, COFF_RSRC_FLAGS);
    if (scn->segs != NULL)
    {
        apperror(RCWARNING(ERROR_ONLY_ONE_RESFILE), basename(res_file->name));
        return;
    }

    /* Skip the useless WIN32 marker header */
    hdr = next_resource_header((RSRC_HDR *)res_file->base);

    while (!end_of_file(res_file, hdr))
    {
        RESENTRY *res = &resource_root;

        if (ordinal(resource_type(hdr)) != RSRC_T_DLGINCLUDE)
        {
            /*
             * Add a sorted entry for the resource type.
             */
            if (is_ordinal(resource_type(hdr)))
                res = lookup_numbered_resource(res, hdr, comp_numbered_type);
            else
                res = lookup_named_resource(res, hdr, comp_named_type);

            /*
             * Add a sorted entry for the resource name.
             */
            if (is_ordinal(resource_name(hdr)))
                res = lookup_numbered_resource(res, hdr, comp_numbered_name);
            else
                res = lookup_named_resource(res, hdr, comp_named_name);

            /*
             * Add a sorted entry for the resource language.
             */
            lookup_numbered_resource(res, hdr, comp_language);
        }

        /* Take us to the next resource, please! */
        hdr = next_resource_header(hdr);
    }

    /*
     * Did we find any resources in the file?
     */
    if (resource_root.count == 0)
    {
        apperror(RCWARNING(ERROR_EMPTY_RESFILE), basename(res_file->name));
        return;
    }

    /* Build the resource section */
    make_resource_section();

    free_resource_tree(&resource_root);
}

/****************************************************************************
 *                                                                          *
 * Function: next_resource_header                                           *
 *                                                                          *
 * Purpose : Walk past the current resource.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static RSRC_HDR *next_resource_header(RSRC_HDR *hdr)
{
    long size;

    size = ROUNDUP_DWORD(hdr->head.ressize + hdr->head.hdrsize);

    return (RSRC_HDR *)((char *)hdr + size);
}

/****************************************************************************
 *                                                                          *
 * Function: lookup_named_resource                                          *
 *                                                                          *
 * Purpose : Add a sorted entry to the resource tree (named resource).      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static RESENTRY *lookup_named_resource(RESENTRY *res_node, RSRC_HDR *hdr,
    int (__stdcall *comp)(const RSRC_HDR *, const RSRC_HDR *))
{
    RESENTRY *resT;
    RESENTRY *res_prev;
    RESENTRY *res;
    int cond = !0;

    /* Search this level for a match */
    for (resT = res_node->named_list, res_prev = NULL;
         resT != NULL && (cond = comp(resT->hdr, hdr)) < 0;
         res_prev = resT, resT = resT->next)
        ;

    if (cond == 0)
        return resT;

    /* Didn't find the entry; add it */
    res = (RESENTRY *)my_alloc(sizeof(RESENTRY));

    res->named_list = NULL;
    res->numbered_list = NULL;
    res->hdr = hdr;
    res->count = res->named = res->numbered = 0;

    if (res_prev)
    {
        res_prev->next = res;
        res->next = resT;
    }
    else
    {
        res_node->named_list = res;
        res->next = resT;
    }

    res_node->named++;
    res_node->count++;

    return res;
}

/****************************************************************************
 *                                                                          *
 * Function: lookup_numbered_resource                                       *
 *                                                                          *
 * Purpose : Add a sorted entry to the resource tree (numbered resource).   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static RESENTRY *lookup_numbered_resource(RESENTRY *res_node, RSRC_HDR *hdr,
    int (__stdcall *comp)(const RSRC_HDR *, const RSRC_HDR *))
{
    RESENTRY *resT;
    RESENTRY *res_prev;
    RESENTRY *res;
    int cond = !0;

    /* Search this level for a match */
    for (resT = res_node->numbered_list, res_prev = NULL;
         resT != NULL && (cond = comp(resT->hdr, hdr)) < 0;
         res_prev = resT, resT = resT->next)
        ;

    if (cond == 0)
        return resT;

    /* Didn't find the entry; add it */
    res = (RESENTRY *)my_alloc(sizeof(RESENTRY));

    res->named_list = NULL;
    res->numbered_list = NULL;
    res->hdr = hdr;
    res->count = res->named = res->numbered = 0;

    if (res_prev)
    {
        res_prev->next = res;
        res->next = resT;
    }
    else
    {
        res_node->numbered_list = res;
        res->next = resT;
    }

    res_node->numbered++;
    res_node->count++;

    return res;
}

/****************************************************************************
 *                                                                          *
 * Function: comp_numbered_type                                             *
 *                                                                          *
 * Purpose : Compare two numbered types (for lookup_numbered_resource).     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int comp_numbered_type(const RSRC_HDR *hdr1, const RSRC_HDR *hdr2)
{
    return ordinal(resource_type(hdr1)) - ordinal(resource_type(hdr2));
}

/****************************************************************************
 *                                                                          *
 * Function: comp_named_type                                                *
 *                                                                          *
 * Purpose : Compare two named types (for lookup_named_resource).           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int comp_named_type(const RSRC_HDR *hdr1, const RSRC_HDR *hdr2)
{
    return _wcsicmp(resource_type(hdr1), resource_type(hdr2));
}

/****************************************************************************
 *                                                                          *
 * Function: comp_numbered_name                                             *
 *                                                                          *
 * Purpose : Compare two numbered names (for lookup_numbered_resource).     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int comp_numbered_name(const RSRC_HDR *hdr1, const RSRC_HDR *hdr2)
{
    return ordinal(resource_name(hdr1)) - ordinal(resource_name(hdr2));
}

/****************************************************************************
 *                                                                          *
 * Function: comp_named_name                                                *
 *                                                                          *
 * Purpose : Compare two named names (for lookup_named_resource).           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int comp_named_name(const RSRC_HDR *hdr1, const RSRC_HDR *hdr2)
{
    return _wcsicmp(resource_name(hdr1), resource_name(hdr2));
}

/****************************************************************************
 *                                                                          *
 * Function: comp_language                                                  *
 *                                                                          *
 * Purpose : Compare two language IDs (for lookup_numbered_resource).       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int comp_language(const RSRC_HDR *hdr1, const RSRC_HDR *hdr2)
{
    return resource_language(hdr1) - resource_language(hdr2);
}

/****************************************************************************
 *                                                                          *
 * Function: free_resource_tree                                             *
 *                                                                          *
 * Purpose : Free the resource tree.                                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void free_resource_tree(RESENTRY *res_node)
{
    RESENTRY *res;
    RESENTRY *res_next;

    /* Free the list of named resources */
    for (res = res_node->named_list; res != NULL; res = res_next)
    {
        free_resource_tree(res);
        res_next = res->next;
        my_free(res);
    }

    /* Free the list of numbered resources */
    for (res = res_node->numbered_list; res != NULL; res = res_next)
    {
        free_resource_tree(res);
        res_next = res->next;
        my_free(res);
    }

    /* Clear the resource counters */
    res_node->named = res_node->numbered = res_node->count = 0;
}

/****************************************************************************
 *                                                                          *
 * Function: make_resource_section                                          *
 *                                                                          *
 * Purpose : Create the resource section (".rsrc").                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void make_resource_section(void)
{
    SCNENTRY *scn;
    SEGENTRY *seg;
    SYMENTRY *sym;

    /*
     * Calculate the total size of the resource tree.
     */
    directory_size = string_size = data_size = 0;
    sizeof_resource_tree(&resource_root, resource_funcs);
    string_size = ROUNDUP_DWORD(string_size);  /* align data_ptr! */

    /*
     * Allocate a section buffer.
     */
    scn = lookup_section(COFF_RSRC, COFF_RSRC_FLAGS);
    seg = add_segment_to_section(scn);

    seg->alignment = sizeof(long);
    seg->size = directory_size + string_size + data_size;
    seg->data = my_alloc(seg->size);

    /*
     * Initialize the global pointers.
     */
    string_ptr = (char *)seg->data + directory_size;
    data_ptr = (char *)string_ptr + string_size;  /* aligned; see above! */

    /*
     * Get a symbol for the relocations.
     */
    sym = lookup_static(seg, COFF_RSRC);
    sym->type = resolved;

    /* Write the resource tree to the buffer */
    write_resource_tree(&resource_root, resource_funcs, seg, 0);
}

/****************************************************************************
 *                                                                          *
 * Function: sizeof_resource_tree                                           *
 *                                                                          *
 * Purpose : Calculate the total size of the resource tree.                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void sizeof_resource_tree(RESENTRY *res_node, RESFUNCS *funcs)
{
    if (res_node->count != 0)  /* directory */
    {
        RESENTRY *res;

        directory_size += sizeof(COFF_RESHDR);

        for (res = res_node->named_list; res != NULL; res = res->next)
        {
            directory_size += sizeof(COFF_RESDIR);
            sizeof_resource_tree(res, funcs+1);
            string_size += funcs->strsize_of_item(res->hdr);
        }

        for (res = res_node->numbered_list; res != NULL; res = res->next)
        {
            directory_size += sizeof(COFF_RESDIR);
            sizeof_resource_tree(res, funcs+1);
        }
    }
    else  /* leaf */
    {
        directory_size += sizeof(COFF_RESENT);
        data_size += ROUNDUP_DWORD(res_node->hdr->head.ressize);
    }
}

/****************************************************************************
 *                                                                          *
 * Function: strsize_of_resource_type                                       *
 *                                                                          *
 * Purpose : Return the size of a resource directory string (type).         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static size_t strsize_of_resource_type(const RSRC_HDR *hdr)
{
    return sizeof(COFF_RESSTR) + wcslen(resource_type(hdr)) * sizeof(wchar_t);
}

/****************************************************************************
 *                                                                          *
 * Function: strsize_of_resource_name                                       *
 *                                                                          *
 * Purpose : Return the size of a resource directory string (name).         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static size_t strsize_of_resource_name(const RSRC_HDR *hdr)
{
    return sizeof(COFF_RESSTR) + wcslen(resource_name(hdr)) * sizeof(wchar_t);
}

/****************************************************************************
 *                                                                          *
 * Function: write_resource_tree                                            *
 *                                                                          *
 * Purpose : Write the resource directory tree.                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           99-01-22  Changed to align_dword_ptr() with padding.           *
 *           00-11-16  Support for ARM machine added.                       *
 *                                                                          *
 ****************************************************************************/

static long write_resource_tree(RESENTRY *res_node, RESFUNCS *funcs, SEGENTRY *seg, long offset)
{
    RESENTRY *res;

    if (res_node->count != 0)  /* directory */
    {
        COFF_RESHDR *reshdr;
        COFF_RESDIR *resdir;

        /*
         * Write the directory header.
         */
        reshdr = (COFF_RESHDR *)((char *)seg->data + offset);
        reshdr->rh_flags = 0;
        reshdr->rh_timdat = time_stamp;
        reshdr->rh_majver = 0;
        reshdr->rh_minver = 0;
        reshdr->rh_nnames = res_node->named;
        reshdr->rh_nids = res_node->numbered;
        reshdr++;

        resdir = (COFF_RESDIR *)reshdr;

        offset += sizeof(COFF_RESHDR) + res_node->count * sizeof(COFF_RESDIR);

        /*
         * Write the named directory entries (must be first).
         */
        for (res = res_node->named_list; res != NULL; res = res->next)
        {
            long strptr = write_string(seg, funcs->strptr_to_item(res->hdr));

            resdir->rd_strpos = strptr;
            resdir->rd_strent = TRUE;
            resdir->rd_dirpos = offset;
            resdir->rd_dirent = (res->count != 0);
            resdir++;

            /* Write lower level nodes */
            offset = write_resource_tree(res, funcs+1, seg, offset);
        }

        /*
         * Write the numbered directory entries.
         */
        for (res = res_node->numbered_list; res != NULL; res = res->next)
        {
            resdir->rd_name = ordinal(funcs->strptr_to_item(res->hdr));
            resdir->rd_dirpos = offset;
            resdir->rd_dirent = (res->count != 0);
            resdir++;

            /* Write lower level nodes */
            offset = write_resource_tree(res, funcs+1, seg, offset);
        }
    }
    else  /* leaf */
    {
        COFF_RESENT *resent;
        RELENTRY *rel;

        /*
         * Write the leaf entry.
         */
        resent = (COFF_RESENT *)((char *)seg->data + offset);
        offset += sizeof(COFF_RESENT);

        resent->re_offset = ((char *)data_ptr - (char *)seg->data);
        resent->re_size = res_node->hdr->head.ressize;
        resent->re_codepage = 0;
        resent->re_reserved = 0;

        /*
         * Copy raw data for the resource.
         */
        memcpy(data_ptr, (char *)res_node->hdr + res_node->hdr->head.hdrsize, res_node->hdr->head.ressize);
        data_ptr = align_dword_ptr((char *)data_ptr + res_node->hdr->head.ressize);

        /*
         * Add a relocation entry.
         */
        rel = add_relocation_to_segment(seg);
        rel->offset = ((char *)&resent->re_offset - (char *)seg->data);
        rel->type = (options.machine == MACHINE_ARM) ? COFF_R_ARM_ADDR32NB : COFF_R_I386_DIR32NB;
        rel->sym = seg->stcs;
    }

    return offset;
}

/****************************************************************************
 *                                                                          *
 * Function: write_string                                                   *
 *                                                                          *
 * Purpose : Write a resource directory string.                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static long write_string(SEGENTRY *seg, const wchar_t *name)
{
    COFF_RESSTR *resstr;

    resstr = (COFF_RESSTR *)string_ptr;

    resstr->rs_size = (ushort_t)wcslen(name);
    wcscpy(resstr->rs_name, name);

    string_ptr += sizeof(*resstr) + resstr->rs_size * sizeof(wchar_t);

    return ((char *)resstr - (char *)seg->data);
}

/****************************************************************************
 *                                                                          *
 * Function: resource_type                                                  *
 *                                                                          *
 * Purpose : Return a pointer to the resource type.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static __inline const wchar_t *resource_type(const RSRC_HDR *hdr)
{
    return hdr->type;
}

/****************************************************************************
 *                                                                          *
 * Function: resource_name                                                  *
 *                                                                          *
 * Purpose : Return a pointer to the resource name.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static const wchar_t *resource_name(const RSRC_HDR *hdr)
{
    return is_ordinal(hdr->type) ? hdr->name :
        hdr->type + wcslen(hdr->type) + 1;
}

/****************************************************************************
 *                                                                          *
 * Function: resource_language                                              *
 *                                                                          *
 * Purpose : Return the resource language.                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           98-05-24  Bugfix: rewritten; didn't handle header alignment.   *
 *                                                                          *
 ****************************************************************************/

static ushort_t resource_language(const RSRC_HDR *hdr)
{
    ushort_t *language;

    /* After *aligned* header, minus characteristics, version, language */
    language = (ushort_t *)((char *)hdr + hdr->head.hdrsize -
        sizeof(ulong_t) - sizeof(long) - sizeof(ushort_t));

    return *language;
}

/****************************************************************************
 *                                                                          *
 * Function: resource_language_ord                                          *
 *                                                                          *
 * Purpose : Return the resource language as an ordinal.                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static const wchar_t *resource_language_ord(const RSRC_HDR *hdr)
{
    static wchar_t name[2];

    name[0] = (wchar_t)-1;
    name[1] = resource_language(hdr);

    return name;
}

