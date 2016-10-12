/****************************************************************************
 *                                                                          *
 * File    : map.c                                                          *
 *                                                                          *
 * Purpose : Win32 Linker; map file management.                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "link.h"

#define MAX_MAPFILE_SIZE      0x1000000     /* 16 MB */
#define SORT_ARRAY_INCREMENT  256
#define MAX_LINE_WIDTH        70

#define SECTION_CLASS(scn) \
 (((scn)->flags & COFF_STYP_TEXT) ? "CODE" : \
  ((scn)->flags & COFF_STYP_DATA) ? "DATA" : \
  ((scn)->flags & COFF_STYP_BSS) ? "DATA" : "")

typedef struct _SORTENTRY
{
    SYMENTRY *pub;  /* public symbol */
    SEGENTRY *seg;  /* defining segment (with filename) */
} SORTENTRY;

/* Static function prototypes */
static char *write_map_file(char *);
static int __cdecl comp_symbol_value(const void *, const void *);
static int __cdecl comp_ordinal(const void *, const void *);
static void symbol_object_file(char *, MODENTRY *);
static void *create_map_map(void);
static void close_map_map(void *);

/****************************************************************************
 *                                                                          *
 * Function: write_map_image                                                *
 *                                                                          *
 * Purpose : Write the (optional) map file.                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

void write_map_image(void)
{
    if (options.mapfile)
    {
        WINERR err = 0;
        void *ip;

        ip = create_map_map();

        __try
        {
            ip = write_map_file(ip);
        }
        __except (my_exception_filter(GetExceptionCode(), &err))
        {
            apperror(RCFATAL(err), "write_map_image");
        }

        close_map_map(ip);
    }
}

/****************************************************************************
 *                                                                          *
 * Function: write_map_file                                                 *
 *                                                                          *
 * Purpose : Write the map file.                                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           98-12-12  Export information added; fixups made optional.      *
 *           00-11-16  Support for ARM machine added (fixups).              *
 *                                                                          *
 ****************************************************************************/

static char *write_map_file(char *ip)
{
    GRPENTRY *grp;
    short scnum_entry = 0;
    addr_t vaddr_entry = 0;
    addr_t vaddr_prev = 0;
    char *ip_line = 0;

    /*
     * Output header + module info.
     */
    ip += sprintf(ip, " %s\r\n\r\n", basename(exe_file->name));
    ip += sprintf(ip, " Timestamp is %x (%.24s)\r\n\r\n", time_stamp, ctime(&time_stamp));
    ip += sprintf(ip, " Preferred load address is %08x\r\n\r\n", options.image_base);
    ip += sprintf(ip, " Start         Length     Name                   Class\r\n");

    /*
     * Output section info.
     */
    for (grp = group_list; grp != NULL; grp = grp->next)
    {
        SCNENTRY *scn;

        for (scn = grp->scngrp; scn != NULL; scn = scn->scngrp)
        {
            addr_t vaddr = address_of_section(scn) - grp->vaddr;
            long size = size_of_section(scn);

            if (size != 0)
            {
                ip += sprintf(ip, " %04hx:%08x %08xH %-23s %-22s\r\n",
                    grp->scnum, vaddr, size, scn->name, SECTION_CLASS(scn));
            }
        }
    }

    /*
     * Output header.
     */
    ip += sprintf(ip, "\r\n  Address         Publics by Value              Rva+Base     Lib:Object\r\n\r\n");

    /*
     * Output public symbols.
     */
    for (grp = group_list; grp != NULL; grp = grp->next)
    {
        SCNENTRY *scn;
        SORTENTRY *sort_array = NULL;
        size_t sort_maxcount = 0;
        size_t sort_count = 0;
        size_t i;

        for (scn = grp->scngrp; scn != NULL; scn = scn->scngrp)
        {
            SEGENTRY *seg;

            for (seg = scn->segs; seg != NULL; seg = seg->next)
            {
                SYMENTRY *pub;

                for (pub = seg->pubs; pub != NULL; pub = pub->syms)
                {
                    if (pub->flags.internal)
                        continue;

                    if (sort_count == sort_maxcount)
                    {
                        sort_maxcount += SORT_ARRAY_INCREMENT;
                        sort_array = (SORTENTRY *)my_realloc(sort_array, sort_maxcount * sizeof(SORTENTRY));
                    }

                    sort_array[sort_count].pub = pub;
                    sort_array[sort_count].seg = seg;
                    sort_count++;

                    if (pub == pub_entry)
                    {
                        /* Remember the image entry point */
                        scnum_entry = pub->scnum;
                        vaddr_entry = (pub->value - grp->vaddr);
                    }
                }
            }
        }

        /* Sort symbols in value order */
        qsort(sort_array, sort_count, sizeof(SORTENTRY), comp_symbol_value);

        for (i = 0; i < sort_count; i++)
        {
            SYMENTRY *pub = sort_array[i].pub;
            char filename[MAX_PATH*2];

            symbol_object_file(filename, sort_array[i].seg->mod);

            ip += sprintf(ip, " %04hx:%08x       %-26s %08x %c   %s\r\n",
                pub->scnum, pub->value - grp->vaddr, pub->name,
                pub->value + options.image_base,
                pub->flags.function ? 'f' : ' ',
                filename);
        }

        my_free(sort_array);
    }

    /*
     * Output header.
     */
    ip += sprintf(ip, "\r\n entry point at        %04hx:%08x\r\n", scnum_entry, vaddr_entry);
    ip += sprintf(ip, "\r\n Static symbols\r\n\r\n");

    /*
     * Output static symbols.
     */
    for (grp = group_list; grp != NULL; grp = grp->next)
    {
        SCNENTRY *scn;

        for (scn = grp->scngrp; scn != NULL; scn = scn->scngrp)
        {
            SEGENTRY *seg;

            for (seg = scn->segs; seg != NULL; seg = seg->next)
            {
                SYMENTRY *sym;

                for (sym = seg->stcs; sym != NULL; sym = sym->syms)
                {
                    char filename[MAX_PATH*2];

                    if (sym->flags.internal || !sym->flags.function)
                        continue;

                    symbol_object_file(filename, seg->mod);

                    ip += sprintf(ip, " %04hx:%08x       %-26s %08x %c   %s\r\n",
                        sym->scnum, sym->value - grp->vaddr, sym->name,
                        sym->value + options.image_base,
                        sym->flags.function ? 'f' : ' ',
                        filename);
                }
            }
        }
    }

    /*
     * Output linenumbers (if requested in /MAPINFO).
     */
    if (options.mapfile_lines)
    {
        MODENTRY *mod;

        for (mod = module_list; mod != NULL; mod = mod->next)
        {
            BOOL new_mod = TRUE;

            for (grp = group_list; grp != NULL; grp = grp->next)
            {
                SCNENTRY *scn;

                for (scn = grp->scngrp; scn != NULL; scn = scn->scngrp)
                {
                    SEGENTRY *seg;

                    for (seg = scn->segs; seg != NULL; seg = seg->next)
                    {
                        LINENTRY *lines;

                        if (seg->mod != mod) continue;

                        for (lines = seg->lines;
                             lines != NULL && lines < seg->lines + seg->nlines;
                             lines++)
                        {
                            if (new_mod)  /* been here, done that? */
                            {
                                ip += sprintf(ip, "\r\nLine numbers for %s(%s) segment %s\r\n\r\n",
                                    mod->obj_file->name, mod->src_file ? mod->src_file->name : "", grp->name);
                                ip_line = ip;
                                new_mod = FALSE;
                            }

                            if ((ip - ip_line) >= MAX_LINE_WIDTH)
                            {
                                ip += sprintf(ip, "\r\n");
                                ip_line = ip;
                            }

                            ip += sprintf(ip, " %5d %04hx:%08x", lines->lineno,
                                grp->scnum, seg->vaddr + lines->vaddr - grp->vaddr);
                        }
                    }
                }
            }
        }
    }

    /*
     * Output fixups (if requested in /MAPINFO).
     */
    if (options.mapfile_fixups)
    {
        ip += sprintf(ip, "\r\n");

        ip_line = ip;
        for (grp = group_list; grp != NULL; grp = grp->next)
        {
            SCNENTRY *scn;

            for (scn = grp->scngrp; scn != NULL; scn = scn->scngrp)
            {
                SEGENTRY *seg;

                for (seg = scn->segs; seg != NULL; seg = seg->next)
                {
                    RELENTRY *rel;

                    for (rel = seg->relocs; rel != NULL; rel = rel->next)
                    {
                        addr_t vaddr = seg->vaddr + rel->offset;

                        if ((options.machine == MACHINE_X86 && rel->type == COFF_R_I386_REL32) ||
                            (options.machine == MACHINE_ARM && rel->type == COFF_R_ARM_BRANCH24))
                        {
                            if ((ip - ip_line) >= MAX_LINE_WIDTH)
                            {
                                ip += sprintf(ip, "\r\n");
                                ip_line = ip;
                            }

                            if (ip == ip_line)
                            {
                                ip += sprintf(ip, "FIXUPS: %x", vaddr);
                                vaddr_prev = vaddr;
                            }
                            else
                            {
                                ip += sprintf(ip, " %x", vaddr - vaddr_prev);
                                vaddr_prev = vaddr;
                            }
                        }
                    }
                }
            }
        }

        if (ip != ip_line)
            ip += sprintf(ip, "\r\n");
    }

    /*
     * Output exports (if requested in /MAPINFO).
     */
    if (options.mapfile_exports)
    {
        EXPENTRY **sort_array = NULL;
        size_t sort_maxcount = 0;
        size_t sort_count = 0;
        size_t i;

        for (i = 0; i < export_count; i++)
        {
            /* Skip internal entries */
            if (export_list[i]->sym == NULL) continue;

            if (sort_count == sort_maxcount)
            {
                sort_maxcount += SORT_ARRAY_INCREMENT;
                sort_array = (EXPENTRY **)my_realloc(sort_array, sort_maxcount * sizeof(EXPENTRY *));
            }

            sort_array[sort_count] = export_list[i];
            sort_count++;

        }

        /* Sort exports in ordinal order */
        qsort(sort_array, sort_count, sizeof(EXPENTRY *), comp_ordinal);

        /*
         * Output header + export info.
         */
        ip += sprintf(ip, "\r\n Exports\r\n");
        ip += sprintf(ip, "\r\n  ordinal    name\r\n\r\n");

        for (i = 0; i < sort_count; i++)
        {
            EXPENTRY *exp = sort_array[i];

            ip += sprintf(ip, " %8d    %s\r\n",
                exp->ordinal, exp->sym->name);

            if (strcmp(exp->sym->name, exp->name) != 0)  /* alias? */
                ip += sprintf(ip, "\t\texported name: %s\r\n", exp->name);
        }
    }

    return ip;
}

/****************************************************************************
 *                                                                          *
 * Function: comp_symbol_value                                              *
 *                                                                          *
 * Purpose : Compare two values for the qsort() function.                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int __cdecl comp_symbol_value(const void *item1, const void *item2)
{
    SORTENTRY *sort1 = (SORTENTRY *)item1;
    SORTENTRY *sort2 = (SORTENTRY *)item2;

    return (sort1->pub->value - sort2->pub->value);
}

/****************************************************************************
 *                                                                          *
 * Function: comp_ordinal                                                   *
 *                                                                          *
 * Purpose : Compare two ordinals for the qsort() function.                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-12-12  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int __cdecl comp_ordinal(const void *item1, const void *item2)
{
    EXPENTRY *exp1 = *(EXPENTRY **)item1;
    EXPENTRY *exp2 = *(EXPENTRY **)item2;

    return (exp1->ordinal - exp2->ordinal);
}

/****************************************************************************
 *                                                                          *
 * Function: symbol_object_file                                             *
 *                                                                          *
 * Purpose : Return object filename (where the symbol is defined).          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           98-11-25  Changed 2nd argument to MODENTRY.                    *
 *                                                                          *
 ****************************************************************************/

static void symbol_object_file(char *buf, MODENTRY *mod)
{
    if (mod != NULL && mod->lib_file != NULL)
    {
        /* Archive member name (lib:object) */
        strcpy(buf, basename(mod->lib_file->name));
        update_extension_in_file(buf, "");
        strcat(buf, ":");
        strcat(buf, mod->obj_file->name);
    }
    else if (mod != NULL && mod->obj_file != NULL)
    {
        /* Object file name */
        strcpy(buf, mod->obj_file->name);
    }
    else
    {
        /* Communal symbol */
        strcpy(buf, "<common>");
    }
}

/****************************************************************************
 *                                                                          *
 * Function: create_map_map                                                 *
 *                                                                          *
 * Purpose : Create a read-write file mapping of the map file (sic).        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void *create_map_map(void)
{
    WINERR err;

    if (!map_file)
    {
        char fname[MAX_PATH];

        strcpy(fname, exe_file->name);
        update_extension_in_file(fname, EXT_MAP);
        lookup_file(&map_file, fname);
    }

    err = my_createmap(map_file, MAX_MAPFILE_SIZE);
    if (err) apperror(RCFATAL(err), map_file->name);

    return map_file->base;
}

/****************************************************************************
 *                                                                          *
 * Function: close_map_map                                                  *
 *                                                                          *
 * Purpose : Close a file-mapping of the map file.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void close_map_map(void *next_byte)
{
    WINERR err;

    /* Set size before we call my_closemap() */
    map_file->size = file_offset(map_file, next_byte);

    err = my_closemap(map_file, TRUE);
    if (err) apperror(RCFATAL(err));
}

