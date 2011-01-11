/****************************************************************************
 *                                                                          *
 * File    : order.c                                                        *
 *                                                                          *
 * Purpose : Win32 Linker; section order management.                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "link.h"

#define SORT_ARRAY_INCREMENT  32

#define COFF_STYP_BASIC_MEMORY  (COFF_STYP_READ|COFF_STYP_WRITE|COFF_STYP_EXEC)

/* Static function prototypes */
static void discard_unreferenced_segments(void);
static void segment_information(int, SEGENTRY *);
static void group_sections_by_name(void);
static int __cdecl comp_section_name(const void *, const void *);
static void apply_group_attributes_from_user(void);
static void order_groups(void);
static bool_t add_group_by_attribute(GRPENTRY **, ulong_t);

/****************************************************************************
 *                                                                          *
 * Function: order_sections                                                 *
 *                                                                          *
 * Purpose : Order sections according to 'Microsoft random rules'.          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           99-08-19  Added call to discard_unreferenced_segments().       *
 *           01-09-25  Added call to apply_group_attributes_from_user().    *
 *                                                                          *
 ****************************************************************************/

void order_sections(void)
{
    if (section_list != NULL)
    {
        if (options.discard_unreferenced)
            discard_unreferenced_segments();

        group_sections_by_name();
        apply_group_attributes_from_user();
        order_groups();
    }
}

/****************************************************************************
 *                                                                          *
 * Function: discard_unreferenced_segments                                  *
 *                                                                          *
 * Purpose : Discard segments that were loaded but never referenced.        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           99-08-19  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void discard_unreferenced_segments(void)
{
    long discarded_size = 0;
    SCNENTRY *scn;

    /*
     * Walk through all sections.
     */
    for (scn = section_list; scn != NULL; scn = scn->next)
    {
        SEGENTRY *seg;
        SEGENTRY *seg_prev;
        SEGENTRY *seg_next;

        /*
         * Walk through all segments.
         */
        for (seg = scn->segs, seg_prev = NULL;
             seg != NULL;
             seg_prev = seg, seg = seg_next)
        {
            SYMENTRY *sym;

            seg_next = seg->next;

#if 0
            /*
             * The segment must contain at least one public symbol,
             * and all public symbols must have a reference count
             * of zero, for the segment to be discarded.
             */
            if (!seg->pubs || seg->keep_last) continue;

            for (sym = seg->pubs; sym != NULL; sym = sym->syms)
                if (sym->flags.referenced) break;
            if (sym) continue;
#else
            /*
             * All public and static symbols must have a reference
             * count of zero, for the segment to be discarded.
             * It's not enough to check the public symbols, since
             * the runtime code might introduce static symbols,
             * for instance SEH blocks, which might have been
             * removed by the previous code. Yuck!
             */
            if (!seg->pubs || seg->keep_last) continue;  /* import junk */

            for (sym = seg->pubs; sym != NULL; sym = sym->syms)
                if (sym->flags.referenced) break;
            if (sym) continue;

            for (sym = seg->stcs; sym != NULL; sym = sym->syms)
                if (sym->flags.referenced) break;
            if (sym) continue;
#endif

            /*
             * Discard the segment.
             */
            if (options.verbose)
            {
                if (discarded_size == 0) printf("\n");
                segment_information(MSG_DISCARDED_SEGMENT, seg);
            }

            if (seg_prev)
                seg_next = seg_prev->next = seg->next;
            else
                seg_next = scn->segs = seg->next;

            discarded_size += seg->size;
            discard_segment(seg);

            seg = seg_prev;
        }
    }

    if (options.verbose)
        printmsg(MSG_DISCARDED_SIZE, discarded_size);
}

/****************************************************************************
 *                                                                          *
 * Function: discard_segment                                                *
 *                                                                          *
 * Purpose : Discard the given segment, free all local memory.              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           99-08-19  Created                                              *
 *           03-09-14  Made public; also used by process_section().         *
 *                                                                          *
 ****************************************************************************/

void discard_segment(SEGENTRY *seg)
{
    SYMENTRY *sym;
    SYMENTRY *sym_next;
    RELENTRY *rel;
    RELENTRY *rel_next;

    /* Free all static symbols */
    for (sym = seg->stcs; sym != NULL; sym = sym_next)
    {
        sym_next = sym->syms;
        my_free(sym->weak_name);
        my_free(sym->name);
        my_free(sym);
    }

    /* Free all relocations */
    for (rel = seg->relocs; rel != NULL; rel = rel_next)
    {
        rel_next = rel->next;
        my_free(rel);
    }

    /* Free linenumbers and raw data */
    my_free(seg->lines);
    my_free(seg->data);
    my_free(seg);
}

/****************************************************************************
 *                                                                          *
 * Function: segment_information                                            *
 *                                                                          *
 * Purpose : Display segment information.                                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           99-08-19  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void segment_information(int msg, SEGENTRY *seg)
{
    char filename[MAX_PATH*2];

    if (seg->mod && seg->mod->lib_file)
        sprintf(filename, "%s(%s)", basename(seg->mod->lib_file->name), seg->mod->obj_file->name);
    else if (seg->mod && seg->mod->obj_file)
        strcpy(filename, seg->mod->obj_file->name);
    else
        strcpy(filename, "*");

    printmsg(msg,
        (seg->pubs) ? seg->pubs->name :
        (seg->stcs) ? seg->stcs->name : "", filename);
}

/****************************************************************************
 *                                                                          *
 * Function: group_sections_by_name                                         *
 *                                                                          *
 * Purpose : Apply the special '$'-name rule.                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           98-12-05  Don't group CodeView debug sections (.debug$?).      *
 *           99-02-13  Changed to dynamic allocation of sort array.         *
 *           99-11-14  New logic to sort .tls and .tls$ together.           *
 *                                                                          *
 ****************************************************************************/

static void group_sections_by_name(void)
{
    SCNENTRY *scn1;
    SCNENTRY *scn2;
    SCNENTRY **sort_array;
    size_t sort_maxcount;
    size_t sort_count;
    size_t i;

    /*
     * Walk through all sections.
     */
    sort_array = NULL; sort_maxcount = 0;
    for (scn1 = section_list; scn1 != NULL; scn1 = scn1->next)
    {
        char grpname1[8+1];
        char *s;
        char *t;
        GRPENTRY *grp;

        if (scn1->segs == NULL)
            scn1->grouped = TRUE;

        /* Skip already processed section */
        if (scn1->grouped) continue;

        /* Get group name, terminated by '$' or NUL */
        for (t = grpname1, s = scn1->name; *s && *s != '$'; s++)
            *t++ = *s;
        *t = '\0';

        /* Ignore ".debug$S", ".debug$T", ".debug$F" and ".debug$P" */
        if (strcmp(grpname1, COFF_DEBUG) == 0) continue;

        sort_count = 0;

        if (sort_count == sort_maxcount)
        {
            sort_maxcount += SORT_ARRAY_INCREMENT;
            sort_array = (SCNENTRY **)my_realloc(sort_array,
                sort_maxcount * sizeof(SCNENTRY *));
        }
        sort_array[sort_count++] = scn1;

        /*
         * Walk through remaining sections.
         */
        for (scn2 = scn1->next; scn2 != NULL; scn2 = scn2->next)
        {
            char grpname2[8+1];

            /* Skip already processed section */
            if (scn2->grouped) continue;

            /* Get group name, terminated by '$' or NUL */
            for (t = grpname2, s = scn2->name; *s && *s != '$'; s++)
                *t++ = *s;
            *t = '\0';

            if (strcmp(grpname1, grpname2) == 0)
            {
                if (sort_count == sort_maxcount)
                {
                    sort_maxcount += SORT_ARRAY_INCREMENT;
                    sort_array = (SCNENTRY **)my_realloc(sort_array,
                        sort_maxcount * sizeof(SCNENTRY *));
                }
                sort_array[sort_count++] = scn2;
            }
        }

        /* Sort the collected sections by name */
        qsort(sort_array, sort_count, sizeof(SCNENTRY *), comp_section_name);

        grp = lookup_group(grpname1, scn1->flags);
        for (i = 0; i < sort_count; i++)
        {
            add_section_to_group(grp, sort_array[i]);
            sort_array[i]->grouped = TRUE;
        }
    }

    my_free(sort_array);
}

/****************************************************************************
 *                                                                          *
 * Function: comp_section_name                                              *
 *                                                                          *
 * Purpose : Compare two section names for the qsort() function.            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int __cdecl comp_section_name(const void *item1, const void *item2)
{
    SCNENTRY *scn1 = *(SCNENTRY **)item1;
    SCNENTRY *scn2 = *(SCNENTRY **)item2;

    return strcmp(scn1->name, scn2->name);
}

/****************************************************************************
 *                                                                          *
 * Function: apply_group_attributes_from_user                               *
 *                                                                          *
 * Purpose : Set user-defined section attributes.                           *
 *                                                                          *
 * Comment : This implementation differs from Microsoft. For example:       *
 *           with MS LINK, the command-line switch "-section:.data,S"       *
 *           will make the .data section shared, but will also create       *
 *           a new (unshared) section called .bss. This linker will         *
 *           make the combined sections (.data + .bss) shared. This         *
 *           is because we have trouble with our alias scheme.              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-09-25  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void apply_group_attributes_from_user(void)
{
    ATTENTRY *att;

    for (att = attrib_list; att != NULL; att = att->next)
    {
        GRPENTRY *grp;

        for (grp = group_list; grp != NULL; grp = grp->next)
            if (strcmp(att->name, grp->name) == 0) break;

        if (grp)
        {
            if (att->memflags != 0)
                grp->flags = (grp->flags & ~COFF_STYP_BASIC_MEMORY) | att->memflags;
            grp->flags |= att->addflags;
        }
        else
        {
            apperror(RCWARNING(ERROR_UNKNOWN_ATTRIB_SECTION), att->name);
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: order_groups                                                   *
 *                                                                          *
 * Purpose : Order groups for the executable image.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void order_groups(void)
{
    GRPENTRY *new_group_list = NULL;

    while (add_group_by_attribute(&new_group_list, COFF_TEXT_FLAGS))
        ;

    while (add_group_by_attribute(&new_group_list, COFF_RDATA_FLAGS))
        ;

    while (add_group_by_attribute(&new_group_list, COFF_DATA_FLAGS))
        ;

    while (add_group_by_attribute(&new_group_list, 0))
        ;

    group_list = new_group_list;
}

/****************************************************************************
 *                                                                          *
 * Function: add_group_by_attribute                                         *
 *                                                                          *
 * Purpose : Add a group with the given flags to the list.                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           01-09-25  Changed set of flags for testing.                    *
 *                                                                          *
 ****************************************************************************/

#define COFF_FLAGS  (COFF_STYP_CONTENTS|COFF_STYP_BASIC_MEMORY|COFF_STYP_DISCARD)
static bool_t add_group_by_attribute(GRPENTRY **new_group_list, ulong_t flags)
{
    GRPENTRY *grp;
    GRPENTRY *grp_prev;

    for (grp = group_list, grp_prev = NULL;
         grp != NULL;
         grp_prev = grp, grp = grp->next)
    {
        /* sort discardable sections last, shared sections in normal place */
        if (flags == 0 || (grp->flags & COFF_FLAGS) == (flags & COFF_FLAGS))
        {
            if (!*new_group_list)
            {
                /* This is the first node. Start the list */
                *new_group_list = grp;
            }
            else
            {
                GRPENTRY *grpT;

                /* Find the end of the list and tack on the new node */
                for (grpT = *new_group_list; grpT->next; grpT = grpT->next)
                    ;

                grpT->next = grp;
            }

            if (grp_prev)
                grp_prev->next = grp->next;
            else
                group_list = grp->next;

            grp->next = NULL;
            return TRUE;
        }
    }

    return FALSE;
}
#undef COFF_FLAGS

