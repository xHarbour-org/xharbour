/****************************************************************************
 *                                                                          *
 * File    : dictary.c                                                      *
 *                                                                          *
 * Purpose : Win32 Linker; dictionary management.                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop
#include <limits.h>

#include "link.h"

/* Makes the code more readable */
#define hash_t  uint_t

#define EXPORT_ARRAY_INCREMENT  128

/* Used by lookup_public() and rename_weak_symbol() */
static SYMENTRY *pub_buckets[137];

/* Static function prototypes */
static ulong_t combine_attributes(const char *, ulong_t, ulong_t);
static bool_t test_section_alias_circle(const char *);
static hash_t hash_name(const char *);

/****************************************************************************
 *                                                                          *
 * Function: lookup_section                                                 *
 *                                                                          *
 * Purpose : Lookup a section by name.                                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           98-12-05  Global CodeView section pointers added.              *
 *           02-02-27  Set default if section content is not specified.     *
 *                                                                          *
 ****************************************************************************/

SCNENTRY *lookup_section(const char *name, ulong_t flags)
{
    SCNENTRY *scn;
    SCNENTRY *scn_last;

    if ((flags & COFF_STYP_CONTENTS) == 0)
    {
        flags |= COFF_STYP_DATA|COFF_STYP_READ|COFF_STYP_WRITE;
        flags &= ~COFF_STYP_EXEC;
        apperror(RCWARNING(ERROR_SECTION_WITHOUT_TYPE), name);
    }

    flags &= ~(COFF_STYP_COMDAT|COFF_STYP_ALIGN);

    for (scn = section_list, scn_last = NULL;
         scn != NULL;
         scn_last = scn, scn = scn->next)
    {
        if (strcmp(name, scn->name) == 0)
        {
            scn->flags = combine_attributes(name, scn->flags, flags);
            return scn;
        }
    }

    /* Didn't find the section; add it */
    scn = (SCNENTRY *)my_alloc(sizeof(SCNENTRY) + strlen(name));

    scn->next = NULL;
    scn->scngrp = NULL;
    scn->segs = scn->seg_last = NULL;
    scn->flags = flags;
    scn->grouped = 0;
    strcpy(scn->name, name);

    if (!scn_last)
    {
        /* This is the first node. Start the list */
        section_list = scn;
    }
    else
    {
        /* Append the new node to the list */
        scn_last->next = scn;
    }

    /* Update global CodeView section pointers */
    if (strcmp(scn->name, COFF_DEBUGS) == 0) scn_sym = scn;
    if (strcmp(scn->name, COFF_DEBUGT) == 0) scn_type = scn;
    if (strcmp(scn->name, COFF_DEBUGF) == 0) scn_fpo = scn;

    return scn;
}

/****************************************************************************
 *                                                                          *
 * Function: combine_attributes                                             *
 *                                                                          *
 * Purpose : Combine attributes for two sections; report inconsistencies.   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static ulong_t combine_attributes(const char *name, ulong_t c1, ulong_t c2)
{
    if (c1 & COFF_STYP_DATA)
    {
        if (c2 & COFF_STYP_BSS)
            c2 = ((c2 & ~COFF_STYP_CONTENTS)|COFF_STYP_DATA);
    }
    else if (c1 & COFF_STYP_BSS)
    {
        if (c2 & COFF_STYP_DATA)
            c1 = ((c1 & ~COFF_STYP_CONTENTS)|COFF_STYP_DATA);
    }

    if ((c1 & (COFF_STYP_MEMORY|COFF_STYP_CONTENTS)) !=
        (c2 & (COFF_STYP_MEMORY|COFF_STYP_CONTENTS)))
    {
        apperror(RCWARNING(ERROR_SECTION_ATTR_DIFF), name, c1, c2);
    }

    return c1;
}

/****************************************************************************
 *                                                                          *
 * Function: add_segment_to_section                                         *
 *                                                                          *
 * Purpose : Add a segment (object section) to a section.                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           98-12-30  Made faster with new SCNENTRY field: seg_last.       *
 *                                                                          *
 ****************************************************************************/

SEGENTRY *add_segment_to_section(SCNENTRY *scn)
{
    SEGENTRY *seg;

    seg = (SEGENTRY *)my_alloc(sizeof(SEGENTRY));

    seg->next = NULL;
    seg->pubs = NULL;
    seg->stcs = NULL;
    seg->mod = NULL;
    seg->vaddr = 0;
    seg->alignment = 0;
    seg->size = 0;
    seg->data = NULL;
    seg->relocs = NULL;
    seg->lines = NULL;
    seg->nlines = 0;
    seg->keep_last = FALSE;

    if (scn->seg_last != NULL)
        scn->seg_last = scn->seg_last->next = seg;
    else
        scn->seg_last = scn->segs = seg;

    return seg;
}

/****************************************************************************
 *                                                                          *
 * Function: add_segment_to_import_section                                  *
 *                                                                          *
 * Purpose : Add a segment (object section) to a import section.            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

SEGENTRY *add_segment_to_import_section(SCNENTRY *scn, FILEINFO *lib_file)
{
    SEGENTRY *seg;

    seg = (SEGENTRY *)my_alloc(sizeof(SEGENTRY));

    seg->next = NULL;
    seg->pubs = NULL;
    seg->stcs = NULL;
    seg->mod = NULL;
    seg->vaddr = 0;
    seg->alignment = 0;
    seg->size = 0;
    seg->data = NULL;
    seg->relocs = NULL;
    seg->lines = NULL;
    seg->nlines = 0;
    seg->keep_last = FALSE;

    if (!scn->segs)
    {
        /* This is the first node. Start the list */
        scn->segs = seg;
    }
    else
    {
        SEGENTRY *segT;

        /* Group segments by import archive */
        for (segT = scn->segs; segT->next != NULL; segT = segT->next)
        {
            if (lib_file == segT->mod->lib_file &&
               (lib_file != segT->next->mod->lib_file || segT->next->keep_last))
                break;
        }

        seg->next = segT->next;
        segT->next = seg;
    }

    return seg;
}

/****************************************************************************
 *                                                                          *
 * Function: add_relocation_to_segment                                      *
 *                                                                          *
 * Purpose : Add a relocation entry to the segment.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

RELENTRY *add_relocation_to_segment(SEGENTRY *seg)
{
    RELENTRY *rel;

    rel = (RELENTRY *)my_alloc(sizeof(RELENTRY));

    rel->next = NULL;
    rel->sym = NULL;
    rel->offset = 0;
    rel->type = 0;

    if (!options.mapfile_fixups)
    {
        rel->next = seg->relocs;
        seg->relocs = rel;
    }
    else
    {
        /*
         * The above code will always work, and is *much* faster, but
         * this makes the FIXUP section (in the MAP file) look nice.
         */
        if (!seg->relocs)
        {
            /* This is the first node. Start the list */
            seg->relocs = rel;
        }
        else
        {
            RELENTRY *relT;

            /* Find the end of the list and tack on the new node */
            for (relT = seg->relocs; relT->next != NULL; relT = relT->next)
                ;

            relT->next = rel;
        }
    }

    return rel;
}

/****************************************************************************
 *                                                                          *
 * Function: lookup_group                                                   *
 *                                                                          *
 * Purpose : Lookup a section group by name.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

GRPENTRY *lookup_group(const char *name, ulong_t flags)
{
    GRPENTRY *grp;
    GRPENTRY *grp_last;
    RENENTRY *ren;

    /* Look for the group in the alias list */
    for (ren = alias_list; ren != NULL; ren = ren->next)
    {
        if (strcmp(name, ren->from_name) == 0)
            return lookup_group(ren->to_name, flags);
    }

    /* Look for the group name */
    for (grp = group_list, grp_last = NULL;
         grp != NULL;
         grp_last = grp, grp = grp->next)
    {
        if (strcmp(name, grp->name) == 0)
        {
            grp->flags = combine_attributes(name, grp->flags, flags);
            return grp;
        }
    }

    /* Didn't find the group; add it */
    grp = (GRPENTRY *)my_alloc(sizeof(GRPENTRY) + strlen(name));

    grp->next = NULL;
    grp->scngrp = NULL;
    grp->flags = flags;
    grp->vaddr = 0;
    grp->scnum = 0;
    strcpy(grp->name, name);

    if (!grp_last)
    {
        /* This is the first node. Start the list */
        group_list = grp;
    }
    else
    {
        /* Append the new node to the list */
        grp_last->next = grp;
    }

    return grp;
}

/****************************************************************************
 *                                                                          *
 * Function: add_section_to_group                                           *
 *                                                                          *
 * Purpose : Add a section to a section group.                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           98-12-09  Rewritten; always keep uninitialized data last.      *
 *                                                                          *
 ****************************************************************************/

void add_section_to_group(GRPENTRY *grp, SCNENTRY *scn)
{
    if (!grp->scngrp)
    {
        /* This is the first node. Start the list */
        grp->scngrp = scn;
    }
    else
    {
        SCNENTRY *scnT;
        SCNENTRY *scn_prev;

        for (scnT = grp->scngrp, scn_prev = NULL;
             scnT != NULL;
             scn_prev = scnT, scnT = scnT->scngrp)
        {
            /* Always keep uninitialized data last, in the group */
            if ((scnT->flags & COFF_STYP_BSS) && (scn->flags & COFF_STYP_DATA))
                break;
        }

        scn->scngrp = scnT;

        if (scn_prev)
            scn_prev->scngrp = scn;
        else
            grp->scngrp = scn;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: lookup_section_alias                                           *
 *                                                                          *
 * Purpose : Lookup a section alias.                                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           99-04-02  Better check with test_section_alias_circle().       *
 *                                                                          *
 ****************************************************************************/

RENENTRY *lookup_section_alias(const char *from_name, const char *to_name)
{
    RENENTRY *ren;
    RENENTRY *ren_last;

    for (ren = alias_list, ren_last = NULL;
         ren != NULL;
         ren_last = ren, ren = ren->next)
    {
        if (strcmp(from_name, ren->from_name) == 0)
            return ren;
    }

    /* Didn't find the name; add it */
    ren = (RENENTRY *)my_alloc(sizeof(RENENTRY));

    ren->next = NULL;
    ren->from_name = tstrcpy(from_name);
    ren->to_name = tstrcpy(to_name);
    ren->visited = FALSE;

    if (!ren_last)
    {
        /* This is the first node. Start the list */
        alias_list = ren;
    }
    else
    {
        /* Append the new node to the list */
        ren_last->next = ren;
    }

    /* Check that the node doesn't cause a circular link */
    if (test_section_alias_circle(from_name))
    {
        if (!ren_last)
        {
            /* This is the first node. Clear the list */
            alias_list = NULL;
        }
        else
        {
            /* Remove the new node from the list */
            ren_last->next = NULL;
        }

        my_free(ren->from_name);
        my_free(ren->to_name);
        my_free(ren);
        ren = NULL;
    }

    return ren;
}

/****************************************************************************
 *                                                                          *
 * Function: test_section_alias_circle                                      *
 *                                                                          *
 * Purpose : Look for a circular alias link.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           99-04-02  Created                                              *
 *                                                                          *
 ****************************************************************************/

static bool_t test_section_alias_circle(const char *name)
{
    RENENTRY *ren;

    for (ren = alias_list; ren != NULL; ren = ren->next)
    {
        if (strcmp(name, ren->from_name) == 0)
        {
            bool_t visited = ren->visited;
            if (!visited)
            {
                ren->visited = TRUE;
                visited = test_section_alias_circle(ren->to_name);
                ren->visited = FALSE;
            }
            return visited;
        }
    }

    return FALSE;
}

/****************************************************************************
 *                                                                          *
 * Function: lookup_section_attributes                                      *
 *                                                                          *
 * Purpose : Lookup attributes for a section by name.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-09-25  Created                                              *
 *                                                                          *
 ****************************************************************************/

ATTENTRY *lookup_section_attributes(const char *name)
{
    ATTENTRY *att;
    ATTENTRY *att_last;

    for (att = attrib_list, att_last = NULL;
         att != NULL;
         att_last = att, att = att->next)
    {
        if (strcmp(name, att->name) == 0)
            return att;
    }

    /* Didn't find the section; add it */
    att = (ATTENTRY *)my_alloc(sizeof(ATTENTRY) + strlen(name));

    att->next = NULL;
    att->memflags = att->addflags = 0;
    strcpy(att->name, name);

    if (!att_last)
    {
        /* This is the first node. Start the list */
        attrib_list = att;
    }
    else
    {
        /* Append the new node to the list */
        att_last->next = att;
    }

    return att;
}

/****************************************************************************
 *                                                                          *
 * Function: lookup_export                                                  *
 *                                                                          *
 * Purpose : Lookup a exported symbol.                                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

EXPENTRY *lookup_export(const char *name, const char *symname)
{
    int lo = 0;
    int hi = export_count-1;
    EXPENTRY *exp;

    /*
     * Search for the given symbol.
     */
    while (hi >= lo)
    {
        int this = (lo + hi) / 2;
        int cond = strcmp(name, export_list[this]->name);

        if (cond < 0)
            hi = this-1;
        else if (cond > 0)
            lo = this+1;
        else
            return export_list[this];
    }

    /*
     * Didn't find the symbol; add it (sorted).
     */
    if (export_count == export_maxcount)
    {
        export_maxcount += EXPORT_ARRAY_INCREMENT;
        export_list = (EXPENTRY **)my_realloc(export_list, export_maxcount * sizeof(EXPENTRY *));
    }

    memmove(&export_list[lo+1], &export_list[lo], (export_count-lo) * sizeof(EXPENTRY *));

    exp = (EXPENTRY *)my_alloc(sizeof(EXPENTRY));
    exp->next = NULL;
    exp->name = tstrcpy(name);
    exp->sym = NULL;
    exp->offset = 0;
    exp->ordinal = 0;

    export_list[lo] = exp;
    export_count++;

    /*
     * A real live symbol to export?
     */
    if (symname != NULL)
    {
        SYMENTRY *pub;

        pub = lookup_public(symname);
        pub->flags.referenced = TRUE;

        if (pub->type == unused)
        {
            pub->weak_name = tstrcat("_", symname);
            pub->type = unresolved_weak;
            unresolved_count++;
        }

        exp->sym = pub;
    }

    return exp;
}

/****************************************************************************
 *                                                                          *
 * Function: lookup_public                                                  *
 *                                                                          *
 * Purpose : Lookup a public symbol.                                        *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           98-03-21  Rewritten using a hash table; *much* faster.         *
 *           03-06-30  Call to unmangle_name() added.                       *
 *                                                                          *
 ****************************************************************************/

SYMENTRY *lookup_public(const char *name)
{
    static SYMENTRY *pub_last = 0;
    SYMENTRY *pub;
    hash_t hash;

    /*
     * Unmangle C++ names, if so requested.
     */
    if (options.unmangle_names)
        name = unmangle_name(name);

    /*
     * Use hash value to locate the symbol.
     */
    hash = hash_name(name) % NELEMS(pub_buckets);
    for (pub = pub_buckets[hash]; pub != NULL; pub = pub->next_bucket)
    {
        if (strcmp(name, pub->name) == 0)
            return pub;
    }

    /*
     * Didn't find the symbol. Add it.
     */
    pub = (SYMENTRY *)my_alloc(sizeof(SYMENTRY));

    pub->next_bucket = pub_buckets[hash];
    pub_buckets[hash] = pub;

    pub->next = NULL;
    pub->syms = NULL;
    pub->name = tstrcpy(name);
    pub->weak_name = NULL;
    pub->def_mod = NULL;
    pub->dec_mod = NULL;
    pub->value = 0;
    pub->checksum = 0;
    pub->comseg = NULL;
    pub->type = unused;
#ifdef PRERELEASE
    pub->flags.relocated = FALSE;
#endif
    pub->flags.absolute = pub->flags.common = pub->flags.internal =
    pub->flags.function = pub->flags.referenced =
    pub->flags.nosearch = FALSE;
    pub->scnum = 0;
    pub->comdat = 0;
    pub->class = 0;

    if (pub_last != NULL)
        pub_last = pub_last->next = pub;
    else
        pub_last = public_list = pub;

#ifdef PRERELEASE
    public_count++;
#endif

    return pub;
}

/****************************************************************************
 *                                                                          *
 * Function: dump_public_table                                              *
 *                                                                          *
 ****************************************************************************/

#ifdef PRERELEASE
void dump_public_table(void)
{
    int i;

    for (i = 0; i < NELEMS(pub_buckets); i++)
    {
        SYMENTRY *pub;
        int entries;

        entries = 0;
        for (pub = pub_buckets[i]; pub != NULL; pub = pub->next_bucket)
            entries++;

        printf("bucket %2X: %.*s\n", i, entries,
            "oooooooooo" "oooooooooo" "oooooooooo" "oooooooooo" "oooooooooo"
            "oooooooooo" "oooooooooo" "oooooooooo" "oooooooooo" "oooooooooo"
            "oooooooooo" "oooooooooo" "oooooooooo" "oooooooooo" "oooooooooo"
            "oooooooooo" "oooooooooo" "oooooooooo" "oooooooooo" "oooooooooo"
            "oooooooooo" "oooooooooo" "oooooooooo" "oooooooooo" "oooooooooo");
    }

    printf("\n");
}
#endif

/****************************************************************************
 *                                                                          *
 * Function: lookup_static                                                  *
 *                                                                          *
 * Purpose : Lookup a static symbol (defined in the given segment).         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

SYMENTRY *lookup_static(SEGENTRY *seg, const char *name)
{
    SYMENTRY *sym;
    SYMENTRY *sym_last;
    long checksum;

    /* Use hash value to speed up symbol search */
    checksum = hash_name(name);
    for (sym = seg->stcs, sym_last = NULL;
         sym != NULL;
         sym_last = sym, sym = sym->next)
    {
        if (checksum == sym->checksum && strcmp(name, sym->name) == 0)
            return sym;
    }

    /* Didn't find the symbol; add it */
    sym = (SYMENTRY *)my_alloc(sizeof(SYMENTRY));

    sym->next_bucket = NULL;
    sym->next = NULL;
    sym->syms = NULL;
    sym->name = tstrcpy(name);
    sym->weak_name = NULL;
    sym->value = 0;
    sym->checksum = checksum;
    sym->comseg = NULL;
    sym->type = unused;
#ifdef PRERELEASE
    sym->flags.relocated = FALSE;
#endif
    sym->flags.absolute = sym->flags.common = sym->flags.internal =
    sym->flags.function = sym->flags.referenced =
    sym->flags.nosearch = FALSE;
    sym->scnum = 0;
    sym->comdat = 0;
    sym->class = 0;

    if (!sym_last)
    {
        /* This is the first node. Start the list */
        seg->stcs = sym;
    }
    else
    {
        /* Append the new node to the list */
        sym_last->next = sym_last->syms = sym;
    }

#ifdef PRERELEASE
    static_count++;
#endif

    return sym;
}

/****************************************************************************
 *                                                                          *
 * Function: add_public_to_segment                                          *
 *                                                                          *
 * Purpose : Associate the symbol with the segment (where it is defined).   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

void add_public_to_segment(SEGENTRY *seg, SYMENTRY *pub)
{
    if (!seg->pubs)
    {
        /* This is the first node. Start the list */
        seg->pubs = pub;
    }
    else
    {
        SYMENTRY *sym;

        /* Find the end of the list and tack on the new node */
        for (sym = seg->pubs; sym->syms && sym != pub; sym = sym->syms)
            ;

        /* Avoid repeating ourself */
        if (sym != pub) sym->syms = pub;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: rename_weak_symbol                                             *
 *                                                                          *
 * Purpose : Change the symbol name for a weak external from it's           *
 *           original name to the alternate name.                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           98-03-21  Rewritten using the new hash table. Tricky Dick!     *
 *                                                                          *
 ****************************************************************************/

void rename_weak_symbol(SYMENTRY *pub_weak)
{
    SYMENTRY *pub_prev;
    SYMENTRY *pub;
    hash_t hash;

    /*
     * Use hash value to locate the symbol.
     */
    hash = hash_name(pub_weak->weak_name) % NELEMS(pub_buckets);
    for (pub = pub_buckets[hash]; pub != NULL; pub = pub->next_bucket)
    {
        if (strcmp(pub_weak->weak_name, pub->name) == 0)
        {
            /*
             * Found a symbol with the alternate name. Change all
             * occurencies of the weak node to this one.
             */
            replace_public_node(pub_weak, pub);

            /* Very important to update this!! */
            pub->flags.referenced = pub_weak->flags.referenced;
            pub_weak->flags.referenced = FALSE;

            pub_weak->type = unused;

            unresolved_count--;
            return;
        }
    }

    /*
     * Didn't find a symbol with the alternate name. Change the name
     * of this symbol and turn it into a normal, unresolved, one.
     * This involves updating the hash table. Yuck!
     */
    hash = hash_name(pub_weak->name) % NELEMS(pub_buckets);
    for (pub = pub_buckets[hash], pub_prev = NULL;
         pub != NULL;
         pub_prev = pub, pub = pub->next_bucket)
    {
        if (pub == pub_weak)
        {
            if (pub_prev)
                pub_prev->next_bucket = pub->next_bucket;
            else
                pub_buckets[hash] = pub->next_bucket;
            break;
        }
    }

    SWAP(char *, pub_weak->name, pub_weak->weak_name);
    pub_weak->type = unresolved;

    hash = hash_name(pub_weak->name) % NELEMS(pub_buckets);
    pub_weak->next_bucket = pub_buckets[hash];
    pub_buckets[hash] = pub_weak;
}

/****************************************************************************
 *                                                                          *
 * Function: replace_weak_symbol                                            *
 *                                                                          *
 * Purpose : Change all occurencies of a weak symbol node to another one.   *
 *                                                                          *
 * Comment : The weak symbol is unresolved (that's why we are here!),       *
 *           so we can ignore SYMENTRY ptrs from SEGENTRY nodes.            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

void replace_public_node(SYMENTRY *pub_weak, SYMENTRY *pub)
{
    SCNENTRY *scn;
    size_t i;

    /*
     * Relocation entries contains symbol nodes...
     */
    for (scn = section_list; scn != NULL; scn = scn->next)
    {
        SEGENTRY *seg;

        for (seg = scn->segs; seg != NULL; seg = seg->next)
        {
            RELENTRY *rel;

            for (rel = seg->relocs; rel != NULL; rel = rel->next)
                if (rel->sym == pub_weak) rel->sym = pub;
        }
    }

    /*
     * Export entries also contains symbol nodes...
     */
    for (i = 0; i < export_count; i++)
    {
        EXPENTRY *exp = export_list[i];

        if (exp->sym == pub_weak)
            exp->sym = pub;
    }

    /*
     * Don't forget the entry point symbol...
     */
    if (pub_entry == pub_weak)
        pub_entry = pub;
}

/****************************************************************************
 *                                                                          *
 * Function: lookup_file                                                    *
 *                                                                          *
 * Purpose : Lookup a filename in the given list.                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

FILEINFO *lookup_file(FILEINFO **list, const char *name)
{
    FILEINFO *file;
    FILEINFO *file_last;

    for (file = *list, file_last = NULL;
         file != NULL;
         file_last = file, file = file->next)
    {
        if (strcmp(name, file->name) == 0)
            return file;
    }

    /* Didn't find the file; add it */
    file = (FILEINFO *)my_alloc(sizeof(FILEINFO) + strlen(name));

    file->next = NULL;
    file->hf = NULL;
    file->hmap = NULL;
    file->base = NULL;
    file->size = 0;
    file->delay_dll = NULL;
    strcpy(file->name, name);

    if (!file_last)
    {
        /* This is the first node. Start the list */
        *list = file;
    }
    else
    {
        /* Append the new node to the list */
        file_last->next = file;
    }

    return file;
}

/****************************************************************************
 *                                                                          *
 * Function: remove_file                                                    *
 *                                                                          *
 * Purpose : Remove a filename from the given list.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

void remove_file(FILEINFO **list, FILEINFO *file_free)
{
    FILEINFO *file;
    FILEINFO *file_prev;

    /*
     * Find the existing node and get it's previous link.
     */
    for (file = *list, file_prev = NULL;
         file && file != file_free;
         file_prev = file, file = file->next)
        ;

    if (file)
    {
        if (file_prev)
            file_prev->next = file_free->next;
        else
            *list = file_free->next;

        my_free(file_free);
    }
}

/****************************************************************************
 *                                                                          *
 * Function: lookup_delay_import_module                                     *
 *                                                                          *
 * Purpose : Lookup a filename in the list of delay loaded modules.         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           99-02-13  Created                                              *
 *           00-11-16  Reject this feature for ARM targets.                 *
 *                                                                          *
 ****************************************************************************/

DLLENTRY *lookup_delay_import_module(const char *name, bool_t install)
{
    DLLENTRY *dll;
    DLLENTRY *dll_last;

    /* This is not supported on ARM targets */
    if (delay_list != NULL && options.machine == MACHINE_ARM)
    {
        apperror(RCWARNING(ERROR_CANT_DELAY_ARM));
        return delay_list = NULL;
    }

    for (dll = delay_list, dll_last = NULL;
         dll != NULL;
         dll_last = dll, dll = dll->next)
    {
        if (_stricmp(name, dll->name) == 0)
            return dll;
    }

    if (install)
    {
        /* Need kernel to *implement* the delayed loading! */
        if (_stricmp(name, "kernel32.dll") == 0)
        {
            apperror(RCWARNING(ERROR_IGNORED_DELAY_MODULE), name);
            return NULL;
        }

        /* Didn't find the module; add it */
        dll = (DLLENTRY *)my_alloc(sizeof(DLLENTRY) + strlen(name));

        dll->next = NULL;
        dll->flags.referenced = FALSE;
        strcpy(dll->name, name);

        if (!dll_last)
        {
            /* This is the first node. Start the list */
            delay_list = dll;
        }
        else
        {
            /* Append the new node to the list */
            dll_last->next = dll;
        }
    }

    return dll;
}

/****************************************************************************
 *                                                                          *
 * Function: lookup_libpath                                                 *
 *                                                                          *
 * Purpose : Lookup a library path.                                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           03-06-17  Created                                              *
 *                                                                          *
 ****************************************************************************/

LIBENTRY *lookup_libpath(const char *name)
{
    LIBENTRY *lib_path;
    LIBENTRY *lib_path_last;

    for (lib_path = lib_path_list, lib_path_last = NULL;
         lib_path != NULL;
         lib_path_last = lib_path, lib_path = lib_path->next)
    {
        if (_stricmp(name, lib_path->name) == 0)
            return lib_path;
    }

    /* Didn't find the path; add it */
    lib_path = (LIBENTRY *)my_alloc(sizeof(LIBENTRY) + strlen(name));

    lib_path->next = NULL;
    strcpy(lib_path->name, name);

    if (!lib_path_last)
    {
        /* This is the first node. Start the list */
        lib_path_list = lib_path;
    }
    else
    {
        /* Append the new node to the list */
        lib_path_last->next = lib_path;
    }

    return lib_path;
}

/****************************************************************************
 *                                                                          *
 * Function: lookup_name                                                    *
 *                                                                          *
 * Purpose : Lookup a generic filename.                                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           98-03-21  Rewritten using a hash table.                        *
 *                                                                          *
 ****************************************************************************/

HASHNODE *lookup_name(const char *name)
{
    static HASHNODE *node_buckets[137];
    HASHNODE *node;
    hash_t hash;

    /*
     * Use hash value to locate the name.
     */
    hash = hash_name(name) % NELEMS(node_buckets);
    for (node = node_buckets[hash]; node != NULL; node = node->next_bucket)
    {
        if (strcmp(name, node->name) == 0)
            return node;
    }

    /*
     * Didn't find the name; add it.
     */
    node = (HASHNODE *)my_alloc(sizeof(HASHNODE) + strlen(name));

    node->next_bucket = node_buckets[hash];
    node_buckets[hash] = node;

    strcpy(node->name, name);

    return node;
}

/****************************************************************************
 *                                                                          *
 * Function: add_module_to_list                                             *
 *                                                                          *
 * Purpose : Add a new module to the global list.                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-11-25  Created                                              *
 *                                                                          *
 ****************************************************************************/

MODENTRY *add_module_to_list(void)
{
    static short modnum = 0;
    MODENTRY *mod;

    mod = (MODENTRY *)my_alloc(sizeof(MODENTRY));

    mod->next = NULL;
    mod->lib_file = NULL;
    mod->obj_file = NULL;
    mod->src_file = NULL;
    mod->modnum = ++modnum;  /* it's assumed we never free a module... */

    if (!module_list)
    {
        /* This is the first node. Start the list */
        module_list = mod;
    }
    else
    {
        MODENTRY *modT;

        /* Find the end of the list and tack on the new node */
        for (modT = module_list; modT->next != NULL; modT = modT->next)
            ;

        modT->next = mod;
    }

    return mod;
}

/****************************************************************************
 *                                                                          *
 * Function: hash_name                                                      *
 *                                                                          *
 * Purpose : Transform a character string into a hash value.                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           98-03-21  Rewritten                                            *
 *           98-08-08  Changed to PJW's hashing algorithm (DDJ apr 96).     *
 *                                                                          *
 ****************************************************************************/

#define BITS_IN_HASH    (sizeof(hash_t) * CHAR_BIT)
#define THREE_QUARTERS  ((hash_t)((BITS_IN_HASH * 3) / 4))
#define ONE_EIGHT       ((hash_t)((BITS_IN_HASH) / 8))
#define HIGH_BITS       (~((hash_t)(~0) >> ONE_EIGHT))

#if 1
static __inline hash_t hash_name(const char *name)
{
    hash_t hash;

    hash = 0;
    while (*name != '\0')
    {
        hash_t temp;

        hash = (hash << ONE_EIGHT) + *name++;
        if ((temp = hash & HIGH_BITS) != 0)
            hash = (hash ^ (temp >> THREE_QUARTERS)) & ~HIGH_BITS;
    }

    return hash;
}
#else
static __inline hash_t hash_name(const char *name)
{
    hash_t hash;

    /* KISS: just sum up the characters */
    for (hash = 0; *name != '\0'; hash += *name++)
        ;

    return hash;
}
#endif

/****************************************************************************
 *                                                                          *
 * Function: unmangle_name                                                  *
 *                                                                          *
 * Purpose : Unmangle C++ names (if extern "C" is impossible).              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           03-06-30  Created                                              *
 *           03-09-03  Added fabs-fabsf special hack.                       *
 *                                                                          *
 ****************************************************************************/

/*
int __stdcall test(int c)                   ==>  ?test@@YGHH@Z
void __stdcall apskip(__int64 a, messa b)   ==>  ?apskip@@YGX_JU_messa@@@Z
void __stdcall vv(void)                     ==>  ?vv@@YGXXZ

void __cdecl sc(signed char a) {}           ==>  ?sc@@YAXC@Z
void __cdecl c(char a) {}                   ==>  ?c@@YAXD@Z
void __cdecl uc(unsigned char a) {}         ==>  ?uc@@YAXE@Z

void __cdecl ss(signed short a) {}          ==>  ?ss@@YAXF@Z
void __cdecl s(short a) {}                  ==>  ?s@@YAXF@Z
void __cdecl us(unsigned short a) {}        ==>  ?us@@YAXG@Z

void __cdecl si(signed int a) {}            ==>  ?si@@YAXH@Z
void __cdecl i(int a) {}                    ==>  ?i@@YAXH@Z
void __cdecl ui(unsigned int a) {}          ==>  ?ui@@YAXI@Z

void __cdecl sl(signed long a) {}           ==>  ?sl@@YAXJ@Z
void __cdecl l(long a) {}                   ==>  ?l@@YAXJ@Z
void __cdecl ul(unsigned long a) {}         ==>  ?ul@@YAXK@Z

void __cdecl sl(signed __int64 a) {}        ==>  ?sl@@YAX_J@Z
void __cdecl l(__int64 a) {}                ==>  ?l@@YAX_J@Z
void __cdecl ul(unsigned __int64 a) {}      ==>  ?ul@@YAX_K@Z

void __cdecl f(float a) {}                  ==>  ?f@@YAXM@Z
void __cdecl d(double a) {}                 ==>  ?d@@YAXN@Z

*/

const char *unmangle_name(const char *name)  /* called from archive.c */
{
    static char tname[1024];
    const char *p;
    size_t nlen;

    if (*name == '?')  /* MSVC++ 6.0 */
    {
        for (p = name + 1; *p != '\0' && *p != '@'; p++)
            ;

        if (*p == '@')  /* possibly mangled function */
        {
            nlen = min(p - name - 1, 1000);

            if (*++p == '@' && *++p == 'Y')  /* definitely mangled function */
            {
                if (*++p == 'G')  /* stdcall */
                {
                    ulong_t args;

                    /* skip return type and count argument size */
                    for (args = 0, p += 2; *p != '\0' && *p != '@' && *p != 'Z'; p++)
                    {
                        switch (*p)
                        {
                            case '_':  /* signed __int64 (_J), unsigned __int64 (_K) */
                            case 'C':  /* signed char */
                            case 'D':  /* char */
                            case 'E':  /* unsigned char */
                            case 'F':  /* signed short; short */
                            case 'G':  /* unsigned short */
                            case 'H':  /* signed int; int */
                            case 'I':  /* unsigned int */
                            case 'J':  /* signed long; long */
                            case 'K':  /* unsigned long */
                            case 'M':  /* float */
                            case 'Q':  /* array */
                                args += 4;
                                break;

                            case 'N':  /* double */
                                args += 8;
                                break;

                            case 'X':  /* void */
                                break;

                            case 'P':  /* pointer */
                                if (*++p == 'A')
                                {
                                    if (*++p == 'U')
                                    {
                                        while (*++p != '\0' && *p != '@')
                                            ;

                                        while (*p == '@')
                                            p++;
                                    }
                                    p--;

                                    args += 4;
                                    break;
                                }
                                /* fall through */

                            default:  /* struct/union/class ... */
                                return name;
                        }
                    }

                    sprintf(tname, "_%.*s@%u", nlen, name+1, args);
                    return tname;
                }
                else if (*p == 'A')  /* cdecl */
                {
                    if (strcmp(name, "?fabs@@YAMM@Z") == 0) return "_?fabs";  /* 03-09-03 */
                    sprintf(tname, "_%.*s", nlen, name+1);
                    return tname;
                }
            }
        }
    }

    return name;
}
