/****************************************************************************
 *                                                                          *
 * File    : fixup.c                                                        *
 *                                                                          *
 * Purpose : Win32 Linker; relocation management.                           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           00-11-16  Function apply_arm_relocation() added.               *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "link.h"

#define RELOC_ARRAY_INCREMENT   1024

#define TYPE_OFFSET(t,p)        (short)(((t)<<12)|(p))
#define TYPE_OFFSET_HIGHLOW(p)  TYPE_OFFSET(COFF_B_HIGHLOW, (p))
#define TYPE_OFFSET_NULL        TYPE_OFFSET(COFF_B_ABSOLUTE, 0)

#ifdef PRERELEASE
#define ASSERT_SINGLE_RELOCATION(sym) \
{ \
    if ((sym)->flags.relocated) \
        printf("symbol already relocated: %s\n", (sym)->name); \
    (sym)->flags.relocated = TRUE; \
}
#else
#define ASSERT_SINGLE_RELOCATION(sym)
#endif

/* Locals */
static addr_t *relocs = NULL;
static size_t reloc_count = 0;
static size_t reloc_maxcount = 0;

/* Static function prototypes */
static __inline void apply_x86_relocation(void *, RELENTRY *, addr_t);
static __inline void apply_arm_relocation(void *, RELENTRY *, addr_t);
static __inline addr_t group_address_from_index(short);
static void add_base_relocation(addr_t);
static int __cdecl comp_address(const void *, const void *);
static SEGENTRY *alloc_reloc_section(void);
static void update_reloc_section(SEGENTRY *);
static size_t sizeof_reloc_section(void);

/****************************************************************************
 *                                                                          *
 * Function: fixup_relocations                                              *
 *                                                                          *
 * Purpose : Calculate final symbol values and apply relocations.           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           98-12-05  Update new field in GRPENTRY, SYMENTRY: scnum.       *
 *           98-12-05  Changed relocation loop to include debug sections.   *
 *           00-11-16  Support for ARM machine added.                       *
 *           02-02-27  Detect relocation attempts in BSS section.           *
 *                                                                          *
 ****************************************************************************/

void fixup_relocations(void)
{
    SEGENTRY *seg_reloc;
    GRPENTRY *grp;
    SCNENTRY *scn;
    addr_t vaddr;
    short scnum;

    /*
     * Allocate section for base relocations (before address assignment).
     */
    if (!options.fixed)
        seg_reloc = alloc_reloc_section();
    else
        seg_reloc = NULL;

    /*
     * Update all symbol addresses.
     */
    scnum = 0; vaddr = stub_size;  /* guard page */
    for (grp = group_list; grp != NULL; grp = grp->next)
    {
        SCNENTRY *scn;

        grp->vaddr = vaddr;
        grp->scnum = ++scnum;

        for (scn = grp->scngrp; scn != NULL; scn = scn->scngrp)
        {
            SEGENTRY *seg;

            for (seg = scn->segs; seg != NULL; seg = seg->next)
            {
                long alignment;
                SYMENTRY *sym;

                /* Convert alignment into bytes to achieve it */
                alignment = (vaddr % seg->alignment);
                if (alignment != 0)
                    seg->alignment -= alignment;
                else
                    seg->alignment = 0;

                /* Align the image address */
                vaddr += seg->alignment;

                seg->vaddr = vaddr;

                /* Process public symbols defined here... */
                for (sym = seg->pubs; sym != NULL; sym = sym->syms)
                {
                    sym->scnum = scnum;
                    if (!sym->flags.absolute)
                    {
                        ASSERT_SINGLE_RELOCATION(sym);
                        sym->value += vaddr;
                    }
                }

                /* Process static symbols defined here... */
                for (sym = seg->stcs; sym != NULL; sym = sym->syms)
                {
                    sym->scnum = scnum;
                    if (!sym->flags.absolute)
                    {
                        ASSERT_SINGLE_RELOCATION(sym);
                        sym->value += vaddr;
                    }
                }

                /* Bump the image address */
                vaddr += seg->size;
            }
        }

        /* Align the image address (new section) */
        vaddr = ROUNDUP(vaddr, options.section_alignment);
    }

    /*
     * Apply relocations to *all* sections, including any
     * debug sections, which are not associated with a group
     * (i.e. not mapped into the address space).
     */
    for (scn = section_list; scn != NULL; scn = scn->next)
    {
        SEGENTRY *seg;

        for (seg = scn->segs; seg != NULL; seg = seg->next)
        {
            RELENTRY *rel;

            for (rel = seg->relocs; rel != NULL; rel = rel->next)
            {
                void *raddr = (char *)seg->data + rel->offset;
                addr_t vaddr = seg->vaddr + rel->offset;

                if (seg->data == NULL)
                    apperror(RCFATAL(ERROR_RELOC_IN_BSS_SECTION));

                if (options.machine == MACHINE_X86)
                    apply_x86_relocation(raddr, rel, vaddr);
                else if (options.machine == MACHINE_ARM)
                    apply_arm_relocation(raddr, rel, vaddr);
                else
                    apperror(RCFATAL(ERROR_UNKNOWN_RELOC_MACHINE));
            }
        }
    }

    /*
     * Process base relocations.
     */
    if (seg_reloc != NULL /* && reloc_count != 0 */)
    {
        qsort(relocs, reloc_count, sizeof(addr_t), comp_address);
        update_reloc_section(seg_reloc);
    }

    my_free(relocs);
}

/****************************************************************************
 *                                                                          *
 * Function: apply_x86_relocation                                           *
 *                                                                          *
 * Purpose : Update a relocation entry; Intel processor mode.               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           98-12-05  Added relocation types SECREL and SECTION.           *
 *                                                                          *
 ****************************************************************************/

static __inline void apply_x86_relocation(void *raddr, RELENTRY *rel, addr_t vaddr)
{
#ifdef PRERELEASE
    if (!rel->sym->flags.relocated && !rel->sym->flags.absolute)
    {
        printf("The symbol %s (%s) has not been relocated!\n",
            rel->sym->name, rel->sym->weak_name ? rel->sym->weak_name : "");
    }
#endif

    if (rel->sym->flags.absolute)
        rel->type = COFF_R_I386_ABS;

    switch (rel->type)
    {
        case COFF_R_I386_DIR32:
            *(long *)raddr += (rel->sym->value + options.image_base);
            add_base_relocation(vaddr);
            break;

        case COFF_R_I386_DIR32NB:
            *(long *)raddr += rel->sym->value;
            break;

        case COFF_R_I386_REL32:
            *(long *)raddr += (rel->sym->value - vaddr - sizeof(long));
            break;

        case COFF_R_I386_ABS:
            *(long *)raddr = rel->sym->value;
            break;

        case COFF_R_I386_SECREL:  /* CodeView */
            *(long *)raddr = rel->sym->value -
                group_address_from_index(rel->sym->scnum);
            break;

        case COFF_R_I386_SECTION:  /* CodeView */
            *(short *)raddr = rel->sym->scnum;
            break;

        default:
            apperror(RCWARNING(ERROR_UNKNOWN_RELOC_TYPE), rel->type, rel->sym->name);
            break;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: apply_arm_relocation                                           *
 *                                                                          *
 * Purpose : Update a relocation entry; ARM processor mode.                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-11-16  Created                                              *
 *                                                                          *
 ****************************************************************************/

static __inline void apply_arm_relocation(void *raddr, RELENTRY *rel, addr_t vaddr)
{
#ifdef PRERELEASE
    if (!rel->sym->flags.relocated && !rel->sym->flags.absolute)
    {
        printf("The symbol %s (%s) has not been relocated!\n",
            rel->sym->name, rel->sym->weak_name ? rel->sym->weak_name : "");
    }
#endif

    if (rel->sym->flags.absolute)
        rel->type = COFF_R_ARM_ABS;

    switch (rel->type)
    {
        case COFF_R_ARM_ADDR32:
            *(long *)raddr += (rel->sym->value + options.image_base);
            /* add_base_relocation(vaddr); */
            break;

        case COFF_R_ARM_ADDR32NB:
            *(long *)raddr += rel->sym->value;
            break;

        case COFF_R_ARM_BRANCH24:  /* update low 24 bits */
            *(long *)raddr |= ((rel->sym->value - vaddr - 8) >> 2) & 0x00FFFFFF;
            break;

        case COFF_R_ARM_ABS:
            *(long *)raddr = rel->sym->value;
            break;

        case COFF_R_ARM_SECREL:  /* CodeView */
            *(long *)raddr = rel->sym->value -
                group_address_from_index(rel->sym->scnum);
            break;

        case COFF_R_ARM_SECTION:  /* CodeView */
            *(short *)raddr = rel->sym->scnum;
            break;

        default:
            apperror(RCWARNING(ERROR_UNKNOWN_RELOC_TYPE), rel->type, rel->sym->name);
            break;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: group_address_from_index                                       *
 *                                                                          *
 * Purpose : Look up a group by index and return it's address.              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-12-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

static __inline addr_t group_address_from_index(short scnum)
{
    GRPENTRY *grp;

    for (grp = group_list; grp != NULL; grp = grp->next)
        if (--scnum == 0) return grp->vaddr;

    return 0;
}

/****************************************************************************
 *                                                                          *
 * Function: add_base_relocation                                            *
 *                                                                          *
 * Purpose : Add a new entry to the base relocation array.                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void add_base_relocation(addr_t vaddr)
{
    if (reloc_count == reloc_maxcount)
    {
        reloc_maxcount += RELOC_ARRAY_INCREMENT;
        relocs = (addr_t *)my_realloc(relocs, reloc_maxcount * sizeof(addr_t));
    }

    relocs[reloc_count++] = vaddr;
}

/****************************************************************************
 *                                                                          *
 * Function: comp_address                                                   *
 *                                                                          *
 * Purpose : Compare two offsets for the qsort() function.                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static int __cdecl comp_address(const void *item1, const void *item2)
{
    return (*(addr_t *)item1 - *(addr_t *)item2);
}

/****************************************************************************
 *                                                                          *
 * Function: alloc_reloc_section                                            *
 *                                                                          *
 * Purpose : Allocate a empty base relocation section (last).               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *                                                                          *
 ****************************************************************************/

static SEGENTRY *alloc_reloc_section(void)
{
    SCNENTRY *scn;
    SEGENTRY *seg;
    GRPENTRY *grp;

    /*
     * Allocate a section buffer.
     */
    scn = lookup_section(COFF_RELOCS, COFF_RELOCS_FLAGS);
    seg = add_segment_to_section(scn);

    seg->alignment = sizeof(long);

    /*
     * We are doing this very late; manually create group.
     */
    grp = lookup_group(COFF_RELOCS, COFF_RELOCS_FLAGS);
    add_section_to_group(grp, scn);

    return seg;
}

/****************************************************************************
 *                                                                          *
 * Function: update_reloc_section                                           *
 *                                                                          *
 * Purpose : Update the base relocation section.                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           00-10-02  Bugfix: dummy type/offset filler added.              *
 *                                                                          *
 ****************************************************************************/

static void update_reloc_section(SEGENTRY *seg)
{
    COFF_BRELOC *br;
    addr_t vaddr;
    void *ip;
    long size;
    size_t i;

    /*
     * Allocate the raw data buffer.
     */
    seg->size = sizeof_reloc_section();
    seg->data = ip = my_alloc(seg->size);

    /*
     * Initialize a fixup block header.
     */
    br = (COFF_BRELOC *)ip;
    ip = ((COFF_BRELOC *)ip + 1);

    size = COFF_BRELSZ;
    vaddr = (relocs) ? (relocs[0] & ~(INTEL_PAGE_SIZE-1)) : 0;

    /*
     * Walk through all base relocations.
     */
    for (i = 0; i < reloc_count; i++)
    {
        /*
         * Relocation outside current page?
         */
        if ((vaddr + INTEL_PAGE_SIZE-1) < relocs[i])
        {
            /*
             * Initialize a fixup block header.
             */
            if (size > COFF_BRELSZ)
            {
                if (size % sizeof(long))
                {
                    *(short *)ip = TYPE_OFFSET_NULL;
                    ip = ((short *)ip + 1);
                    size += sizeof(short);
                }

                br->b_vaddr = vaddr;
                br->b_size = size;

                br = (COFF_BRELOC *)ip;
                ip = ((COFF_BRELOC *)ip + 1);
            }

            size = COFF_BRELSZ;
            vaddr = (relocs[i] & ~(INTEL_PAGE_SIZE-1));
        }

        /*
         * Write a type/offset entry.
         */
        *(short *)ip = TYPE_OFFSET_HIGHLOW(relocs[i] - vaddr);
        ip = ((short *)ip + 1);
        size += sizeof(short);
    }

    /*
     * Write a dummy type/offset entry (if no relocations).
     */
    if (reloc_count == 0)
    {
        *(short *)ip = TYPE_OFFSET_NULL;
        ip = ((short *)ip + 1);
        size += sizeof(short);
    }

    /*
     * Initialize a fixup block header.
     */
    if (size > COFF_BRELSZ)
    {
        if (size % sizeof(long))
        {
            *(short *)ip = TYPE_OFFSET_NULL;
            ip = ((short *)ip + 1);
            size += sizeof(short);
        }

        br->b_vaddr = vaddr;
        br->b_size = size;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: sizeof_reloc_section                                           *
 *                                                                          *
 * Purpose : Calculate the total size of the base relocation section.       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           98-01-28  Created                                              *
 *           00-10-02  Bugfix: dummy type/offset filler added.              *
 *                                                                          *
 ****************************************************************************/

static size_t sizeof_reloc_section(void)
{
    size_t size;
    size_t totsize;
    addr_t vaddr;
    size_t i;

    size = COFF_BRELSZ;
    totsize = 0;
    vaddr = 0;

    /*
     * Walk through all base relocations.
     */
    for (i = 0; i < reloc_count; i++)
    {
        /*
         * Relocation outside current page?
         */
        if ((vaddr + INTEL_PAGE_SIZE-1) < relocs[i])
        {
            /*
             * Initialize fixup block header.
             */
            if (size > COFF_BRELSZ)
                totsize += ROUNDUP_DWORD(size);

            size = COFF_BRELSZ;
            vaddr = (relocs[i] & ~(INTEL_PAGE_SIZE-1));
        }

        /*
         * Add size of a type/offset entry.
         */
        size += sizeof(short);
    }

    /*
     * Add size of a dummy type/offset entry.
     */
    if (reloc_count == 0)
        size += sizeof(short);

    if (size > COFF_BRELSZ)
        totsize += ROUNDUP_DWORD(size);

    return totsize;
}

