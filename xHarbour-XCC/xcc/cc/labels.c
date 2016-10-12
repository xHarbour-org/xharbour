/****************************************************************************
 *                                                                          *
 * File    : labels.c                                                       *
 *                                                                          *
 * Purpose : ISO C Compiler; Generic Assembler; Label manager.              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "lcc.h"

/*
 * A local label is one that begins with exactly one period. Things
 * that begin with *two* periods are POCC-specific things.
 */
#define islocal(name)   ((name)[0] == '.' && (name)[1] != '.')

#define BLOCK_ENTRIES   320     /* number of labels/block */
#define HASH_ENTRIES    31      /* number of hash table entries */
#define BLOCK_SIZE      (BLOCK_ENTRIES * sizeof(ALABEL))

#define END_LIST        -3      /* don't clash with NO_SEG! */
#define END_BLOCK       -2
#define BOGUS_VALUE     -4

/* label entry */
typedef union _ALABEL {
    LABELDEF def;               /* definition of the label */
    struct {
        long movingon;
        union _ALABEL *next;    /* next block */
    } adm;
} ALABEL;

/* Locals */
static ALABEL *labtab[HASH_ENTRIES];    /* using a hash table */
static ALABEL *labfree[HASH_ENTRIES];   /* pointer into the above */

static char *prevname;

/* Static function prototypes */
static ALABEL *install_label(const char *, bool_t);
static void init_block(ALABEL *);
static char *perm_copy(const char *, const char *);

/****************************************************************************
 *                                                                          *
 * Function: asmlab_init                                                    *
 *                                                                          *
 * Purpose : Initialize the label manager.                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void asmlab_init(void)
{
    int i;

    for (i = 0; i < HASH_ENTRIES; i++)
    {
        labtab[i] = my_alloc(BLOCK_SIZE);
        init_block(labtab[i]);
        labfree[i] = labtab[i];
    }

    prevname = "";
}

/****************************************************************************
 *                                                                          *
 * Function: asmlab_cleanup                                                 *
 *                                                                          *
 * Purpose : Cleanup after the label manager.                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void asmlab_cleanup(void)
{
    int i;

    for (i = 0; i < HASH_ENTRIES; i++)
    {
        ALABEL *label;
        ALABEL *hold;

        label = hold = labtab[i];
        while (label)
        {
            while (label->adm.movingon != END_BLOCK) label++;
            label = label->adm.next;
            my_free(hold);
            hold = label;
        }
    }
}

/****************************************************************************
 *                                                                          *
 * Function: asmlab_lookup                                                  *
 *                                                                          *
 * Purpose : Find a label by name. Return segment & offset if success.      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

bool_t asmlab_lookup(const char *name, long *segment, long *offset, LABELDEF **labdef)
{
    ALABEL *label;

    label = install_label(name, FALSE);
    if (label && label->def.defined)
    {
        *segment = label->def.segment;
        *offset = label->def.offset;
        *labdef = &label->def;
        return TRUE;
    }

    return FALSE;
}

/****************************************************************************
 *                                                                          *
 * Function: install_label                                                  *
 *                                                                          *
 * Purpose : Find the ALABEL entry for the given label name.                *
 *           Create a new one if not found, and 'create' is true.           *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static ALABEL *install_label(const char *name, bool_t create)
{
    int hash = 0;
    char *prev;
    const char *p;
    int prevlen;
    ALABEL *label;

    prev = (islocal(name)) ? prevname : "";
    prevlen = strlen(prev);

    for (p = prev; *p != 0; p++)
        hash += *p;
    for (p = name; *p != 0; p++)
        hash += *p;
    hash %= HASH_ENTRIES;

    /*
     * Lookup the silly label.
     */
    for (label = labtab[hash]; label->adm.movingon != END_LIST; label++)
    {
        if (label->adm.movingon == END_BLOCK)
        {
            label = label->adm.next;
            if (!label) break;
        }

        if (strncmp(label->def.name, prev, prevlen) == 0 &&
            strcmp(label->def.name+prevlen, name) == 0)
            return label;
    }

    if (create)
    {
        if (labfree[hash]->adm.movingon == END_BLOCK)
        {
            /*
             * Must allocate a new block.
             */
            labfree[hash]->adm.next = my_alloc(BLOCK_SIZE);
            labfree[hash] = labfree[hash]->adm.next;
            init_block(labfree[hash]);
        }

        labfree[hash]->adm.movingon = BOGUS_VALUE;
        labfree[hash]->def.name = perm_copy(prev, name);
        labfree[hash]->def.defined = FALSE;
        labfree[hash]->def.global = FALSE;
        labfree[hash]->def.external = FALSE;
        labfree[hash]->def.normal = FALSE;
        labfree[hash]->def.symbol = 0;

        return labfree[hash]++;
    }

    return NULL;
}

/****************************************************************************
 *                                                                          *
 * Function: init_block                                                     *
 *                                                                          *
 * Purpose : Initialize a new hash block.                                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static void init_block(ALABEL *block)
{
    int i;

    for (i = 0; i < BLOCK_ENTRIES-1; i++)
        block[i].adm.movingon = END_LIST;

    block[BLOCK_ENTRIES-1].adm.movingon = END_BLOCK;
    block[BLOCK_ENTRIES-1].adm.next = NULL;
}

/****************************************************************************
 *                                                                          *
 * Function: perm_copy                                                      *
 *                                                                          *
 * Purpose : Copy two label strings to permanent storage.                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

static char *perm_copy(const char *s1, const char *s2)
{
    size_t len = strlen(s1) + strlen(s2) + 1;
    char *p;
    char *q;

    p = q = memalloc(len, PERM);

    while ((*q = *s1++) != 0) q++;
    while ((*q++ = *s2++) != 0) ;

    return p;
}

/****************************************************************************
 *                                                                          *
 * Function: asmlab_define                                                  *
 *                                                                          *
 * Purpose : Define a label, during pass 1.                                 *
 *                                                                          *
 * Comment : This procedure is called to define a new label. The boolean    *
 *           parameter 'normal' states whether the label is a 'normal'      *
 *           label (which should affect the local-label system), or         *
 *           something other like an EQU or a segment-base symbol, which    *
 *           shouldn't.                                                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void asmlab_define(const char *name, long segment, long offset, bool_t normal, bool_t external)
{
    ALABEL *label;

    label = install_label(name, TRUE);
    if (label->def.defined)
    {
        apperror(RCERROR(ERROR_LABEL_REDEFINITION), name);
        return;
    }

    label->def.defined = TRUE;
    if (external) label->def.external = TRUE;

    if (name[0] != '.' && normal)  /* not local or special */
        prevname = label->def.name;
    else if (name[0] == '.' && name[1] != '.' && *prevname == '\0')
        apperror(RCERROR(ERROR_LOCAL_LABEL_TOO_SOON));

    label->def.segment = segment;
    label->def.offset = offset;
    label->def.normal = (name[0] != '.' && normal);
}

/****************************************************************************
 *                                                                          *
 * Function: asmlab_redefine                                                *
 *                                                                          *
 * Purpose : Redefine a label, during pass 2.                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void asmlab_redefine(const char *name, long segment, long offset)
{
    ALABEL *label;

    label = install_label(name, FALSE);
    if (!label) apperror(RCFATAL(ERROR_INTERNAL), "asmlab_redefine()");

    if (*name != '.' && label->def.normal)
        prevname = label->def.name;

    label->def.segment = segment;
    label->def.offset = offset;

    if (!label->def.referenced && label->def.external && options.optimize)
        return;

    label->def.symbol = (*OF->symbol)(label->def.name,
        label->def.segment, label->def.offset,
        label->def.external ? SYMDEF_EXTERN :
        label->def.global ? SYMDEF_GLOBAL : SYMDEF_LOCAL);
}

/****************************************************************************
 *                                                                          *
 * Function: asmlab_phase_check                                             *
 *                                                                          *
 * Purpose : Check for phase errors, during pass 3.                         *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void asmlab_phase_check(const char *name, long segment, long offset)
{
    ALABEL *label;

    label = install_label(name, FALSE);
    if (!label) apperror(RCFATAL(ERROR_INTERNAL), "asmlab_phase_check()");

    if (*name != '.' && label->def.normal)
        prevname = label->def.name;

    if (label->def.segment != segment ||
        label->def.offset != offset)
    {
        printf("def.segment=%ld, segment=%ld, def.offset=%ld, offset=%ld\n", label->def.segment, segment, label->def.offset, offset);
        apperror(RCFATAL(ERROR_PHASE_ERROR), name);
    }
}

/****************************************************************************
 *                                                                          *
 * Function: asmlab_define_common                                           *
 *                                                                          *
 * Purpose : Define a label as COMMON.                                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void asmlab_define_common(const char *name, long segment, long size)
{
    ALABEL *label;

    label = install_label(name, TRUE);
    if (label->def.defined)
    {
        apperror(RCERROR(ERROR_LABEL_REDEFINITION), name);
        return;
    }

    label->def.defined = TRUE;

    if (name[0] != '.')  /* not local or special */
        prevname = label->def.name;
    else
        apperror(RCERROR(ERROR_LOCAL_LABEL_AS_COMMON), name);

    label->def.segment = segment;
    label->def.offset = 0;
    label->def.symbol = (*OF->symbol)(label->def.name, label->def.segment, size, SYMDEF_COMMON);
}

/****************************************************************************
 *                                                                          *
 * Function: asmlab_declare_global                                          *
 *                                                                          *
 * Purpose : Declare a label as GLOBAL.                                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void asmlab_declare_global(const char *name)
{
    ALABEL *label;

    if (islocal(name))
    {
        apperror(RCERROR(ERROR_LABEL_ALREADY_LOCAL), name);
        return;
    }

    label = install_label(name, TRUE);
    if (!label->def.global)
    {
        if (label->def.defined && !label->def.external)
            apperror(RCERROR(ERROR_MISPLACED_GLOBAL), name);
        else
            label->def.global = TRUE;
    }
}

/****************************************************************************
 *                                                                          *
 * Function: asmlab_is_extern                                               *
 *                                                                          *
 * Purpose : Return true if the given label is EXTERN.                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

bool_t asmlab_is_extern(const char *name)
{
    ALABEL *label;

    label = install_label(name, FALSE);
    return label && label->def.external;
}

