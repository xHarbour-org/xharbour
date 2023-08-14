/****************************************************************************
 *                                                                          *
 * File    : alloc.c                                                        *
 *                                                                          *
 * Purpose : ISO C Compiler; Arena memory allocations.                      *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "proj.h"
#pragma hdrstop

#include "lcc.h"

typedef struct _BLOCK {
    struct _BLOCK *next;
    char *limit;
    char *avail;
} BLOCK;

typedef union _ALIGN {
    long l;
    char *p;
    double d;
    int (*f)(void);
} ALIGN;

typedef union _HEADER {
    BLOCK b;
    ALIGN a;
} HEADER;

#ifdef PURIFY
static HEADER *arena[3];

/****************************************************************************
 *                                                                          *
 * Function: memalloc                                                       *
 *                                                                          *
 * Purpose : Allocate a memory block in the given arena.                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void *memalloc(size_t size, uint_t a)
{
    HEADER *ap = my_alloc(sizeof(*ap) + size);

    assert(a < NELEMS(arena));

    ap->b.next = (void *)arena[a];
    arena[a] = ap;

    return ap + 1;
}

/****************************************************************************
 *                                                                          *
 * Function: memfree                                                        *
 *                                                                          *
 * Purpose : Free all memory blocks in the given arena.                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void memfree(uint_t a)
{
    HEADER *ap;
    HEADER *next;

    assert(a < NELEMS(arena));

    for (ap = arena[a]; ap != NULL; ap = next)
    {
        next = (void *)ap->b.next;
        my_free(ap);
    }
    arena[a] = NULL;
}

/****************************************************************************
 *                                                                          *
 * Function: memarray                                                       *
 *                                                                          *
 * Purpose : Allocate a new array in the given arena.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void *memarray(size_t nelems, size_t size, uint_t a)
{
    return memalloc(nelems * size, a);
}

#else  /* !PURIFY */
static BLOCK
     first[] = { { NULL }, { NULL }, { NULL } },
    *arena[] = { &first[0], &first[1], &first[2] };
static BLOCK *freeblocks;

/****************************************************************************
 *                                                                          *
 * Function: memalloc                                                       *
 *                                                                          *
 * Purpose : Allocate a memory block in the given arena.                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void *memalloc(size_t size, uint_t a)
{
    BLOCK *ap;

    assert(a < NELEMS(arena));
    assert(size > 0);

    ap = arena[a];
    size = roundup(size, sizeof(ALIGN));

    while (size > (size_t)(ap->limit - ap->avail))
    {
        if ((ap->next = freeblocks) != NULL)
        {
            freeblocks = freeblocks->next;
            ap = ap->next;
        }
        else
        {
            size_t m = sizeof(HEADER) + size + roundup(10*1024, sizeof(ALIGN));

            ap->next = my_alloc(m);
            ap = ap->next;
            ap->limit = (char *)ap + m;
        }

        ap->avail = (char *)((HEADER *)ap + 1);
        ap->next = NULL;
        arena[a] = ap;
    }

    ap->avail += size;
    return ap->avail - size;
}

/****************************************************************************
 *                                                                          *
 * Function: memfree                                                        *
 *                                                                          *
 * Purpose : Free all memory blocks in the given arena.                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void memfree(uint_t a)
{
    assert(a < NELEMS(arena));

    arena[a]->next = freeblocks;
    freeblocks = first[a].next;
    first[a].next = NULL;
    arena[a] = &first[a];
}

/****************************************************************************
 *                                                                          *
 * Function: memarray                                                       *
 *                                                                          *
 * Purpose : Allocate a new array in the given arena.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-06-06  Created                                              *
 *                                                                          *
 ****************************************************************************/

void *memarray(size_t nelems, size_t size, uint_t a)
{
    return memalloc(nelems * size, a);
}
#endif  /* PURIFY */
