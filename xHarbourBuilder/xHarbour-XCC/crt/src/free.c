/****************************************************************************
 *                                                                          *
 * File    : free.c                                                         *
 *                                                                          *
 * Purpose : free function.                                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xalloc.h"
#include "xthread.h"

/* free an allocated data object */
#ifdef __MT__
static void _mt_free(void *cp)
#else
void __cdecl (free)(void *cp)
#endif
{
    /*
     * This is where the boundary tags come into their own. The
     * boundary tag guarantees a constant time insert with full
     * coalescing (the time varies slightly for the four case possible,
     * but still, essentially a very fast free.
     */
    /*
     *  P0 is the block being freed. P1 is the pointer to the block
     *  before the block being freed, and P2 is the block after it.
     *  We can either coalesce with P1, P2, both, or neither
     */
    register Word *p0, *p1, *p2;
    register size_t sizep0;
    int bin, oldbin = -1;

    if (cp == 0)
        return;

    p0 = (Word *)cp;
    p0 -= HEADERWORDS;

    CHECKALLOCPTR(p0);
    /* With debugging, the CHECKALLOCPTR would have already aborted */
    if (TAG(p0) == FREE)
    {
        errno = EINVAL;
        return;
    }

    /*
     * clear the entire block that used to be p0's, just in case
     * someone tries to refer to it or anything in it again.  We leave
     * the end tags alone for now - we'll smash them individually
     * depending on the way p0 merges with p1 and/or p2.
     */
    sizep0 = SIZE(p0);
    DMEMSET(p0 + FREEHEADERWORDS, sizep0 - FREE_OVERHEAD);

    p1 = p0 - 1;
    /*
     * p0 now points to the end of the block -- we start treating it as
     * a free block.
     */
    p0 += sizep0 - 1;
    p2 = p0 + 1;

    /*
     * We can't match the SIZEFIELDs of p1/p2 with p1/p2 + SIZE(p1/p2)
     * -1 because they might be a fake tag to indicate the bounds of
     * the arena. Further, we should only check p1 if p0-1 is not the
     * _malloc_loword or an arena bound - else p1 is probably not a
     * valid pointer. If tag p0-1 is allocated, then it could be an
     * arena bound.
     */
    if (TAG(p2) == FREE)
    {
        /*
         * Aha - block p2 (physically after p0) is free.  Merging
         * p0 with p2 merely means increasing p2's size to
         * incorporate p0 - no other pointer shuffling needed.
         * We'll move it to the right free-list later, if necessary.
         */
        p2 += FREESIZE(p2) - 1;
        oldbin = GETBIN(FREESIZE(p2));
        CHECKFREEPTR(p2);
        sizep0 += FREESIZE(p2);
        SIZEFIELD(p2- sizep0 + 1) = SIZEFIELD(p2) = FREEMASK(sizep0);
        /* Smash p0's old end tag and p2's old start tag */
        DMEMSET(p0 - FREETRAILERWORDS + 1, FREETRAILERWORDS + FREEHEADERWORDS);
        p0 = p2; /* p0 just vanished - became part of p2 */
    }
    if (TAG(p1) == FREE)
    {
        /*
         * The block p1 (physically precedes p0 in memory) is free.
         * We grow p0 backward to absorb p1 and delete p1 from its
         * free list, since it no longer exists.
         */
        CHECKFREEPTR(p1);
        sizep0 += FREESIZE(p1);
        bin = GETBIN(FREESIZE(p1));
        UNLINK(p1, bin);
        SIZEFIELD(p0 - sizep0 + 1) = SIZEFIELD(p0) = FREEMASK(sizep0);
        /*
         * smash the free list pointers in p1 (SIZE, NEXT, PREV) to
         * make sure no one refers to them again. We cannot smash
         * the start boundary tag because it becomes the start tag
         * for the new block.  Also trash p0's start tag.
         */
        DMEMSET(p1 - FREETRAILERWORDS + 1, FREETRAILERWORDS + FREEHEADERWORDS);
    }

    bin = GETBIN(sizep0);
    if (oldbin != bin)
    {
        /*
         * If we're here, it means block P0 needs to be inserted in
         * the correct free list, either because it didn't merge
         * with anything, or because it merged with p1 so we
         * deleted p1, or it merged with p2 and grew out p2's
         * existing free-list.
         */
        if (oldbin >= 0)
        {
            /* merged with P2, still in P2's free-list */
            UNLINK(p0, oldbin);
        }

        LINK(p0, sizep0, bin);
        __malloc_lastbin = bin;
        __malloc_rovers[bin] = p0;
    }

    CHECKHEAP();
    return;
}

#ifdef __MT__
/* free an allocated data object */
void __cdecl (free)(void *cp)
{
    __mtlock(_HEAP_LOCK);
    __try
    {
        _mt_free(cp);
    }
    __finally
    {
        __mtunlock(_HEAP_LOCK);
    }
}
#endif /* __MT__ */

