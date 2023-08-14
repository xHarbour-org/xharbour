/****************************************************************************
 *                                                                          *
 * File    : verify.c                                                       *
 *                                                                          *
 * Purpose : heap_verify function.                                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xalloc.h"
#include "xthread.h"

/*
 *  Goes through the entire heap checking all pointers, tags for
 *  consistency. Should catch most casual heap corruption (overwriting
 *  the end of a malloc'ed chunk, etc..) Nonetheless, heap corrupters
 *  tend to be devious and ingenious in ways they corrupt heaps (Believe
 *  me, I know:-). We should probably do the same thing if NDEBUG is
 *  defined, but return 0 instead of aborting. If fullcheck is non-zero,
 *  it also checks that free blocks contain the magic pattern written
 *  into them when they were freed to make sure the program is not still
 *  trying to access those blocks.
 */
#ifdef __MT__
static void _mt_heap_verify(int fullcheck)
#else
void heap_verify(int fullcheck)
#endif
{
    register Word *ptr, *p, *blk, *blkend;
    int i;

    if (__malloc_loword == 0)  /* Nothing malloc'ed yet */
        return;

    for (i = 0; i < MAXBINS; i++)
    {
        ptr = __malloc_rovers[i];
        if (ptr != 0)
        {
            assert(i >= __malloc_firstbin);
            CHECKFREEPTR(ptr);
        }
    }

    assert(__malloc_rovers[MAXBINS] == 0);
    for (ptr = __malloc_mem; ptr != 0; ptr = ptr->next)
    {
        /*
         * Check arena bounds - not same as checking block tags,
         * despite similar appearance of the test.
         */
        assert(SIZEFIELD(ptr+1) == SIZEFIELD(ptr + SIZE(ptr+1)));
        blkend = ptr + SIZE(ptr + 1);

        for (blk = ptr + ARENASTART; blk < blkend; blk += SIZE(blk))
        {
            assert(PTR_IN_HEAP(blk));
            assert(VALID_START_SIZE_FIELD(blk));
            if (TAG(blk) == FREE)
            {
                p = blk + FREESIZE(blk) - 1;
                assert(VALID_NEXT_PTR(p));
                assert(VALID_PREV_PTR(p));
#ifndef NDEBUG
                if (fullcheck)
                {
                    /* Make sure all free blocks are filled with FREEMAGIC */
                    int i, n;
                    char *cp;

                    n = (SIZE(blk) - FREE_OVERHEAD) * sizeof(Word);
                    cp = (char *)(blk + FREEHEADERWORDS);
                    for (i = 0; i < n; i++, cp++)
                    {
                        assert(*cp == FREEMAGIC);
                    }
                }
#endif /* NDEBUG */
            }
            else
            {
                assert(VALID_MAGIC(blk));
            }
        }
    }

    return;
}


#ifdef __MT__
void heap_verify(int fullcheck)
{
    __mtlock(_HEAP_LOCK);
    __try
    {
        _mt_heap_verify(fullcheck);
    }
    __finally
    {
        __mtunlock(_HEAP_LOCK);
    }
}
#endif /* __MT__ */


