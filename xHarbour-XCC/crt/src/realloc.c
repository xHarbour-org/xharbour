/****************************************************************************
 *                                                                          *
 * File    : realloc.c                                                      *
 *                                                                          *
 * Purpose : realloc function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xalloc.h"
#include "xthread.h"

/* reallocate a data object on the heap */
#ifdef __MT__
static void *_mt_realloc(void *cp, size_t nbytes)
#else
void * __cdecl (realloc)(void *cp, size_t nbytes)
#endif
{
    register Word *p0 = (Word *)cp;
    register Word *p1;
    void *tmp;
    register size_t required;
    register size_t sizep0;
    int bin;

    if (p0 == 0)
        return malloc(nbytes);

    if (nbytes == 0)
    {
        free(cp);
        return 0;
    }

    required = ALLOC_OVERHEAD + (nbytes + sizeof(Word) - 1) / sizeof(Word);
    if (required < (size_t)MALLOC_MINCHUNK)
        required = MALLOC_MINCHUNK;

    p0 -= HEADERWORDS;

    CHECKALLOCPTR(p0);
    /* With debugging, the CHECKALLOCPTR would have already aborted */
    if (TAG(p0) == FREE)
    {
        errno = EINVAL;
        return 0;
    }

    sizep0 = SIZE(p0);
    if (sizep0 >= required)
    {
        /* Shrinking the block */
        size_t after = sizep0 - required;

        SET_REALSIZE(p0, nbytes);
        if (after < MALLOC_MINCHUNK)
        {
            /* Not enough to free what's left so we return the block intact */
            return cp;
        }
        SIZEFIELD(p0+required-1) = SIZEFIELD(p0) = ALLOCMASK(required);
        p0 += required;
        /*
         * We free what's after the block - mark it alloced and
         * throw it to free() to figure out whether to merge it
         * with what follows...
         */
        SIZEFIELD(p0 + after - 1) = SIZEFIELD(p0) = ALLOCMASK(after);
        SET_REALSIZE(p0, (after - ALLOC_OVERHEAD) * sizeof(Word));
        free((void *)(p0 + HEADERWORDS));
        return cp;
    }

    /*
     * If we get here, then we are growing the block p0 to something bigger.
     */
    p1 = p0 + sizep0;
    required -= sizep0;
    if (TAG(p1) != FREE || FREESIZE(p1) < required)
    {
        /* Have to do it the hard way: block after us cannot be used */
        tmp = malloc(nbytes);
        if (tmp != 0)
        {
            MEMCPY(tmp, cp, ((SIZE(p0) - ALLOC_OVERHEAD)));
            free(cp);
        }
        return tmp;
    }
    /*
     * block after us is free and big enough to provide the required
     * space, so we grow into that block.
     */
    p1 += FREESIZE(p1) - 1;
    CHECKFREEPTR(p1);
    bin = GETBIN(FREESIZE(p1));
    CARVE(p1, FREESIZE(p1), bin, required);
    sizep0 += required;
    SIZEFIELD(p0) = SIZEFIELD(p0+sizep0-1) = ALLOCMASK(sizep0);
    SET_REALSIZE(p0, nbytes);
    CHECKHEAP();
    return cp;

}

#ifdef __MT__
/* reallocate a data object on the heap */
void * __cdecl (realloc)(void *cp, size_t nbytes)
{
    void *p = 0;

    __mtlock(_HEAP_LOCK);
    __try
    {
        p = _mt_realloc(cp, nbytes);
    }
    __finally
    {
        __mtunlock(_HEAP_LOCK);
    }

    return p;
}
#endif /* __MT__ */

