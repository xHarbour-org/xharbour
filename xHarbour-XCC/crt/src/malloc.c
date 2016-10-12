/****************************************************************************
 *                                                                          *
 * File    : malloc.c                                                       *
 *                                                                          *
 * Purpose : malloc function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xalloc.h"
#include "xthread.h"
#include "xcrt.h"

Word *__malloc_rovers[MAXBINS+1] = {0};
Word *__malloc_hiword = 0;
Word *__malloc_loword = 0;
Word *__malloc_mem = 0;

const short __malloc_binmax[MAXBINS] = { 8, 16, 32, 64, 128, 256, 512, 1024 };
int __malloc_firstbin = 0;
int __malloc_lastbin = 0;

static const size_t sbrkunits = DEF_SBRKUNITS;

static int grabhunk(size_t nwords)
{
    void *cp;
    size_t morecore;
    Word *ptr;
    size_t sbrkwords;
    size_t blksize;
    static char *spare;
    static int nspare;

    /*
     *  two words for fake boundary tags for the entire block, and one
     *  for the next ptr of the block.
     */
#define EXCESS 3
    sbrkwords = (size_t)(((nwords + EXCESS) / sbrkunits + 1) * sbrkunits);
    morecore = sbrkwords * sizeof(Word) + SBRKEXTRA;
    if ((cp = __getmem(morecore)) == 0)
        return 0;

    /*
     * Should first GUARANTEE that what sbrk returns is aligned to
     * Word boundaries - see align.h. Unfortunately, to guarantee
     * that the pointer returned by sbrk is aligned on a word
     * boundary, we must ask for sizeof(Word) -1 extra bytes, since
     * we have no guarantee what other sbrk'ed blocks exist. (Sun
     * sbrk always returns an aligned value, that is another story!)
     * We use spare and nspare to keep track of the bytes wasted, so
     * that we can try and reuse them later. If no other sbrk()s are
     * called, then nspare rotates through the values 3, 2, 1, 0,
     * and the first branch of the if() is always taken.
     */
    if ((spare + nspare) == (char *)cp)
    {
        ptr = (Word *)SBRKALIGN(spare);
        morecore += nspare;
        sbrkwords = morecore / sizeof(Word);
    }
    else
    {
        ptr = (Word *)SBRKALIGN(cp);
        morecore -= (char *)ptr - (char *)cp;
    }
    spare = (char *)(ptr + sbrkwords);
    nspare = (morecore - sbrkwords * sizeof(Word));

    /*
     * If the new chunk adjoins __malloc_hiword, then __malloc_hiword
     * need not be a fake boundary tag any longer, (its a real one) and
     * the higher end of the block we sbrk'ed is the fake tag.  So we
     * tag it appropriately, make the start of the block point to the
     * old __malloc_hiword, and free it.  If we aren't next to
     * __malloc_hiword, then someone else sbrk'ed in between, so we
     * can't coalesce over the boundary anyway, in which case we just
     * change __malloc_hiword to be in the new sbrk'ed block without
     * damaging the old one. And we free the block.
     */
    if (ptr != __malloc_hiword + 1 || __malloc_mem == 0)
    {
        /* Non-contiguous sbrk'ed block, or first sbrk we've done. */
        /*
         * First push this block on the stack of non-contiguous blocks
         * we've sbrked. !! For real paranoia, we'd also check
         * __malloc_mem...
         */
        register Word *tmp = __malloc_mem;

        __malloc_mem = ptr;
        ptr->next = tmp;
        ptr++;
        sbrkwords--;

        __malloc_hiword = ptr;
        if (__malloc_loword == 0 || __malloc_loword > ptr)
        {
            /* First time - set lower bound. */
            __malloc_loword = ptr;
        }

        /*
         * Fake boundary tags to indicate the ends of an arena.
         * Since they are marked as allocated, no attempt will be
         * made to coalesce before or after them.
         */
        SIZEFIELD(ptr) = ALLOCED | sbrkwords;
        __malloc_hiword += sbrkwords - 1;
        SIZEFIELD(__malloc_hiword) = ALLOCED | sbrkwords;

        /* * Subtract 2 for the special arena end tags. */
        sbrkwords -= 2;
        ptr++;
        DMEMSET(ptr + FREEHEADERWORDS, sbrkwords - FREE_OVERHEAD);
        ptr = __malloc_hiword - 1;
        __malloc_lastbin = GETBIN(sbrkwords);
        LINK(ptr, sbrkwords, __malloc_lastbin)
        __malloc_rovers[__malloc_lastbin] = ptr;
        while (__malloc_rovers[__malloc_firstbin] == 0 && __malloc_firstbin < MAXBINS-1)
            __malloc_firstbin++;
        return 1;
    }

    /*
     * If we get here, then the sbrked chunk is contiguous, so we fake
     * up the boundary tags and size to look like an allocated block
     * and then call free()
     */
    ptr--;
    blksize = SIZE(ptr) + sbrkwords;
    SIZEFIELD(ptr) = ALLOCMASK(sbrkwords);
    __malloc_hiword += sbrkwords;
    SIZEFIELD(__malloc_hiword-1) = SIZEFIELD(ptr);

    /* Update special arena end tags of the memory chunk */
    SIZEFIELD(__malloc_hiword) = ALLOCMASK(blksize);
    SIZEFIELD(__malloc_hiword - blksize + 1) = ALLOCMASK(blksize);
    SET_REALSIZE(ptr, (sbrkwords - ALLOC_OVERHEAD) * sizeof(Word));
    free((void *)(ptr + HEADERWORDS));
    return 1;
}

/* allocate a data object on the heap */
#ifdef __MT__
static void *_mt_malloc(size_t nbytes)
#else
void * __cdecl (malloc)(size_t nbytes)
#endif /* __MT__ */
{
    register Word *start, *search = 0;
    register Word *p;
    register size_t required;
    register size_t searchsize;
    int bin;

    required = ALLOC_OVERHEAD + (nbytes + sizeof(Word) - 1) / sizeof(Word);
    if (required < (size_t)MALLOC_MINCHUNK)
        required = MALLOC_MINCHUNK;

    searchsize = 0;
    bin = GETBIN(required);
    if (bin < __malloc_firstbin)
        bin = __malloc_firstbin;

    /* typically, we expect to execute this loop only once */
    while (searchsize < required && bin < MAXBINS)
    {
        if ((search = __malloc_rovers[bin++]) == 0)
            continue;

        if (search == __malloc_hiword - 1)
        {
            /* avoid final "wilderness" block */
            CHECKFREEPTR(search);
            search = NEXT(search);
        }
        start = search;
        do
        {
            CHECKFREEPTR(search);
            searchsize = FREESIZE(search);
            if (searchsize >= required)
                break;
            else
                search = NEXT(search);
        } while (search != start);
    }

    if (searchsize < required)
    {
        if (grabhunk(required) == 0)
        {
            errno = ENOMEM;
            return 0;
        }
        /*
         * We made sure in grabhunk() or free() that
         * __malloc_rovers[lastbin] is pointing to the newly sbrked
         * (and freed) block.
         */
        bin = __malloc_lastbin;
        search = __malloc_rovers[bin];
        searchsize = FREESIZE(search);
    }
    else if (bin > 0)
    {
        bin--;
    }
    CARVE(search, searchsize, bin, required);
    p = search - searchsize + 1;
    SIZEFIELD(p) = SIZEFIELD(p + required - 1) = ALLOCMASK(required);
    SET_REALSIZE(p, nbytes);
    return (void *)(p + HEADERWORDS);
}

#ifdef __MT__
/* allocate a data object on the heap */
void * __cdecl (malloc)(size_t nbytes)
{
    void *p = 0;

    __mtlock(_HEAP_LOCK);
    __try
    {
        p = _mt_malloc(nbytes);
    }
    __finally
    {
        __mtunlock(_HEAP_LOCK);
    }

    return p;
}
#endif /* __MT__ */

