#ifndef _XALLOC_H
#define _XALLOC_H

#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <assert.h>
#include <errno.h>

/* xalloc.h - internal header */

/*
 * 'union word' must be aligned to the most pessimistic alignment needed
 * on the architecture since all pointers returned by malloc and friends
 * are really pointers to a Word. This is the job of the 'foo' field in
 * the union, which actually decides the size.
 *
 * 'union word' should also be the size necessary to ensure that an
 * sbrk() immediately following an sbrk(sizeof(union word) * n) returns
 * an address one higher than the first sbrk. i.e. contiguous space from
 * successive sbrks. (Most sbrks will do this regardless of the size -
 * Sun's doesnt) This is not vital - the malloc will work, but will not
 * be able to coalesce between sbrk'ed segments.
 */

/* This default seems to work on most 32bit machines */
#ifndef ALIGN
#define ALIGN long foo
#define NALIGN 4
#endif

/* Align with power of 2 */
#define SIMPLEALIGN(X,N)  (void *)(((size_t)((char *)(X) + (N-1))) & ~(N-1))

#if NALIGN == 2 || NALIGN == 4 || NALIGN == 8 || NALIGN == 16
/* if NALIGN is a power of 2, the next line will do ok */
#define ALIGNPTR(X)  SIMPLEALIGN(X, NALIGN)
#else
/* otherwise we need the generic version; hope the compiler isn't too smart */
static size_t _N = NALIGN;
#define ALIGNPTR(X)  (void *)((((size_t)((void *)(X)+(NALIGN-1)))/_N)*_N)
#endif

/*
 * If your sbrk does not return blocks that are aligned to the
 * requirements of malloc(), as specified by ALIGN above, then you need
 * to define SBRKEXTRA so that it gets the extra memory needed to find
 * an alignment point. Not needed on Suns on which sbrk does return
 * properly aligned blocks.
 */
#ifdef __USE_SAFE_ALIGNMENT__
#define SBRKEXTRA       (sizeof(Word) - 1)
#define SBRKALIGN(x)    ALIGNPTR(x)
#else
#define SBRKEXTRA       0
#define SBRKALIGN(x)    (x)
#endif

#define BITSPERBYTE     CHAR_BIT
#define BITS(type)      (BITSPERBYTE * (int)sizeof(type))

/* size_t with only the high-order bit turned on */
#define HIBITSZ (((size_t)1) << (BITS(size_t) - 1))

/*
 * We assume that FREE is a 0 bit, and the tag for a free block, Or'ing the
 * tag with ALLOCED should turn on the high bit, And'ing with SIZEMASK
 * should turn it off.
 */

#define FREE            ((size_t)0)
#define ALLOCED         (HIBITSZ)
#define SIZEMASK        (~HIBITSZ)

#define MAXBINS         8

#define DEF_SBRKUNITS   1024

#define MALLOC_MINCHUNK FIXEDOVERHEAD

union word {  /* basic unit of storage */
    size_t size;            /* size of this block + 1 bit status */
    union word *next;       /* next free block */
    union word *prev;       /* prev free block */
    void *ptr;              /* stops lint complaining, keeps alignment */
    char c;
    int i;
    char *cp;
    char **cpp;
    int *ip;
    int **ipp;
    ALIGN;                  /* alignment stuff - wild fun */
};

typedef union word Word;

/*
 *  WARNING - Many of these macros are UNSAFE because they have multiple
 *  evaluations of the arguments. Use with care, avoid side-effects.
 */
/*
 * These macros define operations on a pointer to a block. The zero'th
 * word of a block is the size field, where the top bit is 1 if the
 * block is allocated. This word is also referred to as the starting tag.
 * The last word of the block is identical to the zero'th word of the
 * block and is the end tag.  IF the block is free, the second last word
 * is a pointer to the next block in the free list (a doubly linked list
 * of all free blocks in the heap), and the third from last word is a
 * pointer to the previous block in the free list.  HEADERWORDS is the
 * number of words before the pointer in the block that malloc returns,
 * TRAILERWORDS is the number of words after the returned block. Note
 * that the zero'th and the last word MUST be the boundary tags - this
 * is hard-wired into the algorithm. Increasing HEADERWORDS or
 * TRAILERWORDS suitably should be accompanied by additional macros to
 * operate on those words. The routines most likely to be affected are
 * malloc/realloc/free/heap_verify/heap_dump.
 */
/*
 * There are two ways we can refer to a block -- by pointing at the
 * start tag, or by pointing at the end tag. For reasons of efficiency
 * and performance, free blocks are always referred to by the end tag,
 * while allocated blocks are usually referred to by the start tag.
 * Accordingly, the following macros indicate the type of their argument
 * by using either 'p', 'sp', or 'ep' to indicate a pointer. 'p' means
 * the pointer could point at either the start or end tag. 'sp' means it
 * must point at a start tag for that macro to work correctly, 'ep'
 * means it must point at the end tag. Usually, macros operating on free
 * blocks (NEXT, PREV, VALID_PREV_PTR, VALID_NEXT_PTR) take 'ep', while
 * macros operating on allocated blocks (REALSIZE, MAGIC_PTR,
 * SET_REALSIZE) take 'sp'. The size field may be validated using either
 * VALID_START_SIZE_FIELD for an 'sp' or VALID_END_SIZE_FIELD for an
 * 'ep'.
 */
/*
 * SIZE, SIZEFIELD and TAG are valid for allocated and free blocks,
 * REALSIZE is valid for allocated blocks when debugging, and NEXT and
 * PREV are valid for free blocks. We could speed things up by making
 * the free list singly linked when not debugging - the double link are
 * just so we can check for pointer validity. (PREV(NEXT(ep)) == ep and
 * NEXT(PREV(ep)) == ep). FREESIZE is used only to get the size field
 * from FREE blocks - in this implementation, free blocks have a tag
 * bit of 0 so no masking is necessary to operate on the SIZEFIELD when
 * a block is free. We take advantage of that as a minor optimization.
 */
#define SIZEFIELD(p)        ((p)->size)
#define SIZE(p)             ((size_t)(SIZEFIELD(p) & SIZEMASK))
#define ALLOCMASK(n)        ((n) | ALLOCED)
#define FREESIZE(p)         SIZEFIELD(p)
/*
 * FREEMASK should be (n) & SIZEMASK, but since (n) will always have
 * the hi bit off after the conversion from bytes requested by the user
 * to words.
 */
#define FREEMASK(n)         (n)
#define TAG(p)              (SIZEFIELD(p) & ~SIZEMASK)

#ifndef NDEBUG
#define REALSIZE(sp)        (((sp)+1)->size)
#endif

#define NEXT(ep)            (((ep)-1)->next)
#define PREV(ep)            (((ep)-2)->prev)

/*
 * HEADERWORDS is the real block header in an allocated block - the
 * free block header uses extra words in the block itself
 */
#ifndef NDEBUG
#define HEADERWORDS         2       /* Start boundary tag + real size in bytes */
#else
#define HEADERWORDS         1       /* Start boundary tag */
#endif

#define TRAILERWORDS        1
#define FREEHEADERWORDS     1       /* Start boundary tag */
#define FREETRAILERWORDS    3       /* next and prev, end boundary tag */

#define ALLOC_OVERHEAD  (HEADERWORDS + TRAILERWORDS)
#define FREE_OVERHEAD   (FREEHEADERWORDS + FREETRAILERWORDS)

/*
 * The allocator views memory as a list of non-contiguous arenas. (If
 * successive sbrks() return contiguous blocks, they are colaesced into
 * one arena - if a program never calls sbrk() other than malloc(),
 * then there should only be one arena. This malloc will however
 * happily coexist with other allocators calling sbrk() and uses only
 * the blocks given to it by sbrk. It expects the same courtesy from
 * other allocators. The arenas are chained into a linked list using
 * the first word of each block as a pointer to the next arena. The
 * second word of the arena, and the last word, contain fake boundary
 * tags that are permanantly marked allocated, so that no attempt is
 * made to coalesce past them. See the code in dumpheap for more info.
 */
#define ARENASTART          2       /* next ptr + fake start tag */

#ifndef NDEBUG
/* 1 for prev link in free block - next link is absorbed by header REALSIZE word */
#define FIXEDOVERHEAD       (1 + ALLOC_OVERHEAD)
#else
/* 1 for prev link, 1 for next link, + header and trailer */
#define FIXEDOVERHEAD       (2 + ALLOC_OVERHEAD)
#endif

/*
 * Check that pointer is safe to dereference i.e. actually points
 * somewhere within the heap and is properly aligned.
 */
#define PTR_IN_HEAP(p) \
    ((p) > __malloc_loword && (p) < __malloc_hiword && \
     ALIGNPTR(p) == ((void *)(p)))

/* Check that the size field is valid */
#define VALID_START_SIZE_FIELD(sp) \
    (PTR_IN_HEAP((sp) + SIZE(sp) - 1) && \
     SIZEFIELD(sp) == SIZEFIELD((sp) + SIZE(sp) - 1))

#define VALID_END_SIZE_FIELD(ep) \
    (PTR_IN_HEAP((ep) - SIZE(ep) + 1) && \
     SIZEFIELD(ep) == SIZEFIELD((ep) - SIZE(ep) + 1))

#ifndef NDEBUG
/*
 * Byte that is stored at the end of each block if the requested size
 * of the block is not a multiple of sizeof(Word). (If it is a multiple
 * of sizeof(Word), then we don't need to put the magic because the
 * endboundary tag will be corrupted and the tests that check the
 * validity of the boundary tag should detect it.
 */
#define MAGIC_BYTE  ((unsigned char)'\252')

/*
 * Check if size of the block is identical to requested size. Typical
 * tests will be of the form DONT_NEED_MAGIC(p) || something for
 * short-circuited protection, because blocks where DONT_NEED_MAGIC is
 * true will be tested for boundary tag detection so we don't need the
 * magic byte at the end.
 */
#define DONT_NEED_MAGIC(sp) \
    (REALSIZE(sp) == ((SIZE(sp) - ALLOC_OVERHEAD) * sizeof(Word)))

/* must not be used if either DONT_NEED_MAGIC or TOO_SMALL are true */
#define VALID_REALSIZE(sp) \
    (REALSIZE(sp) < ((SIZE(sp) - ALLOC_OVERHEAD) * sizeof(Word)))

/* Location of the magic byte */
#define MAGIC_PTR(sp) \
    ((unsigned char *)((sp) + HEADERWORDS) + REALSIZE(sp))

/*
 * malloc code should only use the next three macros SET_REALSIZE,
 * VALID_MAGIC and TOO_SMALL, since they are the only ones which have
 * non-DEBUG (null) alternatives.
 */

/* Macro sets the realsize of a block if necessary */
#define SET_REALSIZE(sp, n) \
    (TOO_SMALL(sp) ? 0 : (REALSIZE(sp) = (n), DONT_NEED_MAGIC(sp) || (*MAGIC_PTR(sp) = MAGIC_BYTE)))

/* Macro tests that the magic byte is valid if it exists */
#define VALID_MAGIC(sp) \
    (TOO_SMALL(sp) || DONT_NEED_MAGIC(sp) || (VALID_REALSIZE(sp) && (*MAGIC_PTR(sp) == MAGIC_BYTE)))

/*
 * Bytes left over memalign may be too small to hold the real size, they
 * may be 1 word, i.e start and end tag stored in same word!
 */
#define TOO_SMALL(sp) \
    (SIZE(sp) < ALLOC_OVERHEAD)

#else /* NDEBUG */
#define SET_REALSIZE(sp,n)
#define VALID_MAGIC(sp)     (1)
#define TOO_SMALL(sp)       (0)
#endif /* NDEBUG */

/*
 *  Check that a free list ptr points to a block with something pointing
 *  back. This is the only reason we use a doubly linked free list.
 */
#define VALID_NEXT_PTR(ep)  (PTR_IN_HEAP(NEXT(ep)) && PREV(NEXT(ep)) == (ep))
#define VALID_PREV_PTR(ep)  (PTR_IN_HEAP(PREV(ep)) && NEXT(PREV(ep)) == (ep))

/* quick bit-arithmetic to check a number (including 1) is a power of two */
#define is_power_of_2(x)  ((((x) - 1) & (x)) == 0)

#ifndef NDEBUG
#define CHECKHEAP() \
    heap_verify(1);

#define CHECKFREEPTR(ep) \
    { \
        assert(PTR_IN_HEAP(ep));  /* pointer outside heap */  \
        assert(TAG(ep) == FREE);  /* already-allocated block */  \
        assert(VALID_END_SIZE_FIELD(ep));  /* corrupt block */  \
        assert(VALID_NEXT_PTR(ep));  /* corrupt link to next free block */  \
        assert(VALID_PREV_PTR(ep));  /* corrupt link to previous free block */  \
    } \

#define CHECKALLOCPTR(sp) \
    { \
        assert(PTR_IN_HEAP(sp));  /* pointer outside heap */  \
        assert(TAG(sp) != FREE);  /* already-freed block */  \
        assert(VALID_START_SIZE_FIELD(sp));  /* corrupt block */  \
        assert(VALID_MAGIC(sp));  /* block with end overwritten */  \
    }
#else /* NDEBUG */
#define CHECKHEAP()
#define CHECKFREEPTR(ep)
#define CHECKALLOCPTR(sp)
#endif /* NDEBUG */

#define FREEMAGIC   '\125'

/*
 *  Memory functions but in words. We just call memset/memcpy, and hope
 *  that someone has optimized them. If you are on pure 4.2BSD, either
 *  redefine these in terms of bcopy/your own memcpy, or
 *  get the functions from one of 4.3src/Henry Spencer's strings
 *  package/C News src
 */
#define MEMSET(p, c, n)     (void)memset((void *)(p), (c), (size_t)((n) * sizeof(Word)))
#define MEMCPY(p1, p2, n)   (void)memcpy((void *)(p1), (void *)(p2), (size_t)((n) * sizeof(Word)))

#ifndef NDEBUG
#define DMEMSET(p, n)  MEMSET((p), FREEMAGIC, (n))
#else
#define DMEMSET(p, n)
#endif

/*
 * GETBIN, UNLINK, LINK and CARVE are free-list maintenance macros used in
 * several places.  A free-list is a doubly-linked list of non-contiguous
 * blocks, marked by boundary tags that indicate the size.
 */

/* GETBIN returns a number such that i <= __malloc_binmax[bin] */
#define GETBIN(i)   \
    (((i) <= __malloc_binmax[3]) ? \
        (((i) <= __malloc_binmax[1]) ? \
            (((i) <= __malloc_binmax[0]) ? 0 : 1) \
        : \
            (((i) <= __malloc_binmax[2]) ? 2 : 3) \
        ) \
    : \
        (((i) <= __malloc_binmax[5]) ? \
            (((i) <= __malloc_binmax[4]) ? 4 : 5) \
        : \
            (((i) <= __malloc_binmax[6]) ? 6 : 7) \
        ) \
    )

/* UNLINK removes the block 'ep' from the free list 'epbin' */
#define UNLINK(ep, epbin) \
    { \
        register Word *epnext = NEXT(ep); \
        if (ep == epnext) \
        { \
            __malloc_rovers[epbin] = NULL; \
            if (__malloc_firstbin == epbin) \
                while (!__malloc_rovers[__malloc_firstbin] && __malloc_firstbin < MAXBINS-1) \
                    __malloc_firstbin++; \
        } \
        else \
        { \
            register Word *epprev = PREV(ep); \
            NEXT(epprev) = epnext; \
            PREV(epnext) = epprev; \
            if (ep == __malloc_rovers[epbin]) \
                __malloc_rovers[epbin] = epprev; \
        } \
    }

/*
 * LINK adds the block 'ep' (psize words) to the free list 'epbin',
 * immediately after the block pointed to by that bin's rover.
 */
#define LINK(ep, epsize, epbin) \
    { \
        register Word *epprev; \
        register Word *eprover = __malloc_rovers[epbin]; \
        \
        if (eprover == NULL) \
        { \
            __malloc_rovers[epbin] = eprover = epprev = ep; \
            if (__malloc_firstbin > epbin) \
                __malloc_firstbin = epbin; \
        } \
        else \
        { \
            CHECKFREEPTR(eprover); \
            epprev = PREV(eprover); \
        } \
        NEXT(ep) = eprover; \
        PREV(eprover) = ep; \
        NEXT(epprev) = ep; \
        PREV(ep) = epprev; /* PREV(eprover) */  \
        SIZEFIELD(ep) = SIZEFIELD(ep-epsize+1) = FREEMASK(epsize); \
    }

#define CARVE(ep, epsize, epbin, reqsize) \
    { \
        register size_t eprest = epsize - reqsize; \
        int newepbin; \
        \
        if (eprest >= MALLOC_MINCHUNK) \
        { \
            newepbin = GETBIN(eprest); \
            if (newepbin != epbin) \
            { \
                UNLINK(ep, epbin); \
                LINK(ep, eprest, newepbin); \
            } \
            else \
            { \
                SIZEFIELD(ep) = SIZEFIELD(ep-eprest+1) = FREEMASK(eprest); \
            } \
        } \
        else \
        { \
            /* alloc the entire block */ \
            UNLINK(ep, epbin); \
            reqsize = epsize; \
        } \
    }

extern void heap_verify(int);
extern void *__getmem(size_t);

extern Word *__malloc_rovers[];
extern Word *__malloc_hiword;
extern Word *__malloc_loword;
extern Word *__malloc_mem;
extern const short __malloc_binmax[];
extern int __malloc_firstbin;
extern int __malloc_lastbin;

#endif /* _XALLOC_H */

