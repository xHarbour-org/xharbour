/****************************************************************************
 *                                                                          *
 * File    : rand.c                                                         *
 *                                                                          *
 * Purpose : rand function.                                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-12  Rewritten.                                           *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>
#include "xthread.h"

#define TSIZ  32      /* must be power of two */
#define TMSK  (TSIZ - 1)
#define XRND(x)  (x) * 1664525L + 1013904223L

#ifdef __MT__
/* compute pseudo-random value */
int __cdecl (rand)(void)
{
    tiddata *mtd = __get_mtd();
    int j;

    if (mtd->randarr == 0)
        mtd->randarr = (unsigned long *)malloc(32 * sizeof(unsigned long));

    if (mtd->randinit == 0)
    {
        /* warm up, then initialize shuffle table */
        for (j = 0; j < 8; ++j)
            mtd->randseed = XRND(mtd->randseed);
        for (j = 0; j < TSIZ; ++j)
            mtd->randarr[j] = (mtd->randseed = XRND(mtd->randseed));
        mtd->randidx = mtd->randarr[TSIZ - 1];
        mtd->randinit = 1;
    }

    mtd->randseed = XRND(mtd->randseed);
    j = mtd->randidx & TMSK;
    mtd->randidx = mtd->randarr[j];
    mtd->randarr[j] = mtd->randseed;

    return mtd->randidx & RAND_MAX;
}
#else /* __MT__ */
extern int __randinit = 0;
extern unsigned long __randseed = 1;

/* compute pseudo-random value */
int __cdecl (rand)(void)
{
    static unsigned long idx = 0;
    static unsigned long rv[32];
    int j;

    if (__randinit == 0)
    {
        /* warm up, then initialize shuffle table */
        for (j = 0; j < 8; ++j)
            __randseed = XRND(__randseed);
        for (j = 0; j < TSIZ; ++j)
            rv[j] = (__randseed = XRND(__randseed));
        idx = rv[TSIZ - 1];
        __randinit = 1;
    }

    __randseed = XRND(__randseed);
    j = idx & TMSK;
    idx = rv[j];
    rv[j] = __randseed;

    return idx & RAND_MAX;
}
#endif /* __MT__ */
