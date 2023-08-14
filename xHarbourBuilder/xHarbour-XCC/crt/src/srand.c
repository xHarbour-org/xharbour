/****************************************************************************
 *                                                                          *
 * File    : srand.c                                                        *
 *                                                                          *
 * Purpose : srand function.                                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>
#include "xthread.h"

#ifndef __MT__
extern unsigned long __randseed;
extern int __randinit;
#endif /* __MT__ */

#ifdef __MT__
/* alter the seed */
void __cdecl (srand)(unsigned int seed)
{
    tiddata *mtd = __get_mtd();
    mtd->randinit = 0;
    mtd->randseed = seed;
}
#else /* __MT__ */
/* alter the seed */
void __cdecl (srand)(unsigned int seed)
{
    __randinit = 0;
    __randseed = seed;
}
#endif /* __MT__ */

