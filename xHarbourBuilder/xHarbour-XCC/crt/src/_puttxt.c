/****************************************************************************
 *                                                                          *
 * File    : _puttxt.c                                                      *
 *                                                                          *
 * Purpose : __puttxt function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-09  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include "xstdio.h"

/* macros */
#define MAX_PAD  (sizeof(spaces) - 1)

#define PAD(px, s, n, ns)  \
    if ((n) > 0) \
    { \
        int i, j = (n); \
        for (; j > 0; j -= i) \
        { \
            i = (ns) < j ? (ns) : j; \
            { PUT(px, s, i) } \
        } \
    }  /* pad n elements from s[ns] */

#define PUT(px, s, n)  \
    if ((n) > 0) \
        if (((px)->arg = (*(px)->pfn)((px)->arg, s, n)) != 0) \
            (px)->nchar += (n); \
        else \
            return EOF;  /* put n elements from s */

/* static data */
static const char spaces[] = "                                ";
static const char zeroes[] = "00000000000000000000000000000000";

/* print generated text plus padding */
int __puttxt(__prtinfo *px, const char *ac)
{
    int width = px->width - px->n0 - px->nz0 - px->n1 - px->nz1 - px->n2 - px->nz2;

    if (!(px->flags & _FMI))
        { PAD(px, &spaces[0], width, MAX_PAD) }

    { PUT(px, ac, px->n0) }
    { PAD(px, &zeroes[0], px->nz0, MAX_PAD) }
    { PUT(px, px->s, px->n1) }
    { PAD(px, &zeroes[0], px->nz1, MAX_PAD) }
    { PUT(px, px->s + px->n1, px->n2) }
    { PAD(px, &zeroes[0], px->nz2, MAX_PAD) }

    if (px->flags & _FMI)
        { PAD(px, &spaces[0], width, MAX_PAD) }

    return 0;
}

