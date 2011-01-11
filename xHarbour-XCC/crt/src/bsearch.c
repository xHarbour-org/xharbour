/****************************************************************************
 *                                                                          *
 * File    : bsearch.c                                                      *
 *                                                                          *
 * Purpose : bsearch function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>

/* search sorted table by binary chop */
void * __cdecl (bsearch)(const void *key, const void *base, size_t nelem, size_t size, __cmpfunc *cmp)
{
    const char *p;
    size_t n;

    /* check midpoint of whatever is left */
    for (p = (const char *)base, n = nelem; n > 0; )
    {
        const size_t pivot = n >> 1;
        const char *const q = p + size * pivot;
        const int val = (*cmp)(key, q);

        if (val < 0)
            n = pivot;  /* search below pivot */
        else if (val == 0)
            return (void *)q;  /* found */
        else
        {
            /* search above pivot */
            p = q + size;
            n -= pivot + 1;
        }
    }

    return 0;  /* no match */
}

