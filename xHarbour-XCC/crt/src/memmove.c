/****************************************************************************
 *                                                                          *
 * File    : memmove.c                                                      *
 *                                                                          *
 * Purpose : memmove function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <string.h>

/* copy char s2[n] to s1[n] safely */
void * __cdecl (memmove)(void *s1, const void *s2, size_t n)
{
    char *sc1 = (char *)s1;
    const char *sc2 = (const char *)s2;

    if (sc2 < sc1 && sc1 < sc2 + n)
    {
        for (sc1 += n, sc2 += n; n > 0; --n)
            *--sc1 = *--sc2;    /* copy backwards */
    }
    else
    {
        for (; n > 0; --n)
            *sc1++ = *sc2++;    /* copy forwards */
    }

    return s1;
}

