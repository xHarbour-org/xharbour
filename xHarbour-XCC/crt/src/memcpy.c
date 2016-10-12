/****************************************************************************
 *                                                                          *
 * File    : memcpy.c                                                       *
 *                                                                          *
 * Purpose : memcpy function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <string.h>

/* copy char s2[n] to s1[n] in any order */
void * __cdecl (memcpy)(void * restrict s1, const void * restrict s2, size_t n)
{
    char *su1 = (char *)s1;
    const char *su2 = (const char *)s2;

    for (; n > 0; ++su1, ++su2, --n)
        *su1 = *su2;

    return s1;
}

