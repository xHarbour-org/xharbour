/****************************************************************************
 *                                                                          *
 * File    : memcmp.c                                                       *
 *                                                                          *
 * Purpose : memcmp function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <string.h>

/* compare unsigned char s1[n], s2[n] */
int __cdecl (memcmp)(const void *s1, const void *s2, size_t n)
{
    const unsigned char *su1 = (const unsigned char *)s1;
    const unsigned char *su2 = (const unsigned char *)s2;

    for (; n > 0; ++su1, ++su2, --n)
    {
        if (*su1 != *su2)
            return (*su1 < *su2) ? -1 : +1;
    }

    return 0;
}

