/****************************************************************************
 *                                                                          *
 * File    : memset.c                                                       *
 *                                                                          *
 * Purpose : memset function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <string.h>

/* store c throughout unsigned char s[n] */
void * __cdecl (memset)(void *s, int c, size_t n)
{
    const unsigned char uc = c;
    unsigned char *su = (unsigned char *)s;

    for (; n > 0; ++su, --n)
        *su = uc;

    return s;
}

