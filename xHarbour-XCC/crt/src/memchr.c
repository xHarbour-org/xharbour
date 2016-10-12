/****************************************************************************
 *                                                                          *
 * File    : memchr.c                                                       *
 *                                                                          *
 * Purpose : memchr function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <string.h>

/* find first occurrence of c in s[n] */
void * __cdecl (memchr)(const void *s, int c, size_t n)
{
    const unsigned char uc = c;
    const unsigned char *su = (const unsigned char *)s;

    for (; n > 0; ++su, --n)
    {
        if (*su == uc)
            return (void *)su;
    }

    return 0;
}

