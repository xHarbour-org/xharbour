/****************************************************************************
 *                                                                          *
 * File    : strcmp.c                                                       *
 *                                                                          *
 * Purpose : strcmp function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <string.h>

/* compare unsigned char s1[], s2[] */
int __cdecl (strcmp)(const char *s1, const char *s2)
{
    for (; *s1 == *s2; ++s1, ++s2)
    {
        if (*s1 == '\0')
            return 0;
    }

    return (*(unsigned char *)s1 < *(unsigned char *)s2) ? -1 : +1;
}

