/****************************************************************************
 *                                                                          *
 * File    : strchr.c                                                       *
 *                                                                          *
 * Purpose : strchr function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <string.h>

/* find first occurrence of c in char s[] */
char * __cdecl (strchr)(const char *s, int c)
{
    const char ch = c;

    for (; *s != ch; ++s)
        if (*s == '\0') return 0;

    return (char *)s;
}

