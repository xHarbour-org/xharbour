/****************************************************************************
 *                                                                          *
 * File    : strrchr.c                                                      *
 *                                                                          *
 * Purpose : strrchr function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <string.h>

/* find last occurrence of c in char s[] */
char * __cdecl (strrchr)(const char *s, int c)
{
    const char ch = c;
    const char *sc;

    for (sc = 0;; ++s)
    {
        /* check another char */
        if (*s == ch)
            sc = s;

        if (*s == '\0')
            return (char *)sc;
    }
}

