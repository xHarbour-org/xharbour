/****************************************************************************
 *                                                                          *
 * File    : strstr.c                                                       *
 *                                                                          *
 * Purpose : strstr function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <string.h>

/* find first occurrence of s2[] in s1[] */
char * __cdecl (strstr)(const char *s1, const char *s2)
{
    if (*s2 == '\0')
        return (char *)s1;

    for (; (s1 = strchr(s1, *s2)) != 0; ++s1)
    {
        /* match rest of prefix */
        const char *sc1, *sc2;

        for (sc1 = s1, sc2 = s2;;)
        {
            if (*++sc2 == '\0')
                return (char *)s1;
            else if (*++sc1 != *sc2)
                break;
        }
    }

    return 0;
}

