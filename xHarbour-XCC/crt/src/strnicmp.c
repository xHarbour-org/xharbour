/****************************************************************************
 *                                                                          *
 * File    : strnicmp.c                                                     *
 *                                                                          *
 * Purpose : _strnicmp function.                                            *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <string.h>
#include <ctype.h>

/* case-insensitive compare unsigned char s1[max n], s2[max n] */
int __cdecl (_strnicmp)(const char *s1, const char *s2, size_t n)
{
    for (; n > 0; s1++, s2++, n--)
    {
        int c1 = tolower((unsigned char)*s1);
        int c2 = tolower((unsigned char)*s2);

        if (c1 != c2)
            return (c1 < c2) ? -1 : +1;
        else if (c1 == '\0')
            return 0;
    }

    return 0;
}

