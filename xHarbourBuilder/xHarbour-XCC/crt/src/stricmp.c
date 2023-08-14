/****************************************************************************
 *                                                                          *
 * File    : stricmp.c                                                      *
 *                                                                          *
 * Purpose : _stricmp function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <string.h>
#include <ctype.h>

/* case-insensitive compare unsigned char s1[], s2[] */
int __cdecl (_stricmp)(const char *s1, const char *s2)
{
    int c1, c2;

    do
    {
        c1 = tolower((unsigned char)*s1), s1++;
        c2 = tolower((unsigned char)*s2), s2++;
    } while (c1 && c1 == c2);

    /* return (c1 < c2) ? -1 : (c1 > c2) ? +1 : 0; */
    return c1 - c2;
}

