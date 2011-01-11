/****************************************************************************
 *                                                                          *
 * File    : strcat.c                                                       *
 *                                                                          *
 * Purpose : strcat function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <string.h>

/* copy char s2[] to end of s1[] */
char * __cdecl (strcat)(char * restrict s1, const char * restrict s2)
{
    char *s;

    /* find end of s1[] */
    for (s = s1; *s != '\0'; ++s)
        ;
    /* copy s2[] to end */
    for (; (*s = *s2) != '\0'; ++s, ++s2)
        ;

    return s1;
}

