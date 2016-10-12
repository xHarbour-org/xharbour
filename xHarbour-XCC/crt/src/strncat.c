/****************************************************************************
 *                                                                          *
 * File    : strncat.c                                                      *
 *                                                                          *
 * Purpose : strncat function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <string.h>

/* copy char s2[max n] to end of s1[] */
char * __cdecl (strncat)(char * restrict s1, const char * restrict s2, size_t n)
{
    char *s;

    /* find end of s1[] */
    for (s = s1; *s != '\0'; ++s)
        ;

    /* copy at most n chars from s2[] */
    for (; n > 0 && *s2 != '\0'; --n)
        *s++ = *s2++;

    *s = '\0';
    return s1;
}

