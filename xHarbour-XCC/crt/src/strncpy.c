/****************************************************************************
 *                                                                          *
 * File    : strncpy.c                                                      *
 *                                                                          *
 * Purpose : strncpy function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <string.h>

/* copy char s2[max n] to s1[n] */
char * __cdecl (strncpy)(char * restrict s1, const char * restrict s2, size_t n)
{
    char *s;

    /* copy at most n chars from s2[] */
    for (s = s1; n > 0 && *s2 != '\0'; --n)
        *s++ = *s2++;
    for (; n > 0; --n)
        *s++ = '\0';

    return s1;
}

