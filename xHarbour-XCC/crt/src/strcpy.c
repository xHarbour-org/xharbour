/****************************************************************************
 *                                                                          *
 * File    : strcpy.c                                                       *
 *                                                                          *
 * Purpose : strcpy function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <string.h>

/* copy char s2[] to s1[] */
char * __cdecl (strcpy)(char * restrict s1, const char * restrict s2)
{
    char *s;

    for (s = s1; (*s++ = *s2++) != '\0'; )
        ;

    return s1;
}

