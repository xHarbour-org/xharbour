/****************************************************************************
 *                                                                          *
 * File    : strdup.c                                                       *
 *                                                                          *
 * Purpose : _strdup function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-05-25  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <string.h>
#include <stdlib.h>

/* duplicate string into allocated memory */
char * __cdecl (_strdup)(const char *s)
{
    char *s1;

    if (s != 0 && (s1 = malloc(strlen(s) + 1)) != 0)
        return strcpy(s1, s);

    return 0;
}

