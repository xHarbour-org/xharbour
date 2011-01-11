/****************************************************************************
 *                                                                          *
 * File    : strlwr.c                                                       *
 *                                                                          *
 * Purpose : _strlwr function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-05-25  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <string.h>
#include <ctype.h>

/* convert a string in place to lowercase */
char * __cdecl (_strlwr)(char *s)
{
    char *s1;

    if (s)
    {
        for (s1 = s; *s1 != '\0'; s1++)
            if (isupper(*s1)) *s1 = tolower(*s1);
    }

    return s;
}

