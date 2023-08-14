/****************************************************************************
 *                                                                          *
 * File    : strupr.c                                                       *
 *                                                                          *
 * Purpose : _strupr function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-05-25  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <string.h>
#include <ctype.h>

/* convert a string in place to uppercase */
char * __cdecl (_strupr)(char *s)
{
    char *s1;

    if (s)
    {
        for (s1 = s; *s1 != '\0'; s1++)
            if (islower(*s1)) *s1 = toupper(*s1);
    }

    return s;
}

