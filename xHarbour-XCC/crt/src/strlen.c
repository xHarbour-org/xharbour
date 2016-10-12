/****************************************************************************
 *                                                                          *
 * File    : strlen.c                                                       *
 *                                                                          *
 * Purpose : strlen function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <string.h>

/* find length of s[] */
size_t __cdecl (strlen)(const char *s)
{
    const char *sc;

    for (sc = s; *sc != '\0'; ++sc)
        ;

    return sc - s;
}

