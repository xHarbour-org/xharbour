/****************************************************************************
 *                                                                          *
 * File    : perror.c                                                       *
 *                                                                          *
 * Purpose : perror function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <errno.h>
#include <string.h>
#include "xstdio.h"

char * __cdecl _Strerror(int, char *);

/* put error string to stderr */
void __cdecl (perror)(const char *s)
{
    if (s != 0 && *s != '\0')
    {
        /* put user-supplied prefix */
        fputs(s, stderr);
        fputs(": ", stderr);
    }
    fputs(_Strerror(errno, 0), stderr);
    fputc('\n', stderr);
}

