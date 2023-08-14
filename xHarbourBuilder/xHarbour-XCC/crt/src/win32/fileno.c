/****************************************************************************
 *                                                                          *
 * File    : fileno.c                                                       *
 *                                                                          *
 * Purpose : lowio _fileno function -- win32 version.                       *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-03-31  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stdio.h>

/* remove macro definition for fileno() */
#undef  _fileno

/* return the file handle for the specified stream */
int __cdecl _fileno(FILE *str)
{
    return str->fh;
}

