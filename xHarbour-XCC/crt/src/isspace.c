/****************************************************************************
 *                                                                          *
 * File    : isspace.c                                                      *
 *                                                                          *
 * Purpose : isspace function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <ctype.h>

/* test for spacing character */
int __cdecl (isspace)(int c)
{
    return __ctypetab[c] & (_WHITE|_SPACE);
}

