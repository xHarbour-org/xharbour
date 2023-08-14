/****************************************************************************
 *                                                                          *
 * File    : getchar.c                                                      *
 *                                                                          *
 * Purpose : getchar function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xstdio.h"

/* get a character from stdin */
int __cdecl (getchar)(void)
{
    return fgetc(stdin);
}

