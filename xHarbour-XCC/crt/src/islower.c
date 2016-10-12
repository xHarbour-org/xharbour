/****************************************************************************
 *                                                                          *
 * File    : islower.c                                                      *
 *                                                                          *
 * Purpose : islower function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <ctype.h>

/* test for lowercase character */
int __cdecl (islower)(int c)
{
    return __ctypetab[c] & _LOWER;
}

