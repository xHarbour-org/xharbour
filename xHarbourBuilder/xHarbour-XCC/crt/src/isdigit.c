/****************************************************************************
 *                                                                          *
 * File    : isdigit.c                                                      *
 *                                                                          *
 * Purpose : isdigit function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <ctype.h>

/* test for digit */
int __cdecl (isdigit)(int c)
{
    return __ctypetab[c] & _DIGIT;
}

