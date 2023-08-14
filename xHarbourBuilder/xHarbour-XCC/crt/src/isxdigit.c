/****************************************************************************
 *                                                                          *
 * File    : isxdigit.c                                                     *
 *                                                                          *
 * Purpose : isxdigit function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <ctype.h>

/* test for hexadecimal digit */
int __cdecl (isxdigit)(int c)
{
    return __ctypetab[c] & _HEX;
}

