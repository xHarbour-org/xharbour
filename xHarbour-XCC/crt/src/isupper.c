/****************************************************************************
 *                                                                          *
 * File    : isupper.c                                                      *
 *                                                                          *
 * Purpose : isupper function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <ctype.h>

/* test for uppercase character */
int __cdecl (isupper)(int c)
{
    return __ctypetab[c] & _UPPER;
}

