/****************************************************************************
 *                                                                          *
 * File    : isalpha.c                                                      *
 *                                                                          *
 * Purpose : isalpha function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <ctype.h>

/* test for alphabetic character */
int __cdecl (isalpha)(int c)
{
    return __ctypetab[c] & (_LOWER|_UPPER);
}

