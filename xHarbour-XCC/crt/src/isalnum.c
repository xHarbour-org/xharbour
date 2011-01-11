/****************************************************************************
 *                                                                          *
 * File    : isalnum.c                                                      *
 *                                                                          *
 * Purpose : isalnum function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <ctype.h>

/* test for alphanumeric character */
int __cdecl (isalnum)(int c)
{
    return __ctypetab[c] & (_DIGIT|_LOWER|_UPPER);
}

