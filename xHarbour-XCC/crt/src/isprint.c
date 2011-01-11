/****************************************************************************
 *                                                                          *
 * File    : isprint.c                                                      *
 *                                                                          *
 * Purpose : isprint function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <ctype.h>

/* test for printable character */
int __cdecl (isprint)(int c)
{
    return __ctypetab[c] & (_DIGIT|_LOWER|_PUNCT|_SPACE|_UPPER);
}

