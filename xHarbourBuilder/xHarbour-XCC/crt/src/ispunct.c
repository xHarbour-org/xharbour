/****************************************************************************
 *                                                                          *
 * File    : ispunct.c                                                      *
 *                                                                          *
 * Purpose : ispunct function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <ctype.h>

/* test for punctuation character */
int __cdecl (ispunct)(int c)
{
    return __ctypetab[c] & _PUNCT;
}

