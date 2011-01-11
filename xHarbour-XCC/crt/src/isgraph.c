/****************************************************************************
 *                                                                          *
 * File    : isgraph.c                                                      *
 *                                                                          *
 * Purpose : isgraph function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <ctype.h>

/* test for graphic character */
int __cdecl (isgraph)(int c)
{
    return __ctypetab[c] & (_DIGIT|_LOWER|_PUNCT|_UPPER);
}

