/****************************************************************************
 *                                                                          *
 * File    : isblank.c                                                      *
 *                                                                          *
 * Purpose : isblank function [new C99].                                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <ctype.h>

/* test for blank character */
int __cdecl (isblank)(int c)
{
    return __ctypetab[c] & (_SPACE|_BLANK);
}

