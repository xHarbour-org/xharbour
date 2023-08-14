/****************************************************************************
 *                                                                          *
 * File    : tolower.c                                                      *
 *                                                                          *
 * Purpose : tolower function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <ctype.h>

/* convert to lowercase character */
int __cdecl (tolower)(int c)
{
    return __tolowertab[c];
}

