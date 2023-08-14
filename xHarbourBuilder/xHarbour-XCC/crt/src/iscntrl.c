/****************************************************************************
 *                                                                          *
 * File    : iscntrl.c                                                      *
 *                                                                          *
 * Purpose : iscntrl function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <ctype.h>

/* test for control character */
int __cdecl (iscntrl)(int c)
{
    return __ctypetab[c] & _CNTRL;
}

