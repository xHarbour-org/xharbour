/****************************************************************************
 *                                                                          *
 * File    : toupper.c                                                      *
 *                                                                          *
 * Purpose : toupper function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <ctype.h>

/* convert to uppercase character */
int __cdecl (toupper)(int c)
{
    return __touppertab[c];
}

