/****************************************************************************
 *                                                                          *
 * File    : strtod.c                                                       *
 *                                                                          *
 * Purpose : strtod function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>

/* convert string to double, with checking */
double __cdecl (strtod)(const char * restrict s, char ** restrict endptr)
{
    return __stod(s, endptr, 0);
}

