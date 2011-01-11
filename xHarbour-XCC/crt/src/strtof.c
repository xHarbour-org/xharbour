/****************************************************************************
 *                                                                          *
 * File    : strtof.c                                                       *
 *                                                                          *
 * Purpose : strtof function [new C99].                                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>

/* convert string to float, with checking */
float __cdecl (strtof)(const char * restrict s, char ** restrict endptr)
{
    return __stof(s, endptr, 0);
}

