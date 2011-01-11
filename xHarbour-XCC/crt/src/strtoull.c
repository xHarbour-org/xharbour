/****************************************************************************
 *                                                                          *
 * File    : strtoull.c                                                     *
 *                                                                          *
 * Purpose : strtoull function [new C99].                                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>

/* convert string to unsigned long long, with checking */
unsigned long long __cdecl (strtoull)(const char * restrict s, char ** restrict endptr, int base)
{
    return __stoull(s, endptr, base);
}

