/****************************************************************************
 *                                                                          *
 * File    : strtoumax.c                                                    *
 *                                                                          *
 * Purpose : strtoumax function [new C99].                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stddef.h>
#include <inttypes.h>
#include <stdlib.h>

/* convert string to uintmax_t, with checking */
uintmax_t __cdecl (strtoumax)(const char * restrict s, char ** restrict endptr, int base)
{
    return __stoull(s, endptr, base);
}

