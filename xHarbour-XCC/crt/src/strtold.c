/****************************************************************************
 *                                                                          *
 * File    : strtold.c                                                      *
 *                                                                          *
 * Purpose : strtold function (new C99).                                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>

/* convert string to long double, with checking */
long double __cdecl (strtold)(const char * restrict s, char ** restrict endptr)
{
    return __stold(s, endptr, 0);
}

