/****************************************************************************
 *                                                                          *
 * File    : strtoimax.c                                                    *
 *                                                                          *
 * Purpose : strtoimax function (new C99).                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stddef.h>
#include <inttypes.h>
#include <stdlib.h>

/* convert string to intmax_t, with checking */
intmax_t __cdecl (strtoimax)(const char * restrict s, char ** restrict endptr, int base)
{
    return (intmax_t)__stoll(s, endptr, base);
}

