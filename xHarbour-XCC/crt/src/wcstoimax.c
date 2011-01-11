/****************************************************************************
 *                                                                          *
 * File    : wcstoimax.c                                                    *
 *                                                                          *
 * Purpose : wcstoimax function [new C99].                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xwchar.h"
#include <inttypes.h>

/* convert wide string to intmax_t, with checking */
intmax_t __cdecl (wcstoimax)(const wchar_t * restrict s, wchar_t ** restrict endptr, int base)
{
    return __wcstoll(s, endptr, base);
}

