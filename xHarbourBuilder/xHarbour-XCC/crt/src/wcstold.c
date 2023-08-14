/****************************************************************************
 *                                                                          *
 * File    : wcstold.c                                                      *
 *                                                                          *
 * Purpose : wcstold function [new C99].                                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wchar.h>

/* convert wide string to long double, with checking */
long double __cdecl (wcstold)(const wchar_t * restrict s, wchar_t ** restrict endptr)
{
    return __wcstold(s, endptr, 0);
}

