/****************************************************************************
 *                                                                          *
 * File    : wcstof.c                                                       *
 *                                                                          *
 * Purpose : wcstof function [new C99].                                     *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wchar.h>

/* convert wide string to float, with checking */
float __cdecl (wcstof)(const wchar_t * restrict s, wchar_t ** restrict endptr)
{
    return __wcstof(s, endptr, 0);
}

