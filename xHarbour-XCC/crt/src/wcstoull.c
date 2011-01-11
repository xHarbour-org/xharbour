/****************************************************************************
 *                                                                          *
 * File    : wcstoull.c                                                     *
 *                                                                          *
 * Purpose : wcstoull function [new C99].                                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xwchar.h"

/* convert wide string to unsigned long long, with checking */
unsigned long long __cdecl (wcstoull)(const wchar_t * restrict s, wchar_t ** restrict endptr, int base)
{
    return __wcstoull(s, endptr, base);
}

