/****************************************************************************
 *                                                                          *
 * File    : wcstoll.c                                                      *
 *                                                                          *
 * Purpose : wcstoll function [new C99].                                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xwchar.h"

/* convert wide string to long long, with checking */
long long __cdecl (wcstoll)(const wchar_t * restrict s, wchar_t ** restrict endptr, int base)
{
    return __wcstoll(s, endptr, base);
}

