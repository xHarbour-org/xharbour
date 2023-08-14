/****************************************************************************
 *                                                                          *
 * File    : wcstoumax.c                                                    *
 *                                                                          *
 * Purpose : wcstoumax function [new C99].                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wchar.h>
#include <inttypes.h>

/* convert wide string to uintmax_t, with checking */
uintmax_t __cdecl (wcstoumax)(const wchar_t * restrict s, wchar_t ** restrict endptr, int base)
{
    return (uintmax_t)wcstoull(s, endptr, base);
}

