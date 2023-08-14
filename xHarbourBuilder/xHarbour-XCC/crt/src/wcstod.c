/****************************************************************************
 *                                                                          *
 * File    : wcstod.c                                                       *
 *                                                                          *
 * Purpose : wcstod function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wchar.h>

/* convert wide string to double, with checking */
double __cdecl (wcstod)(const wchar_t * restrict s, wchar_t ** restrict endptr)
{
    return __wcstod(s, endptr, 0);
}

