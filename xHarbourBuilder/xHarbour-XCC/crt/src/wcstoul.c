/****************************************************************************
 *                                                                          *
 * File    : wcstoul.c                                                      *
 *                                                                          *
 * Purpose : wcstoul function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <wchar.h>

/* convert wide string to unsigned long */
unsigned long __cdecl (wcstoul)(const wchar_t * restrict s, wchar_t ** restrict endptr, int base)
{
    return __wcstoul(s, endptr, base);
}

