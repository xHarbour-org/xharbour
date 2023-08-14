/****************************************************************************
 *                                                                          *
 * File    : putwc.c                                                        *
 *                                                                          *
 * Purpose : putwc function.                                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xwstdio.h"

/* put character to wide stream */
wint_t __cdecl (putwc)(wchar_t c, FILE *str)
{
    return fputwc(c, str);
}

