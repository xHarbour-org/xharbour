/****************************************************************************
 *                                                                          *
 * File    : putwchar.c                                                     *
 *                                                                          *
 * Purpose : putwchar function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xwstdio.h"

/* put character to wide stdout */
wint_t __cdecl (putwchar)(wchar_t c)
{
    return fputwc(c, stdout);
}

