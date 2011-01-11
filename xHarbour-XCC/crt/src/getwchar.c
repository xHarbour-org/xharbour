/****************************************************************************
 *                                                                          *
 * File    : getwchar.c                                                     *
 *                                                                          *
 * Purpose : getwchar function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xwstdio.h"

/* get a wchar_t from wide stdin */
wint_t __cdecl (getwchar)(void)
{
    return fgetwc(stdin);
}

