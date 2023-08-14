/****************************************************************************
 *                                                                          *
 * File    : wctomb.c                                                       *
 *                                                                          *
 * Purpose : wctomb function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>
#include "xwchar.h"

/* translate wide character to multibyte string */
int __cdecl (wctomb)(char *s, wchar_t wchar)
{
    static mbstate_t mbst = {0};

    return __wctomb(s, wchar, &mbst);
}

