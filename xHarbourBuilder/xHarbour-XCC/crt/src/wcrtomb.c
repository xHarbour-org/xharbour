/****************************************************************************
 *                                                                          *
 * File    : wcrtomb.c                                                      *
 *                                                                          *
 * Purpose : wcrtomb function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <limits.h>
#include "xwchar.h"

/* translate wchar_t to multibyte, restartably */
size_t __cdecl (wcrtomb)(char * restrict s, wchar_t wchar, mbstate_t * restrict pst)
{
    static mbstate_t mbst = {0};
    char buf[MB_LEN_MAX];

    if (pst == 0)
        pst = &mbst;

    return (s != 0) ? __wctomb(s, wchar, pst) : __wctomb(buf, L'0', pst);
}

