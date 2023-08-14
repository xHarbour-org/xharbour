/****************************************************************************
 *                                                                          *
 * File    : mbtowc.c                                                       *
 *                                                                          *
 * Purpose : mbtowc function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>
#include "xwchar.h"

/* determine next multibyte code */
int __cdecl (mbtowc)(wchar_t * restrict pwc, const char * restrict s, size_t n)
{
    static mbstate_t mbst = {0};
    int i = __mbtowc(pwc, s, (n <= MB_CUR_MAX) ? n : MB_CUR_MAX, &mbst);

    return (i < 0) ? -1 : i;
}

