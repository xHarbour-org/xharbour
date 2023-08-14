/****************************************************************************
 *                                                                          *
 * File    : mbrtowc.c                                                      *
 *                                                                          *
 * Purpose : mbrtowc function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xwchar.h"

/* translate multibyte to wchar_t, restartably */
size_t __cdecl (mbrtowc)(wchar_t * restrict pwc, const char * restrict s, size_t n, mbstate_t * restrict pst)
{
    static mbstate_t mbst = {0};

    if (pst == 0)
        pst = &mbst;

    return (s != 0) ? __mbtowc(pwc, s, n, pst) : __mbtowc(0, "", n, pst);
}

