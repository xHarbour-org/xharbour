/****************************************************************************
 *                                                                          *
 * File    : mbrlen.c                                                       *
 *                                                                          *
 * Purpose : mbrlen function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xwchar.h"

/* determine next multibyte code, restartably */
size_t __cdecl (mbrlen)(const char * restrict s, size_t n, mbstate_t * restrict pst)
{
    static mbstate_t mbst = {0};

    return __mbtowc(0, s, n, pst ? pst : &mbst);
}

