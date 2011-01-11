/****************************************************************************
 *                                                                          *
 * File    : mblen.c                                                        *
 *                                                                          *
 * Purpose : mblen function.                                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>
#include "xwchar.h"

/* determine length of next multibyte code */
int __cdecl (mblen)(const char *s, size_t n)
{
    mbstate_t mbst = {0};
    int i;

    i = __mbtowc(0, s, n, &mbst);
    return (i < 0) ? -1 : i;
}

