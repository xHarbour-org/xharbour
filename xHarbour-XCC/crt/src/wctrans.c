/****************************************************************************
 *                                                                          *
 * File    : wctrans.c                                                      *
 *                                                                          *
 * Purpose : wctrans function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <string.h>
#include "xwctype.h"

/* find transformation for wide character */
wctrans_t __cdecl (wctrans)(const char *name)
{
    wctrans_t n;

    for (n = 1; __wctranstab[n].name != 0; ++n)
    {
        if (strcmp(__wctranstab[n].name, name) == 0)
            return n;
    }

    return 0;
}

