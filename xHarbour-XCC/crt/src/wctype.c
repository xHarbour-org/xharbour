/****************************************************************************
 *                                                                          *
 * File    : wctype.c                                                       *
 *                                                                          *
 * Purpose : wctype function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <string.h>
#include "xwctype.h"

/* find classification for wide character */
wctype_t __cdecl (wctype)(const char *name)
{
    wctype_t n;

    for (n = 1; __wctypetab[n].name != 0; ++n)
    {
        if (strcmp(__wctypetab[n].name, name) == 0)
            return n;
    }

    return 0;
}

