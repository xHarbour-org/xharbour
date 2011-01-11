/****************************************************************************
 *                                                                          *
 * File    : tmpfile.c                                                      *
 *                                                                          *
 * Purpose : tmpfile function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-03  Added _Lockfileatomic() and _Unlockfileatomic().     *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>
#include <string.h>
#include "xstdio.h"

/* open a temporary file */
FILE * __cdecl (tmpfile)(void)
{
    char fn[L_tmpnam], *s;
    FILE *str;

    _Lockfileatomic(str);

    if ((str = fopen((const char *)tmpnam(fn), "wb+")) == 0)
        ;
    else if ((s = (char *)malloc(sizeof(fn) + 1)) == 0)
        fclose(str), str = 0;
    else
        str->tmpnam = strcpy(s, fn);

    _Unlockfileatomic(str);
    return str;
}

