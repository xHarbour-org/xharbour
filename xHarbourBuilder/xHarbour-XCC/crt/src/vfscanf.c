/****************************************************************************
 *                                                                          *
 * File    : vfscanf.c                                                      *
 *                                                                          *
 * Purpose : vfscanf function [new C99].                                    *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-02  Added _Lockfileatomic() and _Unlockfileatomic().     *
 *                                                                          *
 ****************************************************************************/

#include "xstdio.h"

/* get or put a character */
static int scin(void *str, int ch, int getfl)
{
    return (getfl) ? fgetc((FILE *)str) : ungetc(ch, (FILE *)str);
}

/* read formatted from stream from arg list */
int __cdecl (vfscanf)(FILE * restrict str, const char * restrict fmt, va_list ap)
{
    int ans;

    _Lockfileatomic(str);
    ans = __scanf(&scin, str, fmt, ap);
    _Unlockfileatomic(str);

    return ans;
}

