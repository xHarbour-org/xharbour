/****************************************************************************
 *                                                                          *
 * File    : vscanf.c                                                       *
 *                                                                          *
 * Purpose : vscanf function [new C99].                                     *
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

/* read formatted from stdin to arg list */
int __cdecl (vscanf)(const char * restrict fmt, va_list ap)
{
    int ans;

    _Lockfileatomic(stdin);
    ans = __scanf(&scin, stdin, fmt, ap);
    _Unlockfileatomic(stdin);

    return ans;
}

