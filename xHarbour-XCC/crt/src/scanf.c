/****************************************************************************
 *                                                                          *
 * File    : scanf.c                                                        *
 *                                                                          *
 * Purpose : scanf function.                                                *
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

/* read formatted from stdin */
int __cdecl (scanf)(const char * restrict fmt, ...)
{
    int ans;
    va_list ap;

    va_start(ap, fmt);
    _Lockfileatomic(stdin);
    ans = __scanf(&scin, stdin, fmt, ap);
    _Unlockfileatomic(stdin);
    va_end(ap);

    return ans;
}

