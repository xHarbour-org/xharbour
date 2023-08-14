/****************************************************************************
 *                                                                          *
 * File    : puts.c                                                         *
 *                                                                          *
 * Purpose : puts function.                                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-02  Added _Lockfileatomic() and _Unlockfileatomic().     *
 *                                                                          *
 ****************************************************************************/

#include "xstdio.h"

/* put string + newline to stdout */
int __cdecl (puts)(const char *s)
{
    int ans;

    _Lockfileatomic(stdout);
    ans = (fputs(s, stdout) < 0 || fputc('\n', stdout) < 0) ? EOF : 0;
    _Unlockfileatomic(stdout);

    return ans;
}

