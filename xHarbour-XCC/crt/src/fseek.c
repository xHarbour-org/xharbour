/****************************************************************************
 *                                                                          *
 * File    : fseek.c                                                        *
 *                                                                          *
 * Purpose : fseek function.                                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-02  Added _Lockfileatomic() and _Unlockfileatomic().     *
 *                                                                          *
 ****************************************************************************/

#include "xstdio.h"

/* set seek offset for stream */
int __cdecl (fseek)(FILE *str, long off, int smode)
{
    int ans;

    _Lockfileatomic(str);
    ans = __fsetpos(str, 0, off, smode);
    _Unlockfileatomic(str);

    return ans;
}

