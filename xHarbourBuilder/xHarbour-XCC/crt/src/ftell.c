/****************************************************************************
 *                                                                          *
 * File    : ftell.c                                                        *
 *                                                                          *
 * Purpose : ftell function.                                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-02  Added _Lockfileatomic() and _Unlockfileatomic().     *
 *                                                                          *
 ****************************************************************************/

#include "xstdio.h"

/* get seek offset for stream */
long __cdecl (ftell)(FILE *str)
{
    int ans;

    _Lockfileatomic(str);
    ans = __fgetpos(str, 0);
    _Unlockfileatomic(str);

    return ans;
}

