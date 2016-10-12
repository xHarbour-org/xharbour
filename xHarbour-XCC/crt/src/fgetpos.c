/****************************************************************************
 *                                                                          *
 * File    : fgetpos.c                                                      *
 *                                                                          *
 * Purpose : fgetpos function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-02  Added _Lockfileatomic() and _Unlockfileatomic().     *
 *                                                                          *
 ****************************************************************************/

#include "xstdio.h"

/* get file position indicator for stream */
int __cdecl (fgetpos)(FILE * restrict str, fpos_t * restrict p)
{
    int ans;

    _Lockfileatomic(str);
    ans = __fgetpos(str, p);
    _Unlockfileatomic(str);

    return ans;
}

