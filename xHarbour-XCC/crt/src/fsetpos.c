/****************************************************************************
 *                                                                          *
 * File    : fsetpos.c                                                      *
 *                                                                          *
 * Purpose : fsetpos function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-02  Added _Lockfileatomic() and _Unlockfileatomic().     *
 *                                                                          *
 ****************************************************************************/

#include "xstdio.h"

/* set file position indicator for stream */
int __cdecl (fsetpos)(FILE *str, const fpos_t *p)
{
    int ans;

    _Lockfileatomic(str);
    ans = __fsetpos(str, p, 0L, SEEK_SET);
    _Unlockfileatomic(str);

    return ans;
}

