/****************************************************************************
 *                                                                          *
 * File    : fwide.c                                                        *
 *                                                                          *
 * Purpose : fwide function.                                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-02  Added _Lockfileatomic() and _Unlockfileatomic().     *
 *                                                                          *
 ****************************************************************************/

#include "xwstdio.h"

/* set/test wide/narrow file mode */
int __cdecl (fwide)(FILE *str, int mode)
{
    int ans;

    _Lockfileatomic(str);

    if (mode < 0 && !(str->mode & _MWIDE))
        str->mode |= _MBYTE;
    else if (mode > 0 && !(str->mode & _MBYTE))
        str->mode |= _MWIDE;

    ans = (str->mode & _MBYTE) ? -1 : (str->mode & _MWIDE) ? 1 : 0;

    _Unlockfileatomic(str);
    return ans;
}

