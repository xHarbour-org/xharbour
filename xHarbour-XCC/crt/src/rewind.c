/****************************************************************************
 *                                                                          *
 * File    : rewind.c                                                       *
 *                                                                          *
 * Purpose : rewind function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-02  Added _Lockfileatomic() and _Unlockfileatomic().     *
 *                                                                          *
 ****************************************************************************/

#include "xstdio.h"

/* rewind stream */
void __cdecl (rewind)(FILE *str)
{
    _Lockfileatomic(str);
    __fsetpos(str, 0, 0L, SEEK_SET);
    str->mode &= ~_MERR;
    _Unlockfileatomic(str);
}

