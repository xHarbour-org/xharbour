/****************************************************************************
 *                                                                          *
 * File    : ferror.c                                                       *
 *                                                                          *
 * Purpose : ferror function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-02  Added _Lockfileatomic() and _Unlockfileatomic().     *
 *                                                                          *
 ****************************************************************************/

#include "xstdio.h"

/* test error indicator for a stream */
int __cdecl (ferror)(FILE *str)
{
    int mode;

    _Lockfileatomic(str);
    mode = str->mode & _MERR;
    _Unlockfileatomic(str);

    return mode;
}

