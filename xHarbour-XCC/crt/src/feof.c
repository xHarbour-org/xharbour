/****************************************************************************
 *                                                                          *
 * File    : feof.c                                                         *
 *                                                                          *
 * Purpose : feof function.                                                 *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-02  Added _Lockfileatomic() and _Unlockfileatomic().     *
 *                                                                          *
 ****************************************************************************/

#include "xstdio.h"

/* test end-of-file indicator for a stream */
int __cdecl (feof)(FILE *str)
{
    int mode;

    _Lockfileatomic(str);
    mode = str->mode & _MEOF;
    _Unlockfileatomic(str);

    return mode;
}

