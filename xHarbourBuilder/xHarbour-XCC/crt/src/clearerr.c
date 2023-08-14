/****************************************************************************
 *                                                                          *
 * File    : clearerr.c                                                     *
 *                                                                          *
 * Purpose : clearerr function.                                             *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-02  Added _Lockfileatomic() and _Unlockfileatomic().     *
 *                                                                          *
 ****************************************************************************/

#include "xstdio.h"

/* clear EOF and error indicators for a stream */
void __cdecl (clearerr)(FILE *str)
{
    _Lockfileatomic(str);
    if (str->mode & (_MOPENR|_MOPENW))
        str->mode &= ~(_MEOF|_MERR);
    _Unlockfileatomic(str);
}

