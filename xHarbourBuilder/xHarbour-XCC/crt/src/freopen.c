/****************************************************************************
 *                                                                          *
 * File    : freopen.c                                                      *
 *                                                                          *
 * Purpose : freopen function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-02  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>
#include "xstdio.h"
#include "xthread.h"

/* reopen a file */
#ifdef __MT__
static FILE * _mt_freopen(const char * restrict name, const char * restrict mods, FILE * restrict str)
#else
FILE * __cdecl (freopen)(const char * restrict name, const char * restrict mods, FILE * restrict str)
#endif /* __MT__ */
{
    unsigned short mode;

    mode = str->mode & _MALFIL;
    str->mode &= ~_MALFIL;
    fclose(str);
    str->mode = mode;

    _Lockfileatomic(str);
    str = __fopen(name, mods, str, _FD_INVALID);
    _Unlockfileatomic(str);

    return str;
}

#ifdef __MT__
/* reopen a file */
FILE * __cdecl (freopen)(const char * restrict name, const char * restrict mods, FILE * restrict str)
{
    __mtlock(_STREAM_LOCK);
    __try
    {
        str = _mt_freopen(name, mods, str);
    }
    __finally
    {
        __mtunlock(_STREAM_LOCK);
    }

    return str;
}
#endif /* __MT__ */

