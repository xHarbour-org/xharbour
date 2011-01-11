/****************************************************************************
 *                                                                          *
 * File    : fopen.c                                                        *
 *                                                                          *
 * Purpose : fopen function.                                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-02  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>
#include "xstdio.h"
#include "xthread.h"

/* open a file, given name */
#ifdef __MT__
static FILE * _mt_fopen(const char * restrict name, const char * restrict mods)
#else
FILE * __cdecl (fopen)(const char * restrict name, const char * restrict mods)
#endif /* __MT__ */
{
    return __fopen(name, mods, __fslot(), _FD_INVALID);
}

#ifdef __MT__
/* open a file, given name */
FILE * __cdecl (fopen)(const char * restrict name, const char * restrict mods)
{
    FILE *str;

    __mtlock(_STREAM_LOCK);
    __try
    {
        str = _mt_fopen(name, mods);
    }
    __finally
    {
        __mtunlock(_STREAM_LOCK);
    }

    return str;
}
#endif /* __MT__ */

