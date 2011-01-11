/****************************************************************************
 *                                                                          *
 * File    : msize.c                                                        *
 *                                                                          *
 * Purpose : _msize function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xalloc.h"
#include "xthread.h"

/* return size of an allocated data object */
#ifdef __MT__
static size_t _mt_msize(void *cp)
#else
size_t __cdecl (_msize)(void *cp)
#endif
{
    register Word *p = (Word *)cp;

    p -= HEADERWORDS;
    CHECKALLOCPTR(p);

    if (TAG(p) == FREE)
        return 0;

    return (SIZE(p) - ALLOC_OVERHEAD) * sizeof(Word);
}

#ifdef __MT__
/* return size of an allocated data object */
size_t __cdecl (_msize)(void *cp)
{
    size_t size;

    __mtlock(_HEAP_LOCK);
    __try
    {
        size = _mt_msize(cp);
    }
    __finally
    {
        __mtunlock(_HEAP_LOCK);
    }

    return size;
}
#endif /* __MT__ */

