/****************************************************************************
 *                                                                          *
 * File    : atexit.c                                                       *
 *                                                                          *
 * Purpose : atexit function.                                               *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-05  Rewritten for C99.                                   *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>
#include "xstdio.h"
#include "xthread.h"

/* external declarations */
extern void (__cdecl *_Atfuns[])(void);
extern size_t _Atcount;
extern size_t _Atcount0;

/* function to call at exit */
#ifdef __MT__
static int _mt_atexit(void (__cdecl *func)(void))
#else
int __cdecl (atexit)(void (__cdecl *func)(void))
#endif /* __MT__ */
{
    if (_Atcount <= _Atcount0)
    {
        /* both stacks are full */
        return -1;
    }
    else
    {
        /* enough room, stack function pointer */
        _Atfuns[--_Atcount] = func;
        return 0;
    }
}

#ifdef __MT__
/* function to call at exit */
int __cdecl (atexit)(void (__cdecl *func)(void))
{
    int ans;

    __mtlock(_EXIT_LOCK);
    __try
    {
        ans = _mt_atexit(func);
    }
    __finally
    {
        __mtunlock(_EXIT_LOCK);
    }

    return ans;
}
#endif /* __MT__ */

