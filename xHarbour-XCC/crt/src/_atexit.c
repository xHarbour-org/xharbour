/****************************************************************************
 *                                                                          *
 * File    : _atexit.c                                                      *
 *                                                                          *
 * Purpose : _Atexit function.                                              *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-09-11  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <stdlib.h>
#include "xcrt.h"

/* external declarations */
extern void (*_Atfuns[])(void);
extern size_t _Atcount;
extern size_t _Atcount0;

/* register function to call at exit */
void (_Atexit)(void (*func)(void))
{
    if (_Atcount <= _Atcount0)
        abort();  /* both stacks are full, give up */
    else
        _Atfuns[_Atcount0++] = func;  /* library wrapup stack */
}

