/****************************************************************************
 *                                                                          *
 * File    : fegetround.c                                                   *
 *                                                                          *
 * Purpose : fegetround function -- win32 version.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-03  Rewritten.                                           *
 *                                                                          *
 ****************************************************************************/

#include <fenv.h>

#define _FE_RND_MASK  3
#define _FE_RND_OFF   10

/* test rounding mode */
int __cdecl (fegetround)(void)
{
    fexcept_t control;

    __asm {
        lea eax,[control]
        fstcw word [eax]
    }

    return ((int)(control >> _FE_RND_OFF) & _FE_RND_MASK);
}

