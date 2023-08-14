/****************************************************************************
 *                                                                          *
 * File    : fesetround.c                                                   *
 *                                                                          *
 * Purpose : fesetround function -- win32 version.                          *
 *                                                                          *
 * History : Date      Reason                                               *
 *           00-09-15  Created                                              *
 *           01-09-03  Rewritten.                                           *
 *                                                                          *
 ****************************************************************************/

#include <fenv.h>

#define _FE_RND_MASK  3
#define _FE_RND_OFF   10

/* set rounding mode */
int __cdecl (fesetround)(int round)
{
    fexcept_t control;

    if ((round & _FE_RND_MASK) != round)
    {
        return 1;
    }
    else
    {
        /* valid mode, set it */
        __asm {
            lea eax,[control]
            fstcw word [eax]
        }

        control = control & ~(_FE_RND_MASK << _FE_RND_OFF) | (round << _FE_RND_OFF);

        __asm {
            lea eax,[control]
            fldcw word [eax]
        }
    }

    return 0;
}

