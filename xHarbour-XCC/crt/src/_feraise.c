/****************************************************************************
 *                                                                          *
 * File    : _feraise.c                                                     *
 *                                                                          *
 * Purpose : __feraise function [new C99].                                  *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-09-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include <errno.h>
#include <fenv.h>
#include <math.h>

/* report floating-point exception */
void __cdecl (__feraise)(int except)
{
    if ((math_errhandling & MATH_ERREXCEPT) != 0)
        feraiseexcept(except);

    if ((math_errhandling & MATH_ERRNO) == 0)
        ;
    else if ((except & (FE_DIVBYZERO|FE_INVALID)) != 0)
        errno = EDOM;
    else if ((except & (FE_UNDERFLOW|FE_OVERFLOW)) != 0)
        errno = ERANGE;
}

