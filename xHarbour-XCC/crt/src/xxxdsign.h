/****************************************************************************
 *                                                                          *
 * File    : xxxdsign.h                                                     *
 *                                                                          *
 * Purpose : Common __fpsign[FL] functionality [new C99].                   *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-09-05  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* test floating-point sign bit */
int __cdecl FNAME(fpsign)(FTYPE x)
{
    return FISNEG(x);
}

