/****************************************************************************
 *                                                                          *
 * File    : xxtrunc.h                                                      *
 *                                                                          *
 * Purpose : Common trunc[fl] functionality.                                *
 *                                                                          *
 * History : Date      Reason                                               *
 *           01-08-30  Created                                              *
 *                                                                          *
 ****************************************************************************/

#include "xmath.h"

/* truncate x toward zero */
FTYPE __cdecl FFUN(trunc)(FTYPE x)
{
    FNAME(fpint)(&x, 0);
    return x;
}

